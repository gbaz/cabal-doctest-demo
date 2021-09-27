--
-- This module is derivative of cabal-docspec
--
-- Copyright (c) 2019 Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- This library is free software: you may copy, redistribute and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation, either version 3 of the License, or (at your
-- option) any later version.

-- This library is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- <https://www.gnu.org/licenses/gpl-3.0.html>.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

module CabalDocspec.Lexer where

import Data.Char
import Data.List              (init, break)
import Language.Haskell.Lexer (Pos, PosToken, line, column)
import Text.Read              (read)
import Control.DeepSeq
import Control.Monad
import Control.Applicative
import Control.Arrow
import Data.Maybe

import qualified Distribution.ModuleName as C
import qualified Language.Haskell.Lexer  as L
import qualified Text.Parsec             as P


-- | Documentation for a module grouped together with the modules name.
data Module a = Module
    { moduleName    :: C.ModuleName
    , moduleSetup   :: Maybe a
    , moduleContent :: [a]
    }
  deriving (Eq, Show, Functor, Foldable, Traversable)

type Located = GenLocated Pos

data GenLocated l e = L l e
  deriving (Eq, Show, Functor, Foldable, Traversable)

unLoc :: GenLocated l e -> e
unLoc (L _  e) = e

prettyPos :: Pos -> String
prettyPos pos = "in comment at " ++ show (line pos) ++ ":" ++ show (column pos)

-------------------------------------------------------------------------------
-- Pass
-------------------------------------------------------------------------------

-- Stubborn pass filters out error tokens (which start with
-- and continues with the rest
--
-- This works around that @haskell-lexer@ doesn't recognise prefix quotes.
--
stubbornPass0 :: String -> [PosToken]
stubbornPass0 = stubborn . L.lexerPass0 where
    stubborn :: [PosToken] -> [PosToken]
    stubborn [] = []
    stubborn [(L.ErrorToken, (pos, '\'' : s)), (L.TheRest, (_, s'))] =
        map (second (first (addPos pos))) $ stubbornPass0 (s ++ s')

    stubborn (t : ts) = t : stubborn ts

    -- only line...
    addPos pos1 pos2 = pos2
        { L.line   = L.line pos1 + L.line pos2 - 1
        }




-------------------------------------------------------------------------------
-- Comment
-------------------------------------------------------------------------------

data Comment
    = LineCommentBlock Pos String
    | NestedComment Pos String
  deriving Show

instance NFData Comment where
    rnf (LineCommentBlock pos s) = rnfPos pos `seq` rnf s
    rnf (NestedComment pos s)    = rnfPos pos `seq` rnf s

rnfPos :: Pos -> ()
rnfPos (L.Pos x y z) = rnf x `seq` rnf y `seq` rnf z

extractComments :: [PosToken] -> [Comment]
extractComments = go 0 where
    go :: Int -> [PosToken] -> [Comment]
    go _diff [] = []
    -- normal comments, -- foo
    go diff ((L.Commentstart, (pos, s)) : (L.Comment, (_pos, str)) : next) =
        goLines diff (adjustPos diff pos) (\n -> s ++ str ++ n) next
    -- nested comments, {- foo -}
    go diff ((L.NestedComment, (pos, s)) : next) = case parseLinePragma s of
        Just l  -> go (l - L.line pos - 1) next
        Nothing -> NestedComment (adjustPos diff pos) s : go diff next
    -- other things are skipped
    go diff (_ : next) = go diff next

    goLines :: Int -> Pos -> (String -> String) -> [PosToken] -> [Comment]
    goLines diff !pos !acc ((L.Commentstart, (_, s)) : (L.Comment, (_, str)) : next) =
        goLines diff pos (acc . (s ++) . (str ++)) next
    -- there could be whitespace between line comments, but not empty lines.
    goLines diff !pos !acc ((L.Whitespace, (_,s)) : next)
        | all (/= '\n') s
        = goLines diff pos acc next
    goLines diff !pos !acc next = LineCommentBlock pos (acc "") : go diff next

    -- adjust position based on {-# LINE #-} pragmas
    adjustPos diff pos = pos { L.line = L.line pos + diff }

-------------------------------------------------------------------------------
-- Process expressions
-------------------------------------------------------------------------------

dropComments :: String -> String
dropComments
    = init
    . concat
    . map wsComment
    . L.lexerPass0
    . (++ "\n") -- work around https://github.com/yav/haskell-lexer/issues/9
  where
    wsComment :: PosToken -> String
    wsComment (L.Commentstart,  (_, s)) = map ws s
    wsComment (L.NestedComment, (_, s)) = map ws s
    wsComment (L.Comment,       (_, s)) = map ws s
    wsComment (_,               (_, s)) =        s

    ws c = if isSpace c then c else ' '

-------------------------------------------------------------------------------
-- Extract docstrings
-------------------------------------------------------------------------------

extractDocstrings :: C.ModuleName -> [Comment] -> Module (Located String)
extractDocstrings modname comments = Module
    { moduleName    = modname
    , moduleSetup   = listToMaybe
        [ c
        | NamedComment "setup" c <- comments'
        ]
    , moduleContent = mapMaybe nonSetup comments'
    }
  where
    nonSetup (HdkComment c) = Just c
    nonSetup (NamedComment "setup" _) = Nothing
    nonSetup (NamedComment _ c) = Just c

    comments' = mapMaybe classifyComment comments

data HdkComment
    = NamedComment String (Located String)
    | HdkComment (Located String)
  deriving (Show)

-- drop leading docstring character when classifying
classifyComment :: Comment -> Maybe HdkComment
classifyComment c = case s of
    '|' : ss -> Just (HdkComment (L pos ss))
    '^' : ss -> Just (HdkComment (L pos ss))
    '*' : ss -> Just (HdkComment (L pos ss)) -- this is an over approximation, for exports
    -- https://gitlab.haskell.org/ghc/ghc/-/blob/bd877edd9499a351db947cd51ed583872b2facdf/compiler/GHC/Parser/Lexer.x#L1404-1407
    '$' : z -> let (name, rest) = break isSpace z in Just (NamedComment name (L pos rest))
    _       -> Nothing
  where
    s = commentString c
    pos = commentPos c

commentString :: Comment -> String
commentString (NestedComment _ ('{' : '-' : rest)) = dropWhile isSpace $ dropTrailingClose rest
commentString (NestedComment _ s)                  = dropWhile isSpace s
commentString (LineCommentBlock _ s)               = dropWhile isSpace $ unlines $ map (dropWhile (== '-')) $ lines s

commentPos :: Comment -> Pos
commentPos (NestedComment p _)    = p
commentPos (LineCommentBlock p _) = p

dropTrailingClose :: String -> String
dropTrailingClose "-}"   = ""
dropTrailingClose (c:cs) = c : dropTrailingClose cs
dropTrailingClose []     = []


-------------------------------------------------------------------------------
-- Pragma parsers
-------------------------------------------------------------------------------

parseLinePragma :: String -> Maybe Int
parseLinePragma input =
    case P.parse linePragmaP "<input>" input of
        Right res -> Just res
        Left _    -> Nothing
  where
    linePragmaP :: P.Parsec String () Int
    linePragmaP = do
        _ <- P.string "{-#"
        skipSpacesP
        token <- tokenP
        unless (map toLower token == "line") $ fail $ "unexpected " ++ token
        skipSpacesP
        read <$> some (P.satisfy isDigit)

parseLanguagePragma :: String -> [String]
parseLanguagePragma input =
    case P.parse languagePragmaP "<input>" input of
        Right res -> res
        Left _    -> []
  where
    languagePragmaP :: P.Parsec String () [String]
    languagePragmaP = do
        _ <- P.string "{-#"
        skipSpacesP
        token <- tokenP
        unless (map toLower token == "language") $ fail $ "unexpected " ++ token
        skipSpacesP

        P.sepBy1 (tokenP <* skipSpacesP) (P.char ',' *> skipSpacesP)

skipSpacesP :: P.Parsec String () ()
skipSpacesP = P.skipMany P.space

tokenP :: P.Parsec String () String
tokenP = some $ P.satisfy isAlphaNum

----

parseModule modName modPath = do
  contents <- stubbornPass0  <$> readFile modPath
  let comments = extractComments contents
  let docs = extractDocstrings modName comments
  return docs