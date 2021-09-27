module Main where
import System.Environment
import CabalDocspec.Lexer
import System.FilePath
import System.FilePattern.Directory
import Distribution.ModuleName hiding (main)
import Control.Monad
import Data.List
import System.Directory
import Debug.Trace
import Data.Char (isSpace)

splitArgs = go []
  where
    go r ("--":xs) = (reverse r, xs)
    go r (x:xs) = go (x:r) xs
    go r [] = (reverse r, [])

findHsFiles dir = map (dir </>) <$> getDirectoryFiles dir ["**/*.hs"]

getModName f = do
  ls <- filter ("module" `isPrefixOf`) . lines <$> readFile f
  case map words ls of
    ((_:mn:_):_) -> pure (Just mn)
    _ -> pure Nothing


generateTests :: [Located String] -> String
generateTests = intercalate ",\n  " . concatMap mkTests
  where mkTests :: Located String -> [String]
        mkTests (L loc xs) = go loc (lines xs)

        go loc ((' ':'>':'>':'>':x):xs) =
          case dropWhile isSpace x of
            (':':'{':cs) -> readDocTestMultiline loc [cs] xs
            cs -> readDocTest loc [cs] xs
        go loc (x:xs) = go loc xs
        go _ [] = []


        readDocTestMultiline loc testCode ((':':'}':_):xs) = readDocTestResult loc (mkTestBodyMultiline testCode)  [] xs
        readDocTestMultiline loc testCode (x:xs) = readDocTestMultiline loc (x:testCode) xs
        readDocTestMultiline loc testCode xs = [] -- unterminated multiline

        readDocTest loc testCode ((' ':'>':'>':'>':x):xs) = readDocTest loc (dropWhile isSpace x:testCode) xs
        readDocTest loc testCode xs = readDocTestResult loc (mkTestBody testCode)  [] xs

        readDocTestResult loc testBody resultString ((' ':'>':'>':'>':x):xs) = mkTest loc testBody resultString  : readDocTest loc [x] xs
        readDocTestResult loc testBody resultString (x : xs) = readDocTestResult loc testBody (dropWhile isSpace x:resultString) xs
        readDocTestResult loc testBody resultString [] = mkTest loc testBody resultString : []


        mkTestBodyMultiline testCode = intercalate "\n" . map (' ':) . reverse $ testCode

        mkTestBody testCode = stripNewlines (intercalate "\n" (reverse $ map fixLet testCode))

        mkTest loc testBody resultString  = "Test.Tasty.HUnit.testCase \"" ++ show loc ++ "\" $ show (" ++ testBody ++ ") Test.Tasty.HUnit.@?= " ++ show (intercalate "\n" . filter (not . (all isSpace)) . map stripNewlines $ reverse resultString)

        stripNewlines = map (\x -> if x == '\n' then ' ' else x)
        fixLet l = if "let" `isPrefixOf` l then l ++ " in " else l


generateFile tgt src = getModName src >>= \mmn ->
  case mmn of
     Just modName -> do
        m <- parseModule (fromString modName) src
        let contents = "module GeneratedTests." ++ modName ++ " where \n" ++ "import " ++ modName ++ "\n" ++ "import qualified Test.Tasty\nimport qualified Test.Tasty.HUnit\n" ++
                     maybe "" unLoc (moduleSetup m) ++ "\n" ++ "properties = Test.Tasty.testGroup " ++ show modName ++ "\n [" ++ generateTests (moduleContent m) ++ "]"
        let outFile = tgt </> "GeneratedTests" </> (toFilePath (moduleName m) ++ ".hs")
        createDirectoryIfMissing True (dropFileName outFile)
        writeFile outFile contents
        pure ["GeneratedTests."++modName]
     Nothing -> pure []


main :: IO ()
main = do
  (tgt:rest) <- getArgs
  let (srcdirs, ghcArgs) = splitArgs rest
  -- TODO cleverly expand srcdirs, can't pass in directly cpp processed modules from other components, gotta guess, sigh.
  files <- concat <$> forM srcdirs findHsFiles -- find hs files in srcdirs
  resultModules <- concat <$> forM files (generateFile tgt)
  let mkImports = concatMap (\x -> "import " ++ x ++ "\n") resultModules
      mkPropertiesList = intercalate ", " $ map (++ ".properties") resultModules
  writeFile (tgt </> "Main.hs") $ "module Main where \nimport Test.Tasty\n"++ mkImports ++ "main = defaultMain $ testGroup \"DocTests\" [" ++ mkPropertiesList ++ "]"
  -- TODO writeMainModule resultModules --   writeFile (tgt </> "Extra.hs") "module Extra where"
  mapM_ putStrLn resultModules
  pure ()
