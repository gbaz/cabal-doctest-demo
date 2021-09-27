module Main where
import System.Environment
import System.FilePath
import System.Directory
import Debug.Trace
import Data.Char (isSpace)
import Data.List (isPrefixOf)

splitArgs = go []
  where
    go r ("--":xs) = (reverse r, xs)
    go r (x:xs) = go (x:r) xs
    go r [] = (reverse r, [])

main :: IO ()
main = do
  (tgt:rest) <- getArgs
  let (srcdirs, ghcArgs) = splitArgs rest
  let args = srcdirs ++ cleanupGhcArgs ghcArgs
  createDirectoryIfMissing True tgt
  writeFile (tgt </> "Main.hs") $ "module Main where \nimport Test.DocTest \nmain = doctest " ++ show args


cleanupGhcArgs [] = []
cleanupGhcArgs ("-package-db":x:xs) = ("-package-db " ++ x) : cleanupGhcArgs xs
cleanupGhcArgs ("-package-id":x:xs) = "-package-id" :    x  : cleanupGhcArgs xs
cleanupGhcArgs (x:xs) = if passArg x then x : cleanupGhcArgs xs else cleanupGhcArgs xs
  where
    passArg a = ("-optP" `isPrefixOf` a) || a `elem` ["-hide-all-packages", "-no-user-package-db"]
