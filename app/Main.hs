module Main where

import System.Environment (getArgs)

import Interpolation (interpolate, Object)
import Grammar (parseGrammar)
import Syntax (plib, grammar, Language, ilib)
import Parser (Parser, PMaybe(PErr, PJust))

import System.Exit

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2 then
        putStrLn "Usage: mocca <mocca library> <src>"
    else
        processFile (head args) (head $ tail args)

parseOrFail :: String -> Parser a -> String -> IO a
parseOrFail msg p s = case p s of
    PErr stack -> putStrLn (msg ++ ": " ++ show stack) >> exitFailure
    PJust (x, "") -> return x
    PJust (_, t) -> putStrLn (msg ++ ": Trailing characters: " ++ show t) >> exitFailure

processFile :: FilePath -> FilePath -> IO ()
processFile libf srcf = do
    libText <- readFile libf
    srcText <- readFile srcf
    lib <- parseOrFail "Unable to parse library." plib libText
    tree <- parseOrFail "Unable to parse source file." (parseGrammar (grammar lib)) srcText
    putStr $ interpolate (ilib lib) tree
