module Main where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS 
import Data.List (foldl', isPrefixOf)
import Data.Maybe
import System.Environment

import Mini.CFG
import Mini.Iloc.Types
import Mini.Types
import Mini.TypeCheck

testJSON :: String
testJSON = "--testJSON"

printProg :: String
printProg = "--printProgram"

printEnv :: String
printEnv = "--printEnv"

dumpIL :: String
dumpIL = "--dumpIL"

-- if "testParse" is passed as a command line arg, re-encodes back to JSON then dumps that JSON

main :: IO ()
main = do
        args <- getArgs
        file <- readFile $ head $ filter (not . (isPrefixOf "--")) args
        let parsedJSON = decode . BS.pack $ file :: Maybe Program
            program = fromMaybe (error "Invalid JSON input") parsedJSON
        when (testJSON `elem` args) $ 
            putStrLn $ BS.unpack $ encode parsedJSON
        when (printProg `elem` args) $ print program
        let env = checkTypes program
        when (printEnv `elem` args) $ print env
        when (not $ shouldPrint args) $ envReport env
        let graphs = fmap (`createGraphs` program) env
        when (dumpIL `elem` args) $ print graphs
--         return ()


shouldPrint :: [String] -> Bool
shouldPrint = any (\x -> x `elem` [testJSON, printProg, printEnv])


envReport :: Either ErrType GlobalEnv -> IO ()
envReport (Left msg) = error msg
envReport _ = return ()
