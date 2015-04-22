module Main where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe
import System.Environment

import Mini.CFG
import Mini.Iloc.Types
import Mini.Types
import Mini.TypeCheck

-- if "testParse" is passed as a command line arg, re-encodes back to JSON then dumps that JSON

main :: IO ()
main = do
        args <- getArgs
        file <- readFile $ head args
        let parsedJSON = decode . BS.pack $ file :: Maybe Program
            program = fromMaybe (error "Invalid JSON input") parsedJSON
        when ("--testJSON" `elem` args) $ 
            putStrLn $ BS.unpack $ encode parsedJSON
        when ("--printProgram" `elem` args) $ print program
        let env = checkTypes program
        when ("--printEnv" `elem` args) $ print env
        when (length args < 2) $ envReport env
        let graphs = fmap (`createGraphs` program) env
        return ()

envReport :: Either ErrType GlobalEnv -> IO ()
envReport (Left msg) = error msg
envReport _ = return ()
