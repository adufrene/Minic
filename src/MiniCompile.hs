module Main where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Environment

import Mini.CFG
import Mini.Iloc
import Mini.Types
import Mini.TypeCheck

-- if "testParse" is passed as a command line arg, re-encodes back to JSON then dumps that JSON

main :: IO ()
main = do
        args <- getArgs
        file <- readFile $ head args
        let parsedJSON = decode . BS.pack $ file :: Maybe Program
        when ("--testJSON" `elem` args) $ 
            putStrLn $ BS.unpack $ encode parsedJSON
        when ("--printProgram" `elem` args) $ print parsedJSON
        let env = fmap checkTypes parsedJSON
        when ("--printEnv" `elem` args) $ print env
        when (length args < 2) $ envReport env
        let graphs = fmap createGraphs parsedJSON
        return ()

envReport :: Maybe (Either ErrType GlobalEnv) -> IO ()
envReport Nothing = error "Bad input"
envReport (Just (Left msg)) = error msg
envReport _ = putStrLn "Successful Compilation"
