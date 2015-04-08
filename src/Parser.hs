module Main where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Mini.Types
import Mini.TypeCheck
import System.Environment

-- if "testParse" is passed as a command line arg, re-encodes back to JSON then dumps that JSON

main :: IO ()
main = do
        args <- getArgs
        file <- readFile $ head args
        let parsedJSON = decode . BS.pack $ file :: Maybe Program
        when ("--printJSON" `elem` args) $ 
            putStrLn $ BS.unpack $ encode $ BS.unpack (encode parsedJSON)
        when ("--printProgram" `elem` args) $ print parsedJSON
        let env = fmap checkTypes parsedJSON 
        when ("--printEnv" `elem` args) $ print env
        putStrLn "Compilation finished"
