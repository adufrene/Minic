module Main where

import Mini.Types
import Data.Aeson
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as BS

-- if "testParse" is passed as a command line arg, re-encodes back to JSON then dumps that JSON

main :: IO ()
main = do
        args <- getArgs
        file <- readFile $ head args
        let parsedJSON = (decode . BS.pack $ file :: Maybe Program)
        if "testParse" `elem` args then
           putStrLn $ read $ BS.unpack $ encode $ BS.unpack (encode parsedJSON)
        else
           putStrLn $ show parsedJSON
        return ()
