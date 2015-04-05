module Main where

import Mini.Types
import Data.Aeson
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as BS

main :: IO ()
main = do
        args <- getArgs
        file <- readFile $ head args
        let parsedJSON = (decode . BS.pack $ file :: Maybe Program)
        putStrLn $ show parsedJSON
        -- re-encode the parsed JSON for testing
        putStrLn $ read $ BS.unpack $ encode $ BS.unpack (encode parsedJSON)
        return ()
