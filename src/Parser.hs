module Main where

import Mini.Types
import Data.Aeson
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as BS

main :: IO ()
main = do
        args <- getArgs
        file <- readFile $ head args
        putStrLn $ show (decode . BS.pack $ file :: Maybe Program)
        return ()
