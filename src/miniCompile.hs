module Main where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS 
import Data.List
import Data.Maybe
import System.Environment

import Mini.Asm.Types
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
        let fileName = head $ filter (not . isPrefixOf "--") args
        file <- readFile fileName
        let parsedJSON = decode . BS.pack $ file :: Maybe Program
            program = fromMaybe (error "Invalid JSON input") parsedJSON
        when (testJSON `elem` args) $ 
            putStrLn $ BS.unpack $ encode parsedJSON
        when (printProg `elem` args) $ print program
        let env = checkTypes program
        when (printEnv `elem` args) $ print env
        if not $ shouldPrint args
            then return ()
            else do 
                    envReport env
                    let graphs = fmap (`createGraphs` program) env
                    if (dumpIL `elem` args) 
                       then writeIloc graphs $ fileNameToIL fileName
                       else writeAsm graphs $ fileNameToS fileName 

shouldPrint :: [String] -> Bool
shouldPrint = not . any (\x -> x `elem` [testJSON, printProg, printEnv])

envReport :: Either ErrType GlobalEnv -> IO ()
envReport (Left msg) = error msg
envReport _ = return ()

stripFile :: String -> String
stripFile file = reverse $ take localNdx $ reverse newName
    where reverseNdx = 1 + fromMaybe (-1) ('.' `elemIndex` reverse file)
          localNdx = fromMaybe 0 $ '/' `elemIndex` reverse newName
          newName = reverse $ drop reverseNdx $ reverse file

fileNameToIL :: String -> String
fileNameToIL oldFile = stripFile oldFile ++ ".il"

writeIloc :: Either ErrType [NodeGraph] -> String -> IO ()
writeIloc (Left msg) _ = error msg
writeIloc (Right graphs) fileName = do
        let print = foldl' (\msg ng -> msg ++ "\n" ++ showNodeGraph ng) "" graphs
        writeFile fileName print

fileNameToS :: String -> String
fileNameToS oldFile = stripFile oldFile ++ ".s"

writeAsm :: Either ErrType [NodeGraph] -> String -> IO ()
writeAsm (Left msg) _ = error msg
writeAsm (Right graphs) fileName = do
        let print = foldl' (\msg insn -> msg ++ show insn ++ "\n") ""
                        $ programToAsm graphs
        writeFile fileName print  
