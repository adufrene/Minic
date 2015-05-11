module Main where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS 
import Data.List
import Data.Maybe
import System.Environment

import Mini.Asm.Types
import Mini.CFG
import Mini.Parser
import Mini.Iloc.Types
import Mini.Types
import Mini.TypeCheck
import Mini.RegAlloc

testJSON :: String
testJSON = "--testJSON"

printProg :: String
printProg = "--printProgram"

printEnv :: String
printEnv = "--printEnv"

dumpIL :: String
dumpIL = "--dumpIL"

-- if "testJSON" is passed as a command line arg, re-encodes back to JSON then dumps that JSON

main :: IO ()
main = do
        args <- getArgs
        let fileName = head $ filter (not . isPrefixOf "--") args
        file <- readFile fileName
--         let parsedJSON = decode . BS.pack $ file :: Maybe Program
--             program = fromMaybe (error "Invalid JSON input") parsedJSON
        let program = parse file
        when (testJSON `elem` args) $ 
            putStrLn $ BS.unpack $ encode program
        when (printProg `elem` args) $ print program
        let env = checkTypes program
        when (printEnv `elem` args) $ print env
        when (shouldPrint args) $
            do 
               envReport env
               if dumpIL `elem` args
                  then writeIloc (fmap (`createGraphs` program) env)
                           $ fileNameToIL fileName
                  else writeAsm env program $ fileNameToS fileName 

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

writeAsm :: Either ErrType GlobalEnv -> Program -> String -> IO ()
writeAsm (Left msg) _ _= error msg
writeAsm (Right env) prog fileName = do
        let print = foldl' (\msg insn -> msg ++ show insn ++ "\n") ""
                        $ programToAsm env prog
        writeFile fileName print  
