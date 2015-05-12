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

printGraph :: String
printGraph = "--printGraph"

dumpIL :: String
dumpIL = "--dumpIL"

-- if "testJSON" is passed as a command line arg, re-encodes back to JSON then dumps that JSON

main :: IO ()
main = do
        args <- getArgs
        let fileName = safeGetFile args
        file <- readFile fileName
        let program = parse file
        when (testJSON `elem` args) $ 
            putStrLn $ BS.unpack $ encode program
        when (printProg `elem` args) $ print program
        let env = checkTypes program
        when (printEnv `elem` args) $ print env
        when (printGraph `elem` args) $ print $ fmap testIntGraph $ (envReport env) `createGraphs` program
        when (shouldPrint args) $
            do 
               let globalEnv = envReport env
               if dumpIL `elem` args
                  then writeIloc (globalEnv `createGraphs` program) 
                           $ fileNameToIL fileName
                  else writeAsm globalEnv program $ fileNameToS fileName 

shouldPrint :: [String] -> Bool
shouldPrint = not . any (\x -> x `elem` [testJSON, printProg, printEnv, printGraph])

envReport :: Either ErrType GlobalEnv -> GlobalEnv
envReport (Left msg) = error msg
envReport (Right env) = env

stripFile :: String -> String
stripFile file = reverse $ take localNdx $ reverse newName
    where reverseNdx = 1 + fromMaybe (-1) ('.' `elemIndex` reverse file)
          localNdx = fromMaybe 0 $ '/' `elemIndex` reverse newName
          newName = reverse $ drop reverseNdx $ reverse file

fileNameToIL :: String -> String
fileNameToIL oldFile = stripFile oldFile ++ ".il"

writeIloc :: [NodeGraph] -> String -> IO ()
writeIloc graphs fileName = do
        let print = foldl' (\msg ng -> msg ++ "\n" ++ showNodeGraph ng) "" graphs
        writeFile fileName print

fileNameToS :: String -> String
fileNameToS oldFile = stripFile oldFile ++ ".s"

writeAsm :: GlobalEnv -> Program -> String -> IO ()
writeAsm env prog fileName = do
        let print = foldl' (\msg insn -> msg ++ show insn ++ "\n") ""
                        $ programToAsm env prog
        writeFile fileName print  

safeGetFile :: [String] -> String
safeGetFile args = if null fileList
                       then error "Please provide mini file as argument"
                        else head fileList
    where fileList = filter (not . isPrefixOf "--") args
