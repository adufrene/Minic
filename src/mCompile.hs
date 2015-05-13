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

printGraphs :: String
printGraphs = "--printGraphs"

testAlloc :: String
testAlloc = "--testAlloc"

dumpIL :: String
dumpIL = "--dumpIL"

noAlloc :: String
noAlloc = "--noAlloc"

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
        when (shouldPrint args) $
            do 
               globalEnv <- envReport env
               let graphs = globalEnv `createGraphs` program
               if printGraphs `elem` args
               then print graphs
               else if testAlloc `elem` args
               then print $ fmap testIntGraph graphs
               else if dumpIL `elem` args
               then writeIloc graphs $ fileNameToIL fileName
               else writeAsm (noAlloc `notElem` args) graphs 
                     (getDeclarations program) $ fileNameToS fileName 

shouldPrint :: [String] -> Bool
shouldPrint = not . any (\x -> x `elem` [testJSON, printProg, printEnv])

envReport :: Either ErrType GlobalEnv -> IO GlobalEnv
envReport (Left msg) = error msg
envReport (Right env) = return env

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

writeAsm :: Bool -> [NodeGraph] -> [Declaration] -> String -> IO ()
writeAsm shouldAlloc graphs decls fileName = writeFile fileName print 
    where regHashes = fmap getRegLookup graphs
          funAsms = (if shouldAlloc
                        then colorProgramToAsm regHashes
                        else programToAsm) graphs decls
          print = foldl' (\msg insn -> msg ++ show insn ++ "\n") 
                    "" funAsms
          
        

safeGetFile :: [String] -> String
safeGetFile args = if null fileList
                       then error "Please provide mini file as argument"
                        else head fileList
    where fileList = filter (not . isPrefixOf "--") args
