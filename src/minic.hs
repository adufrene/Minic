module Main where

import Control.Monad
import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS 
import Data.List
import Data.Maybe
import qualified Data.HashMap.Strict as HM
import System.Environment
import System.FilePath

import Mini.Asm.Types
import Mini.CFG
import Mini.Parser
import Mini.Iloc.Types
import Mini.Optimize
import Mini.Types
import Mini.TypeCheck
import Mini.RegAlloc
import Mini.CopyProp

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

checkColors :: String
checkColors = "--checkColors"

noOpt :: String
noOpt = "--noOpt"

copyProp :: String
copyProp = "--copyProp"

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
               let graphs = doCopyProp (globalEnv `createGraphs` program) shouldCopyProp
                   optFun = if noOpt `elem` args then id else removeUselessCode
                   optimized = optimize optFun <$> graphs
                   shouldCopyProp = copyProp `elem` args
               if printGraphs `elem` args
               then print optimized
               else if testAlloc `elem` args
               then print $ fmap testIntGraph optimized
               else if checkColors `elem` args
               then print $ fmap getRegLookup optimized
               else if dumpIL `elem` args
               then writeIloc optimized $ fileNameToIL fileName
               else writeAsm (noAlloc `notElem` args) optimized 
                     (getDeclarations program) $ fileNameToS fileName

shouldPrint :: [String] -> Bool
shouldPrint = not . any (\x -> x `elem` [testJSON, printProg, printEnv])

envReport :: Either ErrType GlobalEnv -> IO GlobalEnv
envReport (Left msg) = error msg
envReport (Right env) = return env

fileNameToIL :: String -> String
fileNameToIL oldFile = takeBaseName oldFile ++ ".il"

writeIloc :: [NodeGraph] -> String -> IO ()
writeIloc graphs fileName = do
        let print = foldl' (\msg ng -> msg ++ "\n" ++ showNodeGraph ng) "" graphs
        writeFile fileName print

fileNameToS :: String -> String
fileNameToS oldFile = takeBaseName oldFile ++ ".s"

writeAsm :: Bool -> [NodeGraph] -> [Declaration] -> String -> IO ()
writeAsm shouldAlloc graphs decls fileName = writeFile fileName print 
    where regHashes = fmap getRegLookup graphs
          funAsms = (if shouldAlloc
                        then colorProgramToAsm regHashes
                        else programToAsm) graphs decls
          print = foldl' (\msg insn -> msg ++ show insn ++ "\n") 
                    "" funAsms

mapNode :: ([Iloc] -> [Iloc]) -> Node -> Node
mapNode f (Node l il) = Node l (f il)

optimize :: ([Iloc] -> [Iloc]) -> NodeGraph -> NodeGraph
optimize f (g, hash) = (g, HM.map (mapNode f) hash)

safeGetFile :: [String] -> String
safeGetFile args
    | null fileList = error "Please provide mini file as argument"
    | otherwise = head fileList
    where fileList = filter (not . isPrefixOf "--") args
