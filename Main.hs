module Main where

import TeX
import Ascii
import System.Environment
import Control.Monad (forM_)
import RodinTheory (Theory)
import RodinTheory.Read
import RodinTheory.TeX
import RodinTheory.Ascii
import RodinContext (Context)
import RodinContext.Read
import RodinContext.TeX
import RodinContext.Ascii
import RodinMachine (Machine)
import RodinMachine.Read
import RodinMachine.TeX
import RodinMachine.Ascii
import Data.List (lookup)
import Data.List.Split (splitOn)

class Parsable a where
  toParsed :: a -> Parsed

instance Parsable Context where
  toParsed = PContext

instance Parsable Machine where
  toParsed = PMachine

instance Parsable Theory where
  toParsed = PTheory

data Parsed =
      PContext Context
    | PMachine Machine
    | PTheory  Theory

instance ShowTeX Parsed where
  showTeX (PContext ct) = showTeX ct
  showTeX (PMachine ma) = showTeX ma
  showTeX (PTheory  th) = showTeX th

instance ShowAscii Parsed where
  showAscii (PContext ct) = showAscii ct
  showAscii (PMachine ma) = showAscii ma
  showAscii (PTheory  th) = showAscii th

mkParser :: Parsable a => (String -> IO a) -> (String -> IO Parsed)
mkParser parser =
    \x -> parser x >>= (return . toParsed)

data ReadConfig = ReadConfig {
    readerName :: String,
    processor  :: String -> IO Parsed
}

readConfTable :: [(String,ReadConfig)]
readConfTable =
    [ ("buc",ReadConfig "context" $ mkParser parseContextFile)
    , ("bum",ReadConfig "machine" $ mkParser parseMachineFile)
    , ("tuf",ReadConfig "theory"  $ mkParser parseTheoryFile ) ]

data WriteConfig = WriteConfig {
    writerName :: String,
    writerExtension :: String,
    stringify :: Parsed -> String
}

writeConfTable :: [(String,WriteConfig)]
writeConfTable = 
    [ (  "tex",WriteConfig "TeX" "tex" showTeX)
    , ("ascii",WriteConfig "ASCII" "ascii" showAscii) ]

defaultWriteConfig :: WriteConfig
defaultWriteConfig = WriteConfig "TeX" "tex" showTeX

main :: IO ()
main = getArgs >>= readopt >>= \x ->
    case x of
      Nothing -> return ()
      Just (wc,rcs) -> forM_ rcs (\(fn,rc) -> process fn rc wc)

printHelp :: IO ()
printHelp = do
    putStrLn "Rodin file transformer"
    putStrLn ""
    putStrLn "Syntax:"
    putStrLn "    program [-tex|-ascii] <file1> [<file2> [...]]"
    putStrLn ""
    putStrLn "Use -tex or -ascii to transform the given files in .tex or in .ascii (plain text)."
    putStrLn "Give the list of files; they will be combined in one big file. The program will"
    putStrLn "automatically determine the correct way to read it *based on its extension."
    putStrLn ""
    putStrLn "Supported extensions:"
    putStrLn "  Context description files (.buc)"
    putStrLn "  Machine description files (.bum)"
    putStrLn "  Theory description files (.tuf)"
    putStrLn ""

readopt :: [String] -> IO (Maybe (WriteConfig,[(String,ReadConfig)]))
readopt [] = return $ Just (defaultWriteConfig,[])
readopt (x:xs) =
    readopt xs >>= \pxs ->
        if x == "-h" then
            printHelp >> return Nothing
        else
            case pxs of
              Nothing -> return Nothing
              Just (wc,rcs) ->
                if head x == '-' then
                    case lookup (tail x) writeConfTable of
                      Nothing -> do
                          putStrLn $ "Unknown writing configuration '" ++ x ++ "'"
                          return $ Just (wc,rcs)
                      Just wc' -> return $ Just (wc',rcs)
                else
                    case lookup (ext x) readConfTable of
                      Nothing -> do
                          putStrLn $ "Cannot find parser for file '" ++ x ++ "'"
                          return $ Just (wc,rcs)
                      Just rc -> return $ Just (wc,(x,rc):rcs)
    where ext = last . splitOn "."

process :: String -> ReadConfig -> WriteConfig -> IO ()
process filename reader writer = do
    putStrLn $ "Parsing " ++ (readerName reader) ++ " file " ++ filename
    result <- processor reader filename
    putStrLn $ "Parsed!"
    putStrLn $ "Writing " ++ (writerName writer) ++ " file " ++ filename ++ "." ++ (writerExtension writer) ++ "..."
    writeFile (filename ++ "." ++ writerExtension writer) $ stringify writer result
    putStrLn $ "Done."


