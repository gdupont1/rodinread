{-|
Module      : Main
Description : Main module for this project
Copyright   : (c) Guillaume Dupont, 2018
License     : MIT
Maintainer  : guillaume.dupont55@gmail.com

Main module, entry point.
-}
module Main where

import System.Environment
import Control.Monad (forM_)

import Substitution
import Options

data Conf = Conf {
    writer :: WriteConfig,
    reader :: ReadConfig,
    substitutions :: SubstitutionTable,
    filename :: String,
    outdir :: String
}

-- | The main function.
-- Mainly read the options and do the processing
main :: IO ()
main = getArgs >>= (return . readopt) >>= \x ->
    case x of
      NoOption -> return ()
      Help -> printHelp
      Error errs -> printErrors errs
      Cmd cmd ->
          dumpand (readSubstitutions (substitutionFiles cmd)) $ \subst ->
              forM_ (readconfs cmd) $ \(fn,rc) -> process $ Conf {
                  writer = writeconf cmd,
                  reader = rc,
                  substitutions = subst,
                  filename = fn, 
                  outdir = outputdir cmd
              }
    where dumpand :: (Show w, Show e, Show a) => WrapT e w IO a -> (a -> IO ()) -> IO ()
          dumpand wt f = do
              wr <- runWrapT wt
              putStrLn $ show wr
              case wr of
                WrapError _ -> return ()
                WrapWarnings _ a -> (putStrLn "Substitutions:" >> putStrLn (show a) >> putStrLn "") >> f a

-- | Print errors
printErrors :: [String] -> IO ()
printErrors errs = do
    putStrLn "Process stopped due to errors !"
    forM_ errs (\e -> putStrLn e)

-- | Print help
printHelp :: IO ()
printHelp = do
    putStrLn "Rodin file transformer"
    putStrLn ""
    putStrLn "Syntax:"
    putStrLn "    program [-tex|-ascii] <file1> [<file2> [...]] [-d outdir] [-s subst]"
    putStrLn ""
    putStrLn "Use -tex or -ascii to transform the given files in .tex or in .ascii (plain text)."
    putStrLn "Give the list of files; they will be combined in one big file. The program will"
    putStrLn "automatically determine the correct way to read it *based on its extension."
    putStrLn ""
    putStrLn "Use -d to specify an output directory (if no -d provided original file directory"
    putStrLn "is used by default)."
    putStrLn ""
    putStrLn "Use -s to specify a substitution file. Multiple files can be specified."
    putStrLn "Substitution files are written in a @pattern@ => @template@ where 'pattern' is a"
    putStrLn "regex-like pattern and 'template' is a succession of tokens, possibly including"
    putStrLn "references to capture group (with $0, $1, ...; $0 is the whole matched sequence)"
    putStrLn ""
    putStrLn "Substitution patterns are simplified and taylored version of normal regex."
    putStrLn "They are mostly 'print-like' based: a token match a pattern element if both print"
    putStrLn "in a similar way. Besides, some special token are defined:"
    putStrLn "   \\o     Matches any operator ('+', '*', '|->', ...)"
    putStrLn "   \\I     Matches any special identifier ('NATURALS', 'INTEGERS', ...)"
    putStrLn "   \\s     Matches any space ('\\t', ' ', '\\n', ...)"
    putStrLn "   \\k     Matches any 'simple token' ('(', ')', '[', ']', ',', ...)"
    putStrLn "   \\i     Matches any identifier (non-space, non-operator, non-simple, non-special)"
    putStrLn "    .     Matches any token"
    putStrLn "Backslash can also be used to escape any special character: \\, (, ), ., *, ..."
    putStrLn "For especially long tokens, it is possible to delimitate their matcher between \""
    putStrLn "Note that between \" special characters are treated as normal."
    putStrLn "Last, pattern may additionally use the following combining operators:" 
    putStrLn "    _*    Means zero or more repetition of a group or token"
    putStrLn "    _+    Means one or more repetition of a group or token"
    putStrLn "    _?    Means zero or one repetition of a group or token"
    putStrLn "    _|_   Means either left-hand side or right-hand side"
    putStrLn "    (...) Delimitate a caturing group"
    putStrLn ""
    putStrLn "Supported extensions:"
    putStrLn "  Context description files (.buc)"
    putStrLn "  Machine description files (.bum)"
    putStrLn "  Theory description files (.tuf)"
    putStrLn "  Proof obligation files (.bpo)"
    putStrLn ""

process :: Conf -> IO ()
process conf = do
    putStrLn $ "Parsing " ++ (readerName $ reader conf) ++ " file " ++ (filename conf)
    content <- processor (reader conf) (filename conf)
    putStrLn $ "Performing substitution..."
    case substitute (substitutions conf) content of
      Left e -> putStrLn $ "Error while performing substitution on file " ++ (filename conf) ++ ":\n" ++ show e
      Right ct -> do 
        putStrLn $ "Writing " ++ (writerName $ writer conf) ++ " file " ++ (filename conf) ++ "." ++ (writerExtension $ writer conf) ++ "..."
        writeOut (writer conf) (filename conf) (outdir conf) ct
        putStrLn $ "Done."



