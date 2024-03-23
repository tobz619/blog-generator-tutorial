module Main where

import qualified Markup
import qualified Html
import Convert (convert)

import System.Directory
import System.Environment
import Control.Monad (when)


main = do args <- getArgs
          case args of
            [] -> putStrLn "Help to be done"
            [inp, out] -> whenIO (doesFileExist out) $
                    do b <- confirmOverwrite out
                       when b $ do content <- readFile inp
                                   writeFile out content
            _ -> putStrLn "Help to be done"

myhtml =
     html_ "My title"
     ( (<>) (h_ 1 "Heading")
     ( (<>) (p_ "P1")
            (p_ "P2")
     )
     )

confirmOverwrite :: FilePath -> IO Bool
confirmOverwrite out  =
    do putStr $ "File " ++ out ++ " exists. "
       putStrLn "Are you sure you want to overwrite this file?"
       response <- getLine
       case response of
         "y" -> pure True
         "n" -> pure False
         _ -> do putStrLn "Invalid input. Use y or n"
                 confirmOverwrite out

whenIO cond action =
    do b <- cond
       when b action