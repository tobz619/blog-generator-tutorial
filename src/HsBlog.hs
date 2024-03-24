module HsBlog
    ( main
    , process
    , convertDirectory
    , convertSingle
    , confirmOverwrite
    , whenIO
    ) where

import qualified HsBlog.Markup as M 
import qualified HsBlog.Html as H
import HsBlog.Convert (convert)

import System.Directory
import System.Environment
import Control.Monad (when)
import System.IO

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> do content <- getContents
                     putStrLn $ process "Empty title" content
            
            [inp, out] -> whenIO (doesFileExist out) $
                    do whenIO (confirmOverwrite out) $
                          do content <- readFile inp
                             writeFile out (process inp content)
                                
            _ -> putStrLn "HUsage: runghc Main.hs [-- <inp> <out>]"

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

process :: String -> String -> String
process title = H.render . convert title . M.parseMarkup

whenIO :: Monad m => m Bool -> m () -> m ()
whenIO cond action =
  cond >>= when <*> pure action

convertSingle :: String -> Handle -> Handle -> IO ()
convertSingle title inp out = do
  content <- hGetContents inp
  hPutStrLn out (process title content)

convertDirectory :: FilePath -> FilePath -> Bool -> IO ()
convertDirectory from to bool = error "Not implemented"

