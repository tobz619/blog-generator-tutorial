module Main where

import qualified HsBlog
import OptParser

import System.Exit
import System.Directory
import System.IO
import Control.Applicative ((<|>))

main :: IO ()
main = do
    options <- parse
    case options of
        ConvertDir inp out b -> HsBlog.convertDirectory inp out b

        ConvertSingle inp out b -> do
            (title, inpHandle) <- case inp of
                Stdin -> pure ("", stdin)
                InputFile file -> (,) file <$> openFile file ReadMode

            outHandle <- case out of
                Stdout -> pure stdout

                OutputFile file -> do
                    exists <- doesFileExist file
                    shouldOpenFile <- if exists
                                    then (b ||) <$> HsBlog.confirmOverwrite file
                                    else pure True

                    if shouldOpenFile
                        then openFile file WriteMode
                        else exitFailure


            HsBlog.convertSingle title inpHandle outHandle
            hClose inpHandle
            hClose outHandle