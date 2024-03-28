module Main where

import qualified HsBlog
import OptParser

import System.Exit
import System.Directory
import System.IO
import Control.Applicative ((<|>))
import Control.Exception (bracket, finally)

main :: IO ()
main = do
    options <- parse
    case options of
        ConvertDir inp out b -> HsBlog.convertDirectory inp out b

        ConvertSingle inp out b ->
            withInp inp (\t -> withOut b out . HsBlog.convertSingle t)



withInp :: SingleInput -> (String -> Handle -> IO r) -> IO r
withInp  Stdin action = action "" stdin
withInp  (InputFile file) action = withFile file ReadMode
                                  (action file)

withOut :: Bool -> SingleOutput -> (Handle -> IO b) -> IO b
withOut _  Stdout action = action stdout
withOut b (OutputFile file) action =
    do exists <- doesFileExist file
       shouldOpen <- if exists
        then (b ||) <$> HsBlog.confirmOverwrite file
        else pure True
       
       if shouldOpen
        then withFile file WriteMode action
        else exitFailure
