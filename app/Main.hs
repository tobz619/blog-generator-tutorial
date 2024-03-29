module Main where

import qualified HsBlog
import qualified HsBlog.Directory
import OptParser

import System.Exit
import System.Directory
import System.IO
import Control.Applicative ((<|>))
import Control.Exception (bracket, finally)
import HsBlog.Env (Env(..))


main :: IO ()
main = do
    options <- parse
    case options of
        ConvertDir inp out env -> HsBlog.Directory.convertDirectory env inp out 

        ConvertSingle inp out env ->
            withInp inp (\t -> withOut (overwrite env) out . HsBlog.convertSingle t)



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
