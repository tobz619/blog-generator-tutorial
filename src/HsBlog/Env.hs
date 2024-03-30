{- | Enviroment module

This module handles the environment created from parsing command-line
arguments.
-}
module HsBlog.Env where

import Control.Monad.Reader
import Control.Applicative (liftA2)
{- | The environment that will be created after parsing the commands
from the user
 -}
data Env = Env { eBlogName :: String -- ^ The name of the blog
               , eStylesheetPath :: FilePath -- ^ The filepath of the stylesheet
               , overwrite :: Bool -- ^ Automatic overwriting of files
               } 
               deriving Show

defaultEnv :: Env
defaultEnv = Env "Tobi Blog" "style.css" False

type RIOEnv = ReaderT Env IO

liftEnvA2 :: (a -> b -> c) -> (Env -> IO a) -> (Env -> IO b) -> (Env -> IO c)
liftEnvA2 combine fa fb env = 
  liftA2 combine (fa env) (fb env)