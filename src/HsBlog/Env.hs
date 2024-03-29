module HsBlog.Env where

import Control.Monad.Reader
import Control.Applicative (liftA2)

data Env = Env { eBlogName :: String
               , eStylesheetPath :: FilePath
               , overwrite :: Bool
               } 
               deriving Show

defaultEnv :: Env
defaultEnv = Env "Tobi Blog" "style.css" False

type RIOEnv = ReaderT Env IO

liftEnvA2 :: (a -> b -> c) -> (Env -> IO a) -> (Env -> IO b) -> (Env -> IO c)
liftEnvA2 combine fa fb env = 
  liftA2 combine (fa env) (fb env)