module OptParser 
     ( Options(..)
     , SingleInput(..)
     , SingleOutput(..)
     , parse
     )
    where

import Options.Applicative 
import Data.Maybe (fromMaybe)
import Data.Char (toUpper)
import HsBlog.Env(Env(..), defaultEnv)

data Options = ConvertSingle SingleInput SingleOutput Env
             | ConvertDir FilePath FilePath Env 
             deriving Show

data SingleInput = Stdin
                 | InputFile FilePath
                 deriving Show

data SingleOutput = Stdout
                  | OutputFile FilePath
                  deriving Show

parse :: IO Options
parse = execParser opts

opts :: ParserInfo Options
opts = info (helper <*> pOptions) 
            ( fullDesc 
              <> header "hs-blog-generator - a static blog generator"
              <> progDesc "Convert markup files or directories to html"
            )

pOptions :: Parser Options
pOptions = subparser $ pConvertSingleCommand <> pConvertDirCommand

--------------------------------------------------------------------
-- | Helper for making easy input options
inp :: String -> Parser FilePath
inp t = strOption (long "input"
                <> short 'i'
                <> metavar (map toUpper t)
                <> help ("Input " <> t))

-- | Helper for making easy output options
out :: String -> Parser String
out t = strOption (long "output"
                <> short 'o'
                <> metavar (map toUpper t)
                <> help ("Output " <> t))

pInputFile :: Parser SingleInput
pInputFile = InputFile <$> inp "file"

pOutputFile :: Parser SingleOutput
pOutputFile = OutputFile <$> out "file"

pConvertSingle :: Parser Options
pConvertSingle = ConvertSingle <$> pSingleInput <*> pSingleOutput <*> pEnv

pSingleInput :: Parser SingleInput
pSingleInput = fromMaybe Stdin <$> optional pInputFile

pSingleOutput :: Parser SingleOutput
pSingleOutput = fromMaybe Stdout <$> optional pOutputFile

pConvertSingleInfo :: ParserInfo Options
pConvertSingleInfo = info (helper <*> pConvertSingle) (progDesc "Convert a single markup source to html")

pConvertSingleCommand :: Mod CommandFields Options
pConvertSingleCommand = command "convert" pConvertSingleInfo

---------------------------------------------------------------------

pConvertDir :: Parser Options
pConvertDir = ConvertDir <$> inp "directory" <*> out "directory" <*> pEnv

pConvertDirCommandInfo :: ParserInfo Options
pConvertDirCommandInfo = info (helper <*> pConvertDir) (progDesc "Convert a directory of markup files to html")

pConvertDirCommand :: Mod CommandFields Options
pConvertDirCommand = command "convert-dir" pConvertDirCommandInfo

---------------------------------------------------------------------

pEnv :: Parser Env
pEnv =  Env <$> pBlogName <*> pStylesheet <*> pReplace

-------------------------

pBlogName :: Parser String
pBlogName = strOption (
    long "name"
    <> short 'N'
    <> metavar "STRING"
    <> help "blog name"
    <> value (eBlogName defaultEnv)
    <> showDefault
    )

pStylesheet :: Parser FilePath
pStylesheet = strOption (
    long "stylesheet"
    <> short 'S'
    <> metavar "FILE"
    <> help "stylesheet filename"
    <> value (eStylesheetPath defaultEnv)
    <> showDefault
    )

pReplace :: Parser Bool
pReplace = switch (
    long "replace"
    <> short 'r'
    <> help "Automatically replace if file or dir already exists"
    )