module HsBlog.Directory
 ( convertDirectory
 , buildIndex
 ) where

import qualified HsBlog.Markup as M
import qualified HsBlog.Html as H
import HsBlog.Convert (convertStructure, convert)

import Control.Monad
import Data.List (partition)

import System.FilePath
import System.Directory
import System.Exit
import Control.Exception (catch, displayException, SomeException(..))

data DirContents = DirContents {
    dcFilesToProcess :: [(FilePath, String)]
  , dcFilesToCopy :: [FilePath]
  }

getDirFilesAndContent :: FilePath -> IO DirContents
getDirFilesAndContent dirPath = do
  files <- map (dirPath </>) <$> listDirectory dirPath
  let (txts, others) =
        partition ((== ".txt") . takeExtension) files
  txtFilesAndContent  <-
    applyIoOnList readFile txts >>= filterAndReportFailures
  DirContents <$> txtFilesAndContent <*> pure others


applyIoOnList :: (a -> IO b) -> [a] -> IO [(a , Either String b)]
applyIoOnList f = traverse makeIoPair
  where makeIoPair a = (,) a <$> catch (Right <$> f a)
                        (return . Left . displayException)

convertDirectory :: Bool -> FilePath -> FilePath -> IO ()
convertDirectory overwrite inpDir outDir = do
  DirContents toProcess toCopy <- getDirFilesAndContent inpDir
  createOutputDirectoryOrExit outDir
  let outputHtmls = txtsToRenderedHtml toProcess

  copyFiles outDir toCopy
  writeFiles outDir outputHtmls
  putStrLn "Done!"

buildIndex :: [(FilePath, M.Document)] -> H.Html
buildIndex files =
  let previews = map (uncurry indexBuilder) files

  in H.html_ "Tobi Blog"
    (H.h_ 1 (H.link_ "index.html" (H.txt_ "Blog"))
          <> H.h_ 2 (H.txt_ "Posts") <> mconcat previews)


indexBuilder :: String -> [M.Structure] -> H.Structure
indexBuilder path (M.Heading 1 heading : article) =
  H.h_ 3 (H.link_ path (H.txt_ heading)) <>
  foldMap convertStructure (take 3 article) <>
  H.p_ (H.link_ path (H.txt_ "..."))

indexBuilder path _ =
  H.h_ 3 (H.link_ path (H.txt_ path))