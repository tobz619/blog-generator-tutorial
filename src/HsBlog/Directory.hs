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
import Data.Either (fromRight)
import System.IO (hPutStrLn, stderr)
import HsBlog (whenIO, confirmOverwrite)
import Data.Foldable (traverse_)

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
  pure $ DirContents txtFilesAndContent others


applyIoOnList :: (a -> IO b) -> [a] -> IO [(a , Either String b)]
applyIoOnList f = traverse makeIoPair
  where makeIoPair a = (,) a <$> 
                        catch (Right <$> f a)
                              (\(SomeException e) ->
                                 pure . Left . displayException $ e)

filterAndReportFailures :: [(a, Either String b)] -> IO [(a,b)]
filterAndReportFailures = foldMap checkFail
  where checkFail (file, contOrFail) = 
          either
          (\err -> do hPutStrLn stderr err
                      pure []
          )
          (pure . (:[]) . (,) file)
          contOrFail

createOutputDirectoryOrExit :: Bool -> FilePath -> IO ()
createOutputDirectoryOrExit b outDir =
  whenIO (not <$> createOutputDirectory b outDir)
  (hPutStrLn stderr "Cancelled." *> exitFailure)

createOutputDirectory :: Bool -> FilePath -> IO Bool
createOutputDirectory b dir = do
  create <- ifM 
              (doesDirectoryExist dir)
              (do override <- (b ||) <$> confirmOverwrite "Output directory exists. Override?"
                  when override (removeDirectoryRecursive dir)
                  pure override)
              (pure True)
  when create (createDirectory dir)
  pure create

ifM :: Monad m => m Bool -> m b -> m b -> m b
ifM mb t f = mb >>= \b -> if b then t else f

txtsToRenderedHtml :: [(FilePath, String)] -> [(FilePath, String)]
txtsToRenderedHtml ts = (indexHead ts :) . map (convertFile . toOutputMarkupFile) $ ts
  where toOutputMarkupFile (path, cont) = 
          (takeBaseName path <.> "html", M.parseMarkup cont)
 
        convertFile (path, markup) =
          (path, H.render . convert path $ markup)
        
        indexHead f = ("index.html", H.render . buildIndex . map toOutputMarkupFile $ f)

copyFiles :: FilePath -> [FilePath] -> IO ()
copyFiles outDir fs = void . filterAndReportFailures =<< applyIoOnList copyFromTo fs
  where copyFromTo f = copyFile f (outDir </> takeFileName f)

writeFiles :: [Char] -> [(FilePath, String)] -> IO ()
writeFiles outDir fs = void . filterAndReportFailures =<< applyIoOnList writeFileContent fs
  where writeFileContent (f, cont) = writeFile (outDir </> f) cont

convertDirectory :: Bool -> FilePath -> FilePath -> IO ()
convertDirectory overwrite inpDir outDir = do
  DirContents toProcess toCopy <- getDirFilesAndContent inpDir
  createOutputDirectoryOrExit overwrite outDir
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