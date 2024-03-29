module HsBlog.Convert where

import qualified HsBlog.Markup as M
import qualified HsBlog.Html as Html
import qualified HsBlog.Html.Internal as HI
import HsBlog.Env(Env(..))
import Control.Monad.Reader



convertStructure :: M.Structure -> HI.Structure
convertStructure (M.Paragraph p) = Html.p_ $ Html.txt_ p
convertStructure (M.UnorderedList l) = Html.ul_ $ map (Html.p_ . Html.txt_) l
convertStructure (M.OrderedList l) = Html.ol_ $ map (Html.p_ . Html.txt_) l
convertStructure (M.CodeBlock l) = Html.code_ (unlines l)
convertStructure (M.Heading n txt) = Html.h_ n (Html.txt_ txt)


convert :: Env -> String -> M.Document -> Html.Html
convert env title doc = do
  let h = HI.title_ (eBlogName env <> " - " <> title) <> Html.stylesheet_ (eStylesheetPath env)
      b = websiteTitle <> article
      websiteTitle = Html.h_ 1 (Html.link_ "index.html" $ Html.txt_ $ eBlogName env)
      article = foldMap convertStructure doc
  HI.html_ h b