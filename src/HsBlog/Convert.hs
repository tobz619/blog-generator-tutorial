module HsBlog.Convert where

import qualified HsBlog.Markup as M
import qualified HsBlog.Html as Html
import qualified HsBlog.Html.Internal as HI



convertStructure :: M.Structure -> HI.Structure
convertStructure (M.Paragraph p) = Html.p_ $ Html.txt_ p
convertStructure (M.UnorderedList l) = Html.ul_ $ map (Html.p_ . Html.txt_) l
convertStructure (M.OrderedList l) = Html.ol_ $ map (Html.p_ . Html.txt_) l
convertStructure (M.CodeBlock l) = Html.code_ (unlines l)
convertStructure (M.Heading n txt) = Html.h_ n (Html.txt_ txt)


convert :: String -> M.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure