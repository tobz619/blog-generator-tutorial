module Convert where

import qualified Markup as M
import qualified Html
import qualified Html.Internal as HI



convertStructure :: M.Structure -> HI.Structure
convertStructure (M.Paragraph p) = Html.p_ p
convertStructure (M.UnorderedList l) = Html.ul_ $ map Html.p_ l
convertStructure (M.OrderedList l) = Html.ol_ $ map Html.p_ l
convertStructure (M.CodeBlock l) = Html.code_ (unlines l)
convertStructure (M.Heading n txt) = Html.h_ n txt


convert :: String -> M.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure