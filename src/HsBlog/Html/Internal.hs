module HsBlog.Html.Internal where
import GHC.Natural (Natural)

newtype Html = Html { render :: String } deriving Show

newtype Content = Content { getContent :: String } deriving Show

newtype Structure = Structure {getStructureString :: String} deriving Show

newtype Head = Head {getHeadInfo :: String} deriving Show

instance Semigroup Structure where
    (<>) c1 c2 =
        Structure (getStructureString c1 <> getStructureString c2)

instance Monoid Structure where
    mempty = empty_

instance Semigroup Content where
  (<>) c1 c2 =  Content (getContent c1 <> getContent c2)

instance Monoid Content where
  mempty = Content ""

instance Semigroup Head where
  (<>) h1 h2 =
    Head (getHeadInfo h1 <> getHeadInfo h2)
  
instance Monoid Head where
  mempty = Head "" 

-- | Base case for when the list is empty
empty_ :: Structure
empty_ = Structure ""

-- | Builder that wraps our string in the tags it needs
el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

elAttr :: String -> String -> String -> String
elAttr tag attrs content =  
  "<" <> tag <> " " <> attrs <>">" <> content <> "</" <> tag <> ">"

-- | Basic html tags
head_ :: String -> Head 
head_  = Head . el "head" 

title_ :: String -> Head
title_ = Head . el "title" . escape

stylesheet_ :: FilePath -> Head
stylesheet_ path = Head $ 
  "<link rel=\"stylesheet\" type=\"text/css\" href=\"" <> escape path <> "\">"

meta_ :: String -> String -> Head
meta_ name cont = Head $
  "<meta name=\"" <> escape name <> "\" content=\"" <> escape cont <> "\">"

p_ :: Content -> Structure
p_ = Structure . el "p" . getContent

h_ :: Natural -> Content -> Structure
h_ n = Structure . el ("h" <> show n) . getContent

code_ :: String -> Structure
code_ = Structure . el "pre" . escape


ul_, ol_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap (el "li" . getStructureString)
ol_ = Structure . el "ol" . concatMap (el "li" . getStructureString)

-- | Creates a html string from a string and a structure
html_ :: Head -> Structure -> Html
html_ (Head h) cont = Html $ el "head" h <> el "body" (getStructureString  cont)

txt_ :: String -> Content
txt_ = Content . escape

link_ :: String -> Content -> Content
link_ path con = Content $ 
  elAttr "a" ("href=\"" <> escape path <> "\"") (getContent con)

img_ :: String -> Content
img_ path =
  Content $ "<img src=\"" <> escape path <> "\">"

b_ :: Content -> Content
b_ = Content . el "b" . getContent

i_ :: Content -> Content
i_ = Content . el "i" . getContent

-- | Converts key characters into replacements
escape :: String -> String
escape =
    let escapeChar c = case c of
            '<' -> "&lt;"
            '>' -> "&gt;"
            '&' -> "&amp;"
            '"' -> "&quot;"
            '\'' -> "&#39;"
            _ -> [c]
    in
        concatMap escapeChar