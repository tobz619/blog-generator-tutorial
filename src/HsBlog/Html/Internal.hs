module HsBlog.Html.Internal where
import GHC.Natural (Natural)

newtype Html = Html {render :: String} deriving Show

newtype Structure = Structure String deriving Show

instance Semigroup Structure where
    (<>) c1 c2 =
        Structure (getStructureString c1 <> getStructureString c2)

instance Monoid Structure where
    mempty = empty_

-- | Base case for when the list is empty
empty_ :: Structure
empty_ = Structure ""

-- | Builder that wraps our string in the tags it needs
el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"


-- | Basic html tags
head_, p_, li_, code_ :: String -> Structure
head_  = Structure . el "head" . escape
p_ = Structure . el "p" . escape
li_ = Structure . el "li"
code_ = Structure . el "pre" . escape


h_ :: Natural -> String -> Structure
h_ n = Structure . el ("h" <> show n) . escape



ul_, ol_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap (el "li" . getStructureString)
ol_ = Structure . el "ol" . concatMap (el "li" . getStructureString)

-- | Creates a html string from a string and a structure
html_ :: String -> Structure -> Html
html_ title cont = Html $ el "title" title <> getStructureString  cont

-- | Extracts the string from our structure
getStructureString :: Structure -> String
getStructureString str = case str of
    Structure s -> s


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