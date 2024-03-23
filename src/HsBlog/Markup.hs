module HsBlog.Markup
    ( Structure(..)
    , Document
    , parseMarkup
    ) 
where

import Numeric.Natural
import Data.Maybe (maybeToList)


type Document = [Structure]

data Structure = Heading Natural String
               | Paragraph String
               | UnorderedList [String]
               | OrderedList [String]
               | CodeBlock [String]
    deriving (Show, Eq)

parseMarkup :: String -> Document
parseMarkup = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines ctx txts =
    case txts of
        -- done
        [] -> maybeToList ctx

        -- Heading 1
        ('*' : ' ' : line) : rest ->
            maybe id (:) ctx (Heading 1 (trim line) : parseLines Nothing rest)

        -- Unordered List
        ('-' : ' ' : line) : rest ->
            case ctx of
                Just (UnorderedList list) ->
                    parseLines (Just (UnorderedList (list <> [trim line]))) rest

                _ -> maybe id (:) ctx (parseLines (Just (UnorderedList [trim line])) rest)

        -- Unordered List
        ('#' : ' ' : line) : rest ->
            case ctx of
                Just (OrderedList list) ->
                    parseLines (Just (OrderedList (list <> [trim line]))) rest

                _ -> maybe id (:) ctx (parseLines (Just (OrderedList [trim line])) rest)

        -- Code block
        ('>' : ' ' : line) : rest ->
            case ctx of
                Just (CodeBlock c) ->
                    parseLines (Just (CodeBlock (c <> [line]))) rest

                _ -> maybe id (:) ctx (parseLines (Just (CodeBlock [line])) rest)

        -- paragraph
        curLine : rest ->
            let line = trim curLine
            in if line == ""
                then maybe id (:) ctx (parseLines Nothing rest)
                else case ctx of
                    Just (Paragraph p) ->
                        parseLines (Just (Paragraph (unwords [p, line]))) rest

                    _ -> maybe id (:) ctx (parseLines (Just (Paragraph line)) rest)

trim :: String -> String
trim = unwords . words


-- examples:
example1 =  [Paragraph "Hello World"]

example2 = [ Paragraph "Remember that multiple lines with no separation are grouped together into a single paragraph but list items remain separate."
           , OrderedList [ "# Item 1 of a list"
                         , "# Item 2 of the same list"]
           ]

example3 = [Heading 1 "What is the meaning of this?"
           , Paragraph "Many times in life we struggling"
           , Heading 3 "Ways we struggle"
           , UnorderedList ["- Not having enough to eat"
                           ,"- Not having enough to drink"]
           , Paragraph "Anyway, this is the end of the chat"]