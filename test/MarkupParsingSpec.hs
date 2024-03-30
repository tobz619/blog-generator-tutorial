module MarkupParsingSpec where

import Test.Hspec
import HsBlog.Markup

spec :: Spec
spec = do
  describe "Markup parsing tests" $ do
    it "empty" $ shouldBe (parseMarkup "") []
    
    it "paragraph" $ 
      shouldBe (parseMarkup "hello world") [Paragraph "hello world"]

    it "heading 1" $
      shouldBe (parseMarkup "* Heading 1") [Heading 1 "Heading 1"]
    
    it "code" $
      shouldBe (parseMarkup "> main = putStrLn \"Hello!\"")
               [CodeBlock ["main = putStrLn \"Hello!\""]]