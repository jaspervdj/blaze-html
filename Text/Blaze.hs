{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, Rank2Types,
             FlexibleInstances #-}
-- | BlazeHtml is an HTML combinator library. It provides a way to embed HTML in
-- Haskell in an efficient and convenient way, with a light-weight syntax.
--
-- To use the library, one needs to import a set of HTML combinators. For
-- example, you can use HTML 4 Strict.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Prelude hiding (head, id, div)
-- > import Text.Blaze.Html4.Strict hiding (map)
-- > import Text.Blaze.Html4.Strict.Attributes hiding (title)
--
-- To render the page later on, you need a so called Renderer. The recommended
-- renderer is an UTF-8 renderer which produces a lazy bytestring.
--
-- > import Text.Blaze.Renderer.Utf8 (renderHtml)
--
-- Now, you can describe pages using the imported combinators.
--
-- > page1 :: Html
-- > page1 = html $ do
-- > 	head $ do
-- > 		title "Introduction page."
-- > 		link ! rel "stylesheet" ! type_ "text/css" ! href "screen.css"
-- > 	body $ do
-- > 		div ! id "header" $ "Syntax"
-- > 		p "This is an example of BlazeHtml syntax."
-- > 		ul $ forM_ [1, 2, 3] (li . string . show)
--
-- The resulting HTML can now be extracted using:
--
-- > renderHtml page1
--
module Text.Blaze
    (
      -- * Important types.
      Html
    , Tag
    , Attribute
    , AttributeValue

      -- * Creating attributes.
    , dataAttribute
    , customAttribute

      -- * Converting values to HTML.
    , text
    , preEscapedText
    , string
    , preEscapedString
    , showHtml
    , preEscapedShowHtml
    , unsafeByteString

      -- * Creating tags.
    , textTag
    , stringTag

      -- * Converting values to attribute values.
    , textValue
    , preEscapedTextValue
    , stringValue
    , preEscapedStringValue
    , unsafeByteStringValue

      -- * Setting attributes
    , (!)
    ) where

import Text.Blaze.Internal
