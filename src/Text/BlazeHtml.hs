module Text.BlazeHtml
    ( Html
    , HtmlElement (..)
    , toHtml
    , setAttribute
    , printHtml
    ) where

import Data.List (foldl')
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as M
import qualified Data.Text as T

-- | Representation for HTML attributes.
type Attributes = Map Text Text

-- | Data type for a HTML element.
data HtmlElement = HtmlElement Text Attributes Html
                 | HtmlLeafElement Text Attributes
                 | HtmlTextElement Text
                 | HtmlComment Text

-- | Representation of a chunk HTML.
type Html = [HtmlElement] -> [HtmlElement]

-- | Convert HTML to a list of elements.
toHtmlElements :: Html -> [HtmlElement]
toHtmlElements = ($ [])

-- | Create a chunk of HTML from an element.
toHtml :: HtmlElement -> Html
toHtml = (:)

-- | Set an attribute in the next HTML element.
setAttribute :: Text -- ^ Key to set.
             -> Text -- ^ Value to set.
             -> Html
setAttribute _ _ [] = []
setAttribute key value (x:xs) = setAttribute' x : xs
  where
    setAttribute' (HtmlElement tag attributes html) =
        HtmlElement tag (M.insert key value attributes) html
    setAttribute' (HtmlLeafElement tag attributes) =
        HtmlLeafElement tag (M.insert key value attributes)
    setAttribute' y = y

printAttributes :: Attributes -> Text
printAttributes = M.foldWithKey append T.empty
  where
    append key value = T.append
        (key `T.append` T.pack "=\"" `T.append` value `T.append` T.pack "\" ")

printHtmlElement :: HtmlElement -> Text
printHtmlElement (HtmlTextElement text) = text
printHtmlElement (HtmlComment text) =
    T.pack "<!--" `T.append` text `T.append` T.pack "-->"
printHtmlElement (HtmlElement tag attributes html) =
    T.pack "<" `T.append` tag `T.append` T.pack " "
               `T.append` printAttributes attributes `T.append` T.pack ">"
               `T.append` printHtml html
               `T.append` T.pack "</" `T.append` tag `T.append` T.pack ">"
printHtmlElement (HtmlLeafElement tag attributes) =
    T.pack "<" `T.append` tag `T.append` T.pack " "
               `T.append` printAttributes attributes `T.append` T.pack "/>"

printHtml :: Html -> Text
printHtml = foldl' T.append T.empty . map printHtmlElement . toHtmlElements
