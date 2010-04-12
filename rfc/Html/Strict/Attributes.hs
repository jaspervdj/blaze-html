{-# LANGUAGE OverloadedStrings #-}
module Html.Strict.Attributes where

import Prelude ((.), String)

import Data.Text (Text)

import Internal.Html
import Internal.Escaping
import Internal.Attributes

href :: Html h => Text -> Attribute h
href = attribute "href" . unicodeText

alt :: Html h => Text -> Attribute h
alt = attribute "alt" . unicodeText

src :: Html h => Text -> Attribute h
src = attribute "src" . unicodeText

id :: Html h => Text -> Attribute h
id = attribute "id" . unicodeText

class_ :: Html h => Text -> Attribute h
class_ = attribute "class" . unicodeText

name :: Html h => Text -> Attribute h
name = attribute "name" . unicodeText
