module Html.Strict.Attributes where

import Prelude ((.))

import Internal.EncodedHtml
import Internal.Escaping
import Internal.Attributes

href :: Html h => Unescaped h -> Attribute h
href = attribute "href" . replaceUnencodable urlCharReference
