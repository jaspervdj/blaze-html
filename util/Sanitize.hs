-- | A program to sanitize an HTML tag to a Haskell function.
--
module Sanitize
    ( sanitize
    ) where

import Data.Set (Set)
import qualified Data.Set as S

-- | A set of standard Haskell keywords, which cannot be used as combinators.
--
keywords :: Set String
keywords = S.fromList
    [ "case", "class", "data", "default", "deriving", "do", "else", "if"
    , "import", "in", "infix", "infixl", "infixr", "instance" , "let", "module"
    , "newtype", "of", "then", "type", "where"
    ]

-- | Sanitize a tag. This function returns a name that can be used as
-- combinator in haskell source code.
--
-- Examples:
--
-- > sanitize "class" == "class_"
-- > sanitize "http-equiv" = "http_equiv"
--
sanitize :: String -> String
sanitize = appendUnderscore . map replace'
  where
    replace' '-' = '_'
    replace' x   = x

    appendUnderscore tag | tag `S.member` keywords = tag ++ "_"
                         | otherwise               = tag
