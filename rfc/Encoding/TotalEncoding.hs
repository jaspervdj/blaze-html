{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Encoding.Core where

-- | An encoding that never needs to replace a character; eg.g. UTF-8, UTF-16,
-- and UTF-32.
newtype TotalEncoding s = TotalEncoding
    { runTotalEncoding :: s -> s
    } deriving( Monoid, UnicodeSequence )

-- This is of course an instance of @Encoded@.
instance UnicodeSequence s => Encoded (TotalEncoding s) where
    encodingTag        = TotalEncoding id
    replaceUnencodable = const id
