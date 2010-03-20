{-# LANGUAGE OverloadedStrings #-}
module OutputVerification

where

import Test.HUnit
import Test.QuickCheck
import Text.BlazeHtml.Text
import qualified Text.BlazeHtml.Html as A
import Text.BlazeHtml.Render.HtmlText
import Data.List(foldl')
import System.IO

test1 model = TestCase (assertEqual "should stay the same" model (renderHtmlText simpleText))
tests model = TestList [TestLabel "test1" $ test1 model]

simpleText :: HtmlText
simpleText = let x = A.blockquote $ A.img "foo.png" "Foo Illustration"
                 y = addNumberedAttrs 3 (nestElements x 5) in
               A.b y

nestElements x n
  | n == 0 = A.renderElement (pack $ "a" ++ show n) (addNumberedAttrs 2 x)
  | otherwise = A.addUnescapedAttribute "s" "d" $ A.renderElement (pack $ "a" ++ show n) rest
      where rest = nestElements x (n-1)

addNumberedAttrs :: (Num b, A.Html a, Enum b) => b -> a -> a
addNumberedAttrs n e0 =
    foldl' (\e i -> A.addUnescapedAttributes [("id", pack $ show i),("name", pack $ show (-i))] e) e0 [1..n]

prop_dummy :: [Int] -> Property
prop_dummy xs =
    collect xs $
    reverse (reverse xs) == xs

main = do
  content <- readFile "./model.txt"
  runTestTT $ tests $ pack (take (length content - 1) content)

-- setup = do
--   writeFile "./model.txt" $ simpleText
