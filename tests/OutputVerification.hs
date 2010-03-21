{-# LANGUAGE OverloadedStrings #-}
module OutputVerification

where

import Test.HUnit
import Test.QuickCheck
import Text.BlazeHtml.Text(pack)
import qualified Text.BlazeHtml.Html as A
import Text.BlazeHtml.Render.HtmlText(HtmlText,htmlText)
import Data.List(foldl')
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Monoid(mappend)

modelFile = "./model.txt"
test1 model = TestCase (assertEqual "should stay the same" model (htmlText simpleText))
tests model = TestList [TestLabel "test1" $ test1 model]
testForSameOutput model = assertEqual "should stay the same" model (htmlText simpleText)

simpleText :: HtmlText
simpleText = let x = A.blockquote $ A.img "foo.png" "Foo Illustration"
                 y = addNumberedAttrs 3 (nestElements x 5) `mappend` addNumberedAttrs 3 (nestElements x 5)  in
               A.b y

nestElements x n
  | n == 0 = A.nodeElement (pack $ "a" ++ show n) (addNumberedAttrs 2 x)
  | otherwise = A.addUnescapedAttribute "s" "d" $ A.nodeElement (pack $ "a" ++ show n) rest
      where rest = nestElements x (n-1)

addNumberedAttrs :: (Num b, A.Html a, Enum b) => b -> a -> a
addNumberedAttrs n e0 =
    foldl' (\e i -> A.addUnescapedAttributes [("id", pack $ show i),("name", pack $ show (-i))] e) e0 [1..n]

prop_dummy :: [Int] -> Property
prop_dummy xs =
    collect xs $
    reverse (reverse xs) == xs

main2 = do
  content <- TIO.readFile modelFile
  runTestTT $ tests content


testContent = TIO.readFile modelFile

setup = do
  print $ htmlText simpleText
  TIO.writeFile modelFile $ htmlText simpleText
