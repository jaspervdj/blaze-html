import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit
import OutputVerification
import HtmlProperties
import Data.Monoid(mempty)
import Text.BlazeHtml.Render.HtmlIO(HtmlIO)
import Text.BlazeHtml.Render.HtmlPrettyText(HtmlPrettyText)
import Text.BlazeHtml.Render.HtmlByteString(HtmlByteString)
import Text.BlazeHtml.Render.HtmlText(HtmlText)

import SimpleHtmlExamples (simpleHtmlExamples)


main = defaultMain blazeTests

blazeTests =
    [ simpleHtmlExamples
    -- , testGroup "unittest Group" [ testCase "simple text compare" $ testForSameOutput model ]
    -- , testGroup "properties using HtmlText" $ buildTestGroup (mempty :: HtmlText) pHtmlText
    -- , testGroup "properties using HtmlPrettyText" $ buildTestGroup (mempty :: HtmlPrettyText) pHtmlPrettyText
    -- , testGroup "properties using HtmlByteString" $ buildTestGroup (mempty :: HtmlByteString) pHtmlByteString
    -- , testGroup "properties using HtmlIO" $ buildTestGroup (mempty :: HtmlIO) pHtmlIO
    ]

buildTestGroup empty pelem = 
    map (\a->testCase "" a) (collectTests empty pelem)
     
