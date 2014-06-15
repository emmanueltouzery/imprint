import Test.Hspec
import Test.HUnit

import FrameRenderer

main :: IO ()
main = hspec $ do
	describe "contents format parsing tests" runFormatParsingTests

runFormatParsingTests :: Spec
runFormatParsingTests = do
	testEmptyString

testEmptyString :: Spec
testEmptyString = it "parses an empty string" $ do
	assertEqual' (Right $ [Filename]) $ parseFormat "%file"

assertEqual' :: (Show a, Eq a) => a -> a -> Assertion
assertEqual' = assertEqual "doesn't match"
