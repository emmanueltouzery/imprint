import Test.Hspec
import Test.HUnit

import FrameRenderer
import Graphics.HsExif

main :: IO ()
main = hspec $ do
	describe "contents format parsing tests" runFormatParsingTests

runFormatParsingTests :: Spec
runFormatParsingTests = do
	testFile
	testDate
	testExif
	testPercent
	testComposite

testFile :: Spec
testFile = it "parses a file criteria" $ do
	assertEqual' (Right $ [Filename]) $ parseFormat "%file"

testDate :: Spec
testDate = it "parses a date" $ do
	assertEqual' (Right $ [DateFormat "%x HH:MM"]) $ parseFormat "%date{%x HH:MM}"

testExif :: Spec
testExif = it "parses an exif criteria" $ do
	assertEqual' (Right $ [ExifContents fnumber]) $ parseFormat "%aper"

testPercent :: Spec
testPercent = it "parses a percent sign" $ do
	assertEqual' (Right $ [StringContents "%"]) $ parseFormat "%%"

testComposite :: Spec
testComposite = it "parses a composite string" $ do
	assertEqual' (Right $ [StringContents "picture ", Filename, StringContents ", taken on: ", DateFormat "%x HH:MM", StringContents ". f/", ExifContents fnumber]) $ parseFormat "picture %file, taken on: %date{%x HH:MM}. f/%aper"

assertEqual' :: (Show a, Eq a) => a -> a -> Assertion
assertEqual' = assertEqual "doesn't match"
