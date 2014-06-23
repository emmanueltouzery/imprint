import Test.Hspec
import Test.HUnit

import FrameRenderer
import Graphics.HsExif
import Helpers (getTargetFolder)

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
	testShouldFail
	testShouldFail2
	testGetTargetFolder

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
	assertEqual' (Right $ [StringContents "picture ", Filename,
			StringContents ", taken on: ", DateFormat "%x HH:MM",
			StringContents ". f/", ExifContents fnumber]) 
			$ parseFormat "picture %file, taken on: %date{%x HH:MM}. f/%aper"

testShouldFail :: Spec
testShouldFail = it "fails parsing a bad string" $ do
	assertBool "fails" $ isLeft $ parseFormat "%ss"

testShouldFail2 :: Spec
testShouldFail2 = it "fails parsing a bad string" $ do
	assertBool "fails" $ isLeft $ parseFormat "%file %ss"

testGetTargetFolder :: Spec
testGetTargetFolder = it "gets the target folder correctly" $ do
	assertEqual' "/home/emmanuel/Pictures/2010/imprint" $
		getTargetFolder ["/home/emmanuel/Pictures/2010/f1.jpg",
			"/home/emmanuel/Pictures/2010/f2.jpg",
			"/home/emmanuel/Pictures/2010/subfolder/f2.jpg"]
	assertEqual' "/home/emmanuel/Pictures/2011/subfolder/imprint" $
		getTargetFolder ["/home/emmanuel/Pictures/2011/subfolder/f1.jpg",
			"/home/emmanuel/Pictures/2011/subfolder/f2.jpg"]
	assertEqual' "/home/emmanuel/Pictures/2012/imprint" $
		getTargetFolder ["/home/emmanuel/Pictures/2012/subfolder/f1.jpg",
			"/home/emmanuel/Pictures/2012/subfolder2/f2.jpg"]

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

assertEqual' :: (Show a, Eq a) => a -> a -> Assertion
assertEqual' = assertEqual "doesn't match"
