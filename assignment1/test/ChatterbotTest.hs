import Chatterbot

import Test.Tasty
import Test.Tasty.HUnit

reductionTest = testCase "reduction test" $
  reductionsApply reductions (words "can you please tell me what Haskell is")
                         @?= (words "what is Haskell")

reflectTest = testCase "reflect test" $
    reflect ["i", "will", "never", "see", "my", "reflection", "in", "your", "eyes"]
        @?= ["you", "will", "never", "see", "your", "reflection", "in", "my", "eyes"]

transformations = [
    (words "I hate *", words "Why do you hate * ?"),
    (words "I like *", words "I also like * .")
  ]

rulesApplyTest = testGroup "rulesApply tests" [
    testCase "hate mother" $ rulesApply transformations (words "I hate my mother")
      @?= (words "Why do you hate your mother ?"),
    testCase "argh!" $ rulesApply transformations (words "ARGH!")
      @?= (words ""),
    testCase "like mother" $ rulesApply transformations (words "I like my mother")
      @?= (words "I also like your mother .")
  ]

reduceTest = testCase "reduce" $
    (reduce.words) "can you please tell me what Haskell is" @?= words "what is Haskell"

substituteSpecTest = testCase "substitute test" $
    substitute 'x' "3*cos(x) + 4 - x" "5.37" @?= "3*cos(5.37) + 4 - 5.37"

matchSpecTest = testGroup "matchSpec tests" [
    testCase "math" $ match 'x' "2*x+3" "2*7+3" @?= Just "7",
    testCase "gandalf" $ match '*' "frodo" "gandalf" @?= Nothing,
    testCase "list" $ match 2 [1,3..5] [1,3..5] @?= Just [],
    testCase "you and me" $ match '*' "* and *" "you and me" @?= Just "you",
    testCase "math" $ match 'x' "2*x+3+x" "2*7+3" @?= Nothing,
    testCase "bdo" $ match '*' "*do" "bdo" @?= Just "b",
    testCase "dobedo" $ match '*' "*do" "dobedo" @?= Just "dobe",
    testCase "bedobe" $ match '*' "*do" "bedobe" @?= Nothing,
    testCase "empty" $ match '*' "" "" @?= Just [],
    testCase "abba" $ match '*' "abba" "" @?= Nothing,
    testCase "abba" $ match '*' "" "abba" @?= Nothing,
    testCase "a" $ match '*' "a" "a" @?= Just [],
    testCase "a" $ match '*' "*" "a" @?= Just "a",
    testCase "abba" $ match '*' "*" "abba" @?= Just "abba",
    testCase "aXb" $ match '*' "*X*" "aXb" @?= Just "a",
    testCase "aaXbb" $ match '*' "*X*" "aaXbb" @?= Just "aa"
  ]

frenchPresentation = ("My name is *", "Je m'appelle *")

transformationApplyTest = testGroup "transformationApply tests" [
    testCase "Zacharias" $ transformationApply '*' id "My name is Zacharias" frenchPresentation
      @?= Just "Je m'appelle Zacharias",
    testCase "45" $ transformationApply '*' id "My shoe size is 45" frenchPresentation
      @?= Nothing
  ]

swedishPresentation = ("My name is *", "Mitt namn är *")
presentations = [frenchPresentation, swedishPresentation]

transformationsApplyTest = testGroup "transformationsApply tests" [
    testCase "Zacharias" $ transformationsApply '*' id presentations "My name is Zacharias"
      @?= Just "Je m'appelle Zacharias",
    testCase "Zacharias" $ transformationsApply '*' id (reverse presentations) "My name is Zacharias"
      @?= Just "Mitt namn är Zacharias",
    testCase "45" $ transformationsApply '*' id (reverse presentations) "My shoe size is 45"
      @?= Nothing
  ]

allTests = testGroup "all tests"
  [ substituteSpecTest
  , matchSpecTest
  , transformationApplyTest
  , transformationsApplyTest
  , reflectTest
  , rulesApplyTest
  , reductionTest
  ]

main :: IO ()
main = defaultMain allTests
