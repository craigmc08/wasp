module WaspIgnoreFileTest where

import Test.Tasty.Hspec
import Test.Tasty.QuickCheck (property)

import WaspIgnoreFile (parseWaspIgnoreFile, ignores)

spec_IgnoreFile :: Spec
spec_IgnoreFile = do
    describe "IgnoreFile" $ do
        it "When given a single pattern, should match it" $ do
            let ignoreFile = parseWaspIgnoreFile "*.tmp"
            (ignoreFile `ignores` "a.tmp") `shouldBe` True
            (ignoreFile `ignores` "a.src") `shouldBe` False
        
        it "When given a blank input, should match nothing" $ do
            let ignoreFile = parseWaspIgnoreFile ""
            property $ \fp -> not $ ignoreFile `ignores` fp
        
        it "When given a comment as the only line, should match nothing" $ do
            let ignoreFile = parseWaspIgnoreFile "# test comment"
            property $ \fp -> not $ ignoreFile `ignores` fp
        
        it "When the only difference between two files is a comment, the files should match the same strings" $ do
            let comment = "\n# test comment"
            property $ \pat fp -> (parseWaspIgnoreFile pat `ignores` fp) ==
                                  (parseWaspIgnoreFile (pat ++ comment) `ignores` fp)

        it "When given 2 patterns, should match the path if either of the patterns match" $ do
            let pat1 = parseWaspIgnoreFile "a"
            let pat2 = parseWaspIgnoreFile "b"
            let patBoth = parseWaspIgnoreFile "a\nb"
            property $ \fp -> patBoth `ignores` fp == (pat1 `ignores` fp || pat2 `ignores` fp)
