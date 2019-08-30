module Main (main) where

import Test.Hspec
import Text.Trifecta
import Semver

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main =
  hspec $ do
    let ps = parseString
        psv = ps parseSemVer mempty
    describe "Simple semver" $
      it "parses a simple semver" $ do
        let r = maybeSuccess $ psv "2.1.1"
        r `shouldBe` Just (SemVer 2 1 1 [] [])
