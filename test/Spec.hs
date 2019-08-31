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
    describe "Simple semver" $ do
      it "parses semver with only number" $ do
        let r = maybeSuccess $ psv "2.1.1"
        r `shouldBe` Just (SemVer 2 1 1 [] [])
      it "parses semver with release" $ do
        let r = maybeSuccess $ psv "1.0.0-x.7.z.92"
        r `shouldBe` Just (SemVer 1 0 0
                          [ NOSS "x"
                          , NOSI 7
                          , NOSS "z"
                          , NOSI 92] [])
    describe "Complex semver" $ do
      it "parsed semver with release and metadata" $ do
        let r = maybeSuccess $ psv "1.0.0-gamma+002"
        r `shouldBe` Just (SemVer 1 0 0
                          [ NOSS "gamma" ]
                          [ NOSI 2 ] )
      it "prased complex metadata" $ do
        let r = maybeSuccess $ psv "1.0.0-beta+oof.sha.41af286"
        r `shouldBe` Just (SemVer 1 0 0
                          [ NOSS "beta" ]
                          [ NOSS "oof"
                          , NOSS "sha"
                          , NOSS "41af286"])
