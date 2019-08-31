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
    describe "Does not allow leading zeros" $ do
      it "errors if major version has a leading zero" $ do
        let r = maybeSuccess $ psv "01.0.0"
        r `shouldBe` Nothing
      it "parses release with leading zeroes as string" $ do
        let r = maybeSuccess $ psv "1.0.0-01"
        r `shouldBe` Just (SemVer 1 0 0 [NOSS "01"] [])
    describe "Compares semvers" $ do
      it "handles major version difference" $ do
        let big = SemVer 2 1 1 [] []
            little = SemVer 1 1 1 [] []
        big > little `shouldBe` True
      it "handles patch version difference" $ do
        let big = SemVer 1 1 2 [] []
            little = SemVer 1 1 1 [] []
        big > little `shouldBe` True
      it "handles number release difference" $ do
        let big = SemVer 1 1 1 [NOSI 2] []
            little = SemVer 1 1 1 [NOSI 1] []
        big > little `shouldBe` True
      it "handles string release difference" $ do
        let big = SemVer 1 1 1 [NOSS "beta"] []
            little = SemVer 1 1 1 [NOSS "alpha"] []
        big > little `shouldBe` True
      it "handles different number of releases" $ do
        let big = SemVer 1 1 1 [NOSI 2] []
            little = SemVer 1 1 1 [NOSI 1, NOSS "beta"] []
        big > little `shouldBe` True
