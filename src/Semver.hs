module Semver
  ( SemVer (..)
  , NumberOrString (..)
  , parseSemVer
  , someFunc
  ) where

import Control.Applicative
import Text.Trifecta

someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)

data NumberOrString
 = NOSS String
 | NOSI Integer
 deriving (Eq, Show)

instance Ord NumberOrString where
  compare (NOSI _) (NOSS _) = GT
  compare (NOSS _) (NOSI _) = LT
  compare (NOSS x) (NOSS y) = compare x y
  compare (NOSI x) (NOSI y) = compare x y

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

instance Ord SemVer where
  compare (SemVer majorX minorX patchX releaseX _) (SemVer majorY minorY patchY releaseY _) =
    compare majorX majorY <> compare minorX minorY <> compare patchX patchY <>
    compare (length releaseY) (length releaseX) <>
    foldr1 (<>) (zipWith compare releaseX releaseY)

parseSemVerNumber :: Parser Integer
parseSemVerNumber = do
  d <- some digit
  if length d > 1 && head d == '0'
    then fail "Version number can't have leading zeroes"
    else return $ read d

parseMajor :: Parser Major
parseMajor =  parseSemVerNumber <* char '.'

parseMinor :: Parser Minor
parseMinor =  parseSemVerNumber <* char '.'

parsePatch :: Parser Patch
parsePatch =  parseSemVerNumber

parseNumberOrString :: Parser NumberOrString
parseNumberOrString =
  try (NOSI . read <$> some digit <* notFollowedBy letter) <|>
  (NOSS <$> some alphaNum)

parseSemVerNumberOrString :: Parser NumberOrString
parseSemVerNumberOrString =
  try (NOSI <$> parseSemVerNumber <* notFollowedBy letter) <|>
  (NOSS <$> some alphaNum)

parseDotSeparated :: Parser NumberOrString -> Parser [NumberOrString]
parseDotSeparated p = do
  nos <- p
  tryNext nos <|> return [nos]
  where
    tryNext nos = try (char '.') *> runNext nos
    runNext nos = (nos :) <$> parseDotSeparated p


parseRelease :: Parser Release
parseRelease = try (char '-') *> parseDotSeparated parseSemVerNumberOrString <|> return []

parseMetadata :: Parser Metadata
parseMetadata = try (char '+') *> parseDotSeparated parseNumberOrString <|> return []

parseSemVer :: Parser SemVer
parseSemVer =
  SemVer <$> parseMajor <*> parseMinor <*> parsePatch <*> parseRelease <*>
  parseMetadata
