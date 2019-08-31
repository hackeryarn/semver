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

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

parseMajor :: Parser Major
parseMajor =  read <$> (some digit <* char '.')

parseMinor :: Parser Minor
parseMinor =  read <$> (some digit <* char '.')

parsePatch :: Parser Patch
parsePatch =  read <$> some digit

parseNumberOrString :: Parser NumberOrString
parseNumberOrString =
  try (NOSI . read <$> some digit <* notFollowedBy letter) <|>
  (NOSS <$> some alphaNum)

parseDotSeparated :: Parser [NumberOrString]
parseDotSeparated = do
  nos <- parseNumberOrString
  tryNext nos <|> return [nos]
  where
    tryNext nos = try (char '.') *> runNext nos
    runNext nos = (nos :) <$> parseDotSeparated


parseRelease :: Parser Release
parseRelease = try (char '-') *> parseDotSeparated <|> return []

parseMetadata :: Parser Metadata
parseMetadata = try (char '+') *> parseDotSeparated <|> return []

parseSemVer :: Parser SemVer
parseSemVer =
  SemVer <$> parseMajor <*> parseMinor <*> parsePatch <*> parseRelease <*>
  parseMetadata
