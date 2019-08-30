module Semver
  ( SemVer (..)
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
parseNumberOrString = try (NOSI . read <$> some digit) <|> (NOSS <$> some alphaNum)

parseRelease :: Parser Release
parseRelease = many parseNumberOrString

parseMetadata :: Parser Metadata
parseMetadata = many parseNumberOrString

parseSemVer :: Parser SemVer
parseSemVer =
  SemVer <$> parseMajor <*> parseMinor <*> parsePatch <*> parseRelease <*>
  parseMetadata
