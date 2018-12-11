{-# LANGUAGE OverloadedStrings #-}
module Level06.Conf.File where

import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8 as C8

import           Data.Text                  (Text, pack)

import           Data.Bifunctor             (first)
import           Data.Monoid                (Last (Last))

import           Control.Exception          (try, IOException)

import qualified Data.Attoparsec.ByteString as AB

import           Waargonaut                 (Json, parseWaargonaut)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Decode.Error    (DecodeError (ParseFailed))

import Level06.AppM
import           Level06.Types              (ConfigError(..),
                                             PartialConf (PartialConf), partialConfDecoder)
-- $setup
-- >>> :set -XOverloadedStrings

-- | The configuration file is in the JSON format, so we need to write a
-- 'waargonaut' 'Decoder' to go from JSON to our 'PartialConf'.
--
-- Update these tests when you've completed this function.
--
-- >>> readConfFile "badFileName.no"
-- Left (undefined "badFileName.no: openBinaryFile: does not exist (No such file or directory)")
-- >>> readConfFile "files/test.json"
-- Right "{\n  \"foo\": 33\n}\n"
--
readConfFile
  :: FilePath
  -> IO (Either ConfigError ByteString)
readConfFile fp = mapError <$> try readByteStringFromFile
  where
    readByteStringFromFile = C8.pack <$> readFile fp
    mapError = first (MissingConfFile fp)

parseFunc :: ByteString -> Either DecodeError Json
parseFunc = first (ParseFailed . pack . show) . AB.parseOnly parseWaargonaut

-- | Construct the function that will take a ``FilePath``, read it in, decode it,
-- and construct our ``PartialConf``.
parseJSONConfigFile
  :: FilePath
  -> AppM ConfigError PartialConf
parseJSONConfigFile fp = AppM $ (f =<<) <$> (readConfFile fp)
  where
    f bs = first mapError (D.simpleDecode partialConfDecoder parseFunc bs)
    mapError = BadConfFile . fst

-- Go to 'src/Level06/Conf.hs' next.
