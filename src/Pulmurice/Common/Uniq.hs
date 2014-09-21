-----------------------------------------------------------------------------
-- |
-- Module      :  Pulmurice.Common.Uniq
-- Copyright   :  2014 © Futurice OY, Oleg Grenrus
-- License     :  MIT (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
-- Stability   :  experimental
-- Portability :  non-portable (GHC only)
--
-- 32 bytes, 256 bits long random string.
--
----------------------------------------------------------------------------
module Pulmurice.Common.Uniq
  ( Uniq
  , toQCGen
  , randomUniq
  , fromString
  ) where

import Control.Applicative as A
import Data.Aeson
import Data.Attoparsec.ByteString
import Data.Bits
import Data.ByteString as B (ByteString, length)
import Data.ByteString.Base16 as Base16 (encode, decode)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Word
import System.Entropy
import Data.Text as T
import System.Random.TF.Gen
import Test.QuickCheck.Random

uniqLength :: Int
uniqLength = 32

-- | Uniq is 32 bytes long ByteString
newtype Uniq = Uniq B.ByteString
  deriving (Eq, Ord)


instance Show Uniq where
  show (Uniq h) = T.unpack . decodeUtf8 . Base16.encode $ h

anyWord16 :: Parser Word64
anyWord16 = f <$> anyWord8 <*> anyWord8
  where f a b = let a' = fromIntegral a
                    b' = fromIntegral b
                 in a' `shiftL` 8 .|. b'

anyWord32 :: Parser Word64
anyWord32 = f <$> anyWord16 <*> anyWord16
  where f a b = let a' = fromIntegral a
                    b' = fromIntegral b
                 in a' `shiftL` 16 .|. b'

anyWord64 :: Parser Word64
anyWord64 = f <$> anyWord32 <*> anyWord32
  where f a b = let a' = fromIntegral a
                    b' = fromIntegral b
                 in a' `shiftL` 32 .|. b'

anyQuadWord64 :: Parser (Word64, Word64, Word64, Word64)
anyQuadWord64 = (,,,) <$> anyWord64 <*> anyWord64 <*> anyWord64 <*> anyWord64

toQCGen :: Uniq -> QCGen
toQCGen (Uniq u) = QCGen $ seedTFGen seed
  where result = parse anyQuadWord64 u
        seed   = case result of
                   Done _ v -> v
                   _        -> (0, 0, 0, 0)

randomUniq :: IO Uniq
randomUniq = Uniq <$> getEntropy uniqLength

instance ToJSON Uniq where
  toJSON (Uniq u) = toJSON $ decodeUtf8 $ Base16.encode u

instance FromJSON Uniq where
  parseJSON (String text) = fromText text
  parseJSON _ = A.empty

fromText :: Alternative f => T.Text -> f Uniq
fromText text
  | B.length decoded /= uniqLength * 2 || B.length raw /= uniqLength = A.empty
  | otherwise                                                        = pure $ Uniq raw
  where decoded  = encodeUtf8 text
        (raw, _) = Base16.decode decoded

fromString :: Alternative f => String -> f Uniq
fromString = fromText . T.pack
