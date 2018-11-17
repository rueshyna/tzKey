{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc

import qualified Data.HexString as X
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as SI
import qualified Data.ByteString.Unsafe as SU
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Short as BS

import qualified Network.Haskoin.Keys as H
import qualified Network.Haskoin.Crypto as HC
import qualified Network.Haskoin.Address.Base58 as H

import Data.Serialize as S
import Data.HexString as X
import Data.Word

import System.IO.Unsafe

data Err = NoImplement
         | ToMnemonicErr String
         | ToSeedErr String
         deriving (Show, Eq)

putTextLn :: T.Text -> IO ()
putTextLn = putStrLn . T.unpack

hex :: B.ByteString -> B.ByteString
hex = X.toBytes . X.hexString

str :: B.ByteString -> String
str = T.unpack . X.toText . X.fromBytes

tz1Prefix :: B.ByteString
tz1Prefix = BC.pack "\6\161\159"

edpkPrefix :: B.ByteString
edpkPrefix = BC.pack "\13\15\37\217"

edskPrefix :: B.ByteString
edskPrefix = BC.pack "\43\246\78\7"

defaultEntropy :: B.ByteString
defaultEntropy = hex "7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f"

sha256' :: B.ByteString -> B.ByteString
sha256' =  S.encode . HC.sha256

base54Check :: B.ByteString  -- ^ payload
            -> B.ByteString  -- ^ version byte
            -> T.Text
base54Check p v = H.encodeBase58 target
    where verPayload = B.append v p
          checksum = B.take 4 . sha256' . sha256'
          target = B.append verPayload $ checksum verPayload

keypairFromSeed :: B.ByteString -> (B.ByteString, B.ByteString)
keypairFromSeed seed =
    unsafePerformIO $ do
          pk <- SI.mallocByteString cryptoSignPUBLICKEYBYTES
          sk <- SI.mallocByteString cryptoSignSECRETKEYBYTES
          _ <- SU.unsafeUseAsCString seed $ \pseed -> do
            _ <- withForeignPtr pk $ \ppk -> do
              _ <- withForeignPtr sk $ \psk -> do
                _ <- c_crypto_sign_seed_keypair_FFI ppk psk pseed
                return ()
              return ()
            return ()
          return (SI.fromForeignPtr pk 0 cryptoSignPUBLICKEYBYTES,
                  SI.fromForeignPtr sk 0 cryptoSignSECRETKEYBYTES)

cryptoGenerichash :: B.ByteString  -- ^ intput
                  -> B.ByteString  -- ^ key
                  -> Int           -- ^ length of genericHash
                  -> B.ByteString
cryptoGenerichash input key len =
    unsafePerformIO $ do
      out <- SI.mallocByteString len
      _ <- SU.unsafeUseAsCString input $ \pInput -> do
        _ <- SU.unsafeUseAsCString key $ \pKey -> do
          _ <- withForeignPtr out $ \pOut -> do
            _ <- c_crypto_generichash_FFI
                  pOut
                  (int2CSize len)
                  pInput
                  (int2CLong $ BC.length input)
                  pKey
                  (int2CSize $ BC.length key)
            return ()
          return ()
        return ()
      return $ SI.fromForeignPtr out 0 len


int2CLong :: Int -> CLong
int2CLong = fromInteger . toInteger

int2CSize :: Int -> CSize
int2CSize = fromInteger . toInteger

foreign import ccall "crypto_sign_seed_keypair" c_crypto_sign_seed_keypair_FFI
     :: Ptr Word8
     -> Ptr Word8
     -> Ptr CChar
     -> IO CInt

foreign import ccall "crypto_generichash" c_crypto_generichash_FFI
     :: Ptr Word8  -- ^ out
     -> CSize      -- ^ outlen
     -> Ptr CChar  -- ^ in
     -> CLong      -- ^ inlen
     -> Ptr CChar  -- ^ key
     -> CSize      -- ^ keylen
     -> IO CInt

cryptoSignSECRETKEYBYTES :: Int
cryptoSignSECRETKEYBYTES = 64

cryptoSignPUBLICKEYBYTES :: Int
cryptoSignPUBLICKEYBYTES = 32

