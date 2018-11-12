{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Data.Either
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Network.Haskoin.Keys as H
import qualified Network.Haskoin.Address.Base58 as H
import Data.HexString as X
import Data.Serialize.Put

main :: IO ()
main = do
  putStr "mnemonic: "
  let mnemonic = "marble child tag office replace lend stem viable damp prize chef credit soda valid idea"
  let passphrase = ""

  putStr "seed: "
  let seed = either (const "") id $
               H.mnemonicToSeed passphrase mnemonic

  putStrLn $ str seed

  let (k, s) = createKeypairFromSeed' seed
  putStrLn "public key:"
  putStrLn $ str k
  putStrLn "private key:"
  putStrLn $ str s

  putStrLn "generic hash:"
  let gh = cryptoGenerichash k "" 20
  let prefixedGenericHash = B.append tz1Prefix gh
  let firstFourOfDoubleChecksumH = B.take 4 $ sha256_ $ sha256_ prefixedGenericHash
  let prefixedPKhashWithChecksum = B.append prefixedGenericHash firstFourOfDoubleChecksumH
  let pkh = H.encodeBase58 prefixedPKhashWithChecksum 
  putStrLn $ T.unpack pkh

  let prefixedPubKey = B.append edpkPrefix k
  let firstFourOfDoubleChecksumP = B.take 4 $ sha256_ $ sha256_ prefixedPubKey
  let prefixedPubKeyWithChecksum = B.append prefixedPubKey firstFourOfDoubleChecksumP
  let tezosPkString = H.encodeBase58 prefixedPubKeyWithChecksum

  putStrLn "tz public key:"
  putStrLn $ T.unpack tezosPkString

  let prefixedSecKey = B.append edskPrefix s
  let firstFourOfDoubleChecksumS = B.take 4 $ sha256_ $ sha256_ prefixedSecKey
  let prefixedSecKeyWithChecksum = B.append prefixedSecKey firstFourOfDoubleChecksumS
  let tezosSkString = H.encodeBase58 prefixedSecKeyWithChecksum

  putStrLn "tz private key:"
  putStrLn $ T.unpack tezosSkString

  return ()
