{-# LANGUAGE OverloadedStrings #-}

module Main where

import Arg
import Lib

import Data.Either
import Data.Maybe
import Data.Bifunctor

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import qualified Options.Applicative as O
import qualified Network.Haskoin.Keys as H

{-# ANN main("HLint: ignore Use void" :: String )#-}
main :: IO ()
main = do
  opts <- O.execParser $ O.info (O.helper <*> cmd) O.fullDesc

  case opts of
    GenMnemonic {} -> do
      let m = genMnemonic opts
      putStrLn $ either id id $ first show m
    GenSeed {} -> do
      let s = genSeed opts
      putStrLn $ either id id $ first show s

{-
  putStr "mnemonic: "
  let mnemonic = "marble child tag office replace lend stem viable damp prize chef credit soda valid idea"
  let passphrase = ""

  putStr "seed: "
  let seed = either (const "") id $
               H.mnemonicToSeed passphrase mnemonic

  putStrLn $ str seed

  let (k, s) = keypairFromSeed seed
  putStrLn "public key:"
  putStrLn $ str k
  putStrLn "private key:"
  putStrLn $ str s

  putStrLn "generic hash:"
  let gh = cryptoGenerichash k "" 20
  putTextLn $ base54Check gh tz1Prefix

  putStrLn "tz public key:"
  putTextLn $ base54Check k edpkPrefix

  putStrLn "tz private key:"
  putTextLn $ base54Check s edskPrefix
-}
  return ()

genMnemonic :: Arg -> Either Err String
genMnemonic (GenMnemonic e) =
    bimap ToMnemonicErr T.unpack $ H.toMnemonic entropy
    where entropy :: H.Entropy
          entropy = BC.pack $ fromMaybe (BC.unpack defaultEntropy) e
genMnemonic _ = Left NoImplement

genSeed :: Arg -> Either Err String
genSeed (GenSeed p s) =
    bimap ToSeedErr str $ H.mnemonicToSeed pwd mnemonic
    where pwd :: H.Passphrase
          pwd = BC.pack $ fromMaybe "" p
          mnemonic :: H.Mnemonic
          mnemonic = T.pack s

{-
genKeyPair :: Arg -> Either Err String
genKeyPair (GenKeyPair s Tezos) = Left NoImplement
genKeyPair (GenKeyPair s Primitive) = do
    where seed :: BC.ByteString
          seed = 
          (k, s) = keypairFromSeed seed

genKeyPair _ = Left NoImplement
-}
