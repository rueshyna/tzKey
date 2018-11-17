{-# LANGUAGE OverloadedStrings #-}

module Main where

import Arg
import Lib

import Data.Maybe
import Data.Either
import Data.Bifunctor
import qualified Test.RandomStrings as R

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
    GenMnemonic e -> do
      r <- R.randomString (R.onlyWith (`elem` hexRange)  R.randomChar8) 32
      let ent = fromMaybe r e
      let m = genMnemonic ent
      putStrLn $ either show id m
    GenKeyPair p m f -> do
      let pwd = BC.pack <$> p
      let mnemonic = T.pack m
      let eSeed = genSeed pwd mnemonic
      let eKp = keypairFromSeed <$> eSeed
      let epKp = opPair str <$> eKp
      let etKp = opPair T.unpack
               . bimap (base54Check edpkPrefix) (base54Check edskPrefix)
               <$> eKp
      let etH = first
                  ( T.unpack. base54Check tz1Prefix
                  . \k -> cryptoGenerichash k "" 20)
               <$> eKp

      let out = case f of
                  Primitive -> either show kpOut epKp
                  Tezos -> either show kpOut etKp
                        ++ either show wOut etH

      putStrLn out

wOut :: (String, a) -> String
wOut (w, _) = "wallet address\n" ++ w

kpOut :: (String, String) -> String
kpOut (pk, sk) = "public key\n" ++ pk
               ++ "\n\n"
               ++ "private key\n" ++ sk
               ++ "\n\n"
