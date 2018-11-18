module Arg where

import Options.Applicative
import Data.Monoid

data Format = Tezos | Primitive
            deriving (Show, Read, Eq)

data Arg = GenMnemonic
           { entropy    :: Maybe String
           }
         | GenKeyPair
           { password   :: Maybe String
           , mnemonicSentence :: String
           , fmtGenKp   :: Format
           }

cmd :: Parser Arg
cmd = subparser
        ( command "GenMnemonic"
           ( info (helper <*> genMnemonicParser) fullDesc)
        <> command "GenKeyPair"
           ( info (helper <*> genKeyPairParser) fullDesc)
        <> help "Use '--help' to see more detail"
        )

genMnemonicParser :: Parser Arg
genMnemonicParser = GenMnemonic <$> entropy_

genKeyPairParser  :: Parser Arg
genKeyPairParser  = GenKeyPair  <$> password_ <*> mnemonicSentence_<*> fmtGenKp_

entropy_ :: Parser (Maybe String)
entropy_ =
    fmtArg
      (optional . strOption)
      "entropy in a multiple of 32 bits"
      'e'
      "ENTROPY"
      "Using [0-9][a-f] to present, For example: f7f7. Default will generate 256 bits randomly."

seed_ :: Parser (Maybe String)
seed_ =
    fmtArg
      (optional . strOption)
      "seed"
      's'
      "SEED"
      "seed for generating key pair"

mnemonicSentence_ :: Parser String
mnemonicSentence_ =
    fmtArg
      strOption
      "mnemonic"
      'm'
      "MNEMONIC"
      "mnemonic"

password_ :: Parser (Maybe String)
password_ =
    fmtArg
      (optional . strOption)
      "password"
      'p'
      "PASSWORD"
      "password"

fmtGenKp_ :: Parser Format
fmtGenKp_ =
    fmtArg
      (\x -> (read :: String -> Format) <$> strOption x)
      "format"
      'f'
      "FORMAT_GEN_KEYPAIR"
      "Tezos | Primitive"

fmtArg :: (Mod OptionFields b -> Parser a)
       -> String -- ^ long
       -> Char   -- ^ short
       -> String -- ^ metavar
       -> String -- ^ help
       -> Parser a
fmtArg f l s m h =
       f (  long    l
         <> short   s
         <> metavar m
         <> help    h
         )
