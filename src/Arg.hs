module Arg where

import Options.Applicative
import Data.Monoid

data Format = Tezos | Primitive
            deriving (Show, Read, Eq)
data Cat = Address | PublicKey | PrivateKey
            deriving (Show, Read, Eq)

data Arg = GenMnemonic
           { entropy    :: Maybe String
           }
         | GenSeed
           { password   :: Maybe String
           , mnemonicSentence :: String
           }
         | GenKeyPair
           { seed       :: Maybe String
           , fmtGenKp   :: Format
           }
         | GenTezosFormat
           { catData    :: String
           , cat        :: Cat }
         | GenBase58Format
           { versionByte :: String
           , payloadByte :: String
           }
         | GenPublic
           { privateKey :: String
           , fmtGenPk   :: Format
           }
         | ValidateMnemonic
           { mnemonicWord :: String }
         | ValidateKeyPair
           { publicKeyV    :: String
           , privateKeyV   :: String
           , fmtVkp       :: Format
           }
         | ValidateTezosFormat
           { key                :: String }
         | ValidateBase58Check
           { base58CheckString  :: String }

cmd :: Parser Arg
cmd = subparser
        ( command "GenMnemonic"
           ( info (helper <*> genMnemonicParser) fullDesc)
        <> command "GenSeed"
           ( info (helper <*> genSeedParser) fullDesc)
        <> command "GenKeyPair"
           ( info (helper <*> genKeyPairParser) fullDesc)
        <> command "GenTezosFormat"
           ( info (helper <*> genTezosFormatParser) fullDesc)
        <> command "genBase58Format"
           ( info (helper <*> genBase58FormatParser) fullDesc)
        <> command "genPublic"
           ( info (helper <*> genPublicParser) fullDesc)
        <> command "validateMnemonic"
           ( info (helper <*> validateMnemonicParser) fullDesc)
        <> command "validateKeyPair"
           ( info (helper <*> validateKeyPairParser) fullDesc)
        <> command "validateTezosFormat"
           ( info (helper <*> validateTezosFormatParser) fullDesc)
        <> command "validateBase58Check"
           ( info (helper <*> validateBase58CheckParser) fullDesc)
        <> help "Use '--help' to see more detail"
        )

genMnemonicParser         :: Parser Arg
genMnemonicParser         = GenMnemonic         <$> entropy_

genSeedParser             :: Parser Arg
genSeedParser             = GenSeed            <$> password_ <*> mnemonicSentence_

genKeyPairParser          :: Parser Arg
genKeyPairParser          = GenKeyPair          <$> seed_ <*> fmtGenKp_

genTezosFormatParser      :: Parser Arg
genTezosFormatParser      = GenTezosFormat      <$> catData_ <*> cat_

genBase58FormatParser     :: Parser Arg
genBase58FormatParser     = GenBase58Format     <$> versionByte_ <*> payloadByte_

genPublicParser           :: Parser Arg
genPublicParser           = GenPublic           <$> privateKey_ <*> fmtGenPk_

validateMnemonicParser    :: Parser Arg
validateMnemonicParser    = ValidateMnemonic    <$> mnemonicWord_

validateKeyPairParser     :: Parser Arg
validateKeyPairParser     = ValidateKeyPair     <$> publicKeyV_ <*> privateKeyV_ <*> fmtVkp_

validateTezosFormatParser :: Parser Arg
validateTezosFormatParser = ValidateTezosFormat <$> key_

validateBase58CheckParser :: Parser Arg
validateBase58CheckParser = ValidateBase58Check <$> base58CheckString_

entropy_ :: Parser (Maybe String)
entropy_ =
    fmtArg
      (optional . strOption)
      "entropy in a multiple of 32 bits"
      'e'
      "ENTROPY"
      "Using [0-9][a-f] to present, For example: f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7"

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
      "Tezos | Primitive"
      'f'
      "FORMAT_GEN_KEYPAIR"
      "Tezos | Primitive"

catData_ :: Parser String
catData_ =
    fmtArg
      strOption
      "can be public key, private key or address of Tezos"
      'c'
      "CAT_DATA"
      "can be public key, private key or address of Tezos"

cat_ :: Parser Cat
cat_ =
    fmtArg
      (\x -> (read :: String -> Cat) <$> strOption x)
      "Address | PublicKey | PrivateKey"
      'k'
      "FORMAT_GEN_KEYPAIR"
      "Address | PublicKey | PrivateKey"

versionByte_ :: Parser String
versionByte_ =
    fmtArg
      strOption
      "version bytes"
      'v'
      "VERSION_BYTE"
      "version bytes"

payloadByte_ :: Parser String
payloadByte_ =
    fmtArg
      strOption
      "payload bytes"
      'v'
      "PAYLOAD_BYTE"
      "payload bytes"

privateKey_ :: Parser String
privateKey_ =
    fmtArg
      strOption
      "private key"
      'p'
      "PRIVATE_KEY"
      "private key"

fmtGenPk_ :: Parser Format
fmtGenPk_ =
    fmtArg
      (\x -> (read :: String -> Format) <$> strOption x)
      "Tezos | Primitive"
      'v'
      "FORMAT_GEN_KEYPAIR"
      "Tezos | Primitive"

mnemonicWord_ :: Parser String
mnemonicWord_ =
    fmtArg
      strOption
        "Mnemonic word"
        'm'
        "MNEMONIC_WORD"
        "Mnemonic word"

privateKeyV_ :: Parser String
privateKeyV_ =
    fmtArg
      strOption
      "private key"
      'p'
      "PRIVATE_KEY_FOR_VALIDATE"
      "private key"

publicKeyV_ :: Parser String
publicKeyV_ =
    fmtArg
      strOption
      "public key"
      'p'
      "PUBLIC_KEY_FOR_VALIDATE"
      "public key"

fmtVkp_ :: Parser Format
fmtVkp_ =
    fmtArg
      (\x -> (read :: String -> Format) <$> strOption x)
      "Tezos | Primitive"
      'v'
      "FORMAT_KEY_PAIR"
      "Tezos | Primitive"

key_ :: Parser String
key_ =
    fmtArg
      strOption
      "key"
      'k'
      "KEY"
      "key"

base58CheckString_ :: Parser String
base58CheckString_ =
    fmtArg
      strOption
      "base58CheckString"
      'b'
      "BASE58_CHECK_STRING"
      "base58CheckString"

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
