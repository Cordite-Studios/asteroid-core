{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Solar.Asteroid.Core.Valuable.Class where

-- base
import Prelude(Either(..),String)

-- bytestring
import qualified Data.ByteString as B
-- text
import qualified Data.Text as T
-- aeson
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT

class TextValuable val err | val -> err where
    encodeAsText :: val -> T.Text
    decodeFromText :: T.Text -> Either err val

class BinaryValuable val err | val -> err where
    encodeAsByteString :: val -> B.ByteString
    decodeFromByteString :: B.ByteString -> Either err val

class AesonValuable val where
    encodeAsAeson :: val -> A.Value
    parseAsAeson :: A.Value -> AT.Parser val

class
    ( TextValuable val String
    , BinaryValuable val String
    , AesonValuable val 
    ) => ValuableStr val where

class
    ( TextValuable val err
    , BinaryValuable val err
    , AesonValuable val 
    ) =>
    Valuable val err | val -> err where

instance ValuableStr val => Valuable val String where