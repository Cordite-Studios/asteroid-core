{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

module Solar.Asteroid.Core.Value.Basics where

-- base
import Prelude(String,(.),Either(..),Bool(..),Double,Float,Integer,Char)
import Data.Int
import Data.Word

-- text
import qualified Data.Text as T
-- bytestring
import qualified Data.ByteString as B

-- local
import qualified Solar.Asteroid.Core.Valuable as V
import qualified Solar.Asteroid.Core.Value.BasicDerive as BD
import qualified Solar.Asteroid.Core.Valuable.Derive as VD

BD.deriveValuable ''Int
BD.deriveValuable ''Int8
BD.deriveValuable ''Int16
BD.deriveValuable ''Int32
BD.deriveValuable ''Int64
BD.deriveValuable ''Integer
BD.deriveValuable ''Char
BD.deriveValuable ''Double
BD.deriveValuable ''Float
BD.deriveValuable ''Word8
BD.deriveValuable ''Word16
BD.deriveValuable ''Word32
BD.deriveValuable ''Word64

VD.useSerializeInstance ''B.ByteString
VD.deriveTextFromBinary ''B.ByteString ''String
VD.deriveAesonFromText ''B.ByteString
instance V.ValuableStr B.ByteString where

VD.useSerializeInstance ''String
VD.useAesonInstance ''String
instance V.TextValuable String String where
    encodeAsText = T.pack
    decodeFromText = Right . T.unpack
instance V.ValuableStr String  where

VD.useSerializeInstance ''Bool
VD.useAesonInstance ''Bool
VD.deriveTextFromAeson ''Bool
instance V.ValuableStr Bool where

