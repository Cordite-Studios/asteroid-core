{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

module Solar.Asteroid.Core.Valuable.Basics where

-- base
import Prelude(String,(.),Either(..),Bool(..),Double,Float,Integer,Char)
import Data.Int
import Data.Word

-- text
import qualified Data.Text as T
-- bytestring
import qualified Data.ByteString as B

-- local
import Solar.Asteroid.Core.Valuable.Class
import Solar.Asteroid.Core.Valuable.Derive

simpleDeriveValuable ''Int
simpleDeriveValuable ''Int8
simpleDeriveValuable ''Int16
simpleDeriveValuable ''Int32
simpleDeriveValuable ''Int64
simpleDeriveValuable ''Integer
simpleDeriveValuable ''Char
simpleDeriveValuable ''Double
simpleDeriveValuable ''Float
simpleDeriveValuable ''Word8
simpleDeriveValuable ''Word16
simpleDeriveValuable ''Word32
simpleDeriveValuable ''Word64

useSerializeInstance ''B.ByteString
deriveTextFromBinary ''B.ByteString ''String
deriveAesonFromText ''B.ByteString
instance ValuableStr B.ByteString where

useSerializeInstance ''String
useAesonInstance ''String
instance TextValuable String String where
    encodeAsText = T.pack
    decodeFromText = Right . T.unpack
instance ValuableStr String  where

useSerializeInstance ''Bool
useAesonInstance ''Bool
deriveTextFromAeson ''Bool
instance ValuableStr Bool where

