{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Solar.Asteroid.Core.Valuable.Derive where

-- base
import Prelude(String,(.),show,Either(..),return)
import Text.Read
import Data.Monoid

-- template-haskell
import Language.Haskell.TH

-- bytestring
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
-- text
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
-- base64-bytestring
import qualified Data.ByteString.Base64.URL as B64
-- aeson
import qualified Data.Aeson as A
-- cereal
import qualified Data.Serialize as S

-- local
import qualified Solar.Asteroid.Core.Valuable as V

swapByteStringToText :: B.ByteString -> T.Text
swapByteStringToText = TE.decodeUtf8 . B64.encode

swapTextToByteString :: T.Text -> B.ByteString
swapTextToByteString = B64.decodeLenient . TE.encodeUtf8

deriveBinaryFromText :: Name -> Name -> Q [Dec]
deriveBinaryFromText val err = [d|
    instance V.BinaryValuable $(conT val) $(conT err) where
        encodeAsByteString = swapTextToByteString . V.encodeAsText
        decodeFromByteString = V.decodeFromText . swapByteStringToText
    |]

deriveTextFromBinary :: Name -> Name -> Q [Dec]
deriveTextFromBinary val err = [d|
    instance V.TextValuable $(conT val) $(conT err) where
        encodeAsText = swapByteStringToText  . V.encodeAsByteString
        decodeFromText = V.decodeFromByteString . swapTextToByteString
    |]

deriveAesonFromText :: Name -> Q [Dec]
deriveAesonFromText val = [d|
    instance V.AesonValuable $(conT val) where
        encodeAsAeson = A.toJSON . V.encodeAsText
        parseAsAeson v = do
            t <- A.parseJSON v
            case V.decodeFromText t of
                Left _ -> mempty
                Right t' -> return t'
    |]

deriveBinaryFromAeson :: Name -> Q [Dec]
deriveBinaryFromAeson val = [d|
    instance V.BinaryValuable $(conT val) String where
        encodeAsByteString = BL.toStrict . A.encode
        decodeFromByteString v = A.eitherDecodeStrict v
    |]

deriveTextFromAeson :: Name -> Q [Dec]
deriveTextFromAeson val = [d|
    instance V.TextValuable $(conT val) String where
        encodeAsText = TE.decodeUtf8 . BL.toStrict . A.encode
        decodeFromText v = A.eitherDecodeStrict (TE.encodeUtf8 v)
    |]

useAesonInstance :: Name -> Q [Dec]
useAesonInstance val = [d|
    instance V.AesonValuable $(conT val) where
        encodeAsAeson = A.toJSON
        parseAsAeson = A.parseJSON
    |]

useSerializeInstance :: Name -> Q [Dec]
useSerializeInstance val = [d|
    instance V.BinaryValuable $(conT val) String where
        encodeAsByteString = S.encode
        decodeFromByteString = S.decode
    |]

useReadShowInstance :: Name -> Q [Dec]
useReadShowInstance val = [d|
    instance V.TextValuable $(conT val) String where 
        encodeAsText = T.pack . show
        decodeFromText = readEither . T.unpack
    |]

