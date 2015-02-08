{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Solar.Asteroid.Core.Value.BasicDerive where

-- base
import Prelude((++),String,return)

-- template-haskell
import Language.Haskell.TH

-- local
import qualified Solar.Asteroid.Core.Valuable as V
import qualified Solar.Asteroid.Core.Valuable.Derive as VD

deriveValuable :: Name -> Q [Dec]
deriveValuable name = do
    readShow <- VD.useReadShowInstance name
    serialized <- VD.useSerializeInstance name
    aesonized <- VD.useAesonInstance name
    final <- [d|instance V.Valuable $(conT name) String where |]
    return (readShow ++ serialized ++ aesonized ++ final)
