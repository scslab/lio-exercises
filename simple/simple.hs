{-# LANGUAGE DeriveDataTypeable #-}
module Simple where

import Data.Typeable
import LIO

data SimpleLabel = Public | Classified | TopSecret deriving (Eq, Ord, Show, Typeable)

instance Label SimpleLabel where
  x `canFlowTo` y = x <= y
  lub = max
  glb = min


--data SimplePriv = SimplePriv SimpleLabel

--downgrade :: SimplePriv -> SimpleLabel -> SimpleLabel
--downgrade (SimplePriv priv) lbl = ??

