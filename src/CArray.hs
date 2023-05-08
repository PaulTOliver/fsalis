module CArray
  ( CArray(..)
  , (!@)
  , (/@)
  ) where

import Data.Array.Diff (DiffArray, (!), (//), bounds)
import Data.Word (Word32)

-- | ...
newtype CArray a =
  CArray (DiffArray Word32 a)
  deriving (Show)

-- | ...
(!@) :: CArray a -> Word32 -> a
(!@) (CArray xs) i = xs ! _arrWix xs i

-- | ...
(/@) :: CArray a -> Word32 -> a -> CArray a
(/@) (CArray xs) i v = CArray $ xs // [(_arrWix xs i, v)]

-- | ...
_arrWix :: DiffArray Word32 a -> Word32 -> Word32
_arrWix = flip mod . snd . bounds
