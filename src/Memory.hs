module Memory
  ( Memory(..)
  , memInstMask
  , memFlagMask
  , memGetInst
  , memSetInst
  ) where

import Data.Bits ((.&.), (.|.))
import Data.Word (Word32, Word8)

import CArray (CArray, (!@), (/@))
import Instset (Instruction, instFrom, instTo)

-- | ...
newtype Memory =
  Memory (CArray Word8)
  deriving (Show)

-- | ...
memInstMask :: Word8 -> Word8
memInstMask = (.&.) 0x0f

-- | ...
memFlagMask :: Word8 -> Word8
memFlagMask = (.&.) 0xf0

-- | ...
memGetInst :: Memory -> Word32 -> Instruction
memGetInst (Memory m) = instFrom . memInstMask . (!@) m

-- | ...
memSetInst :: Memory -> Word32 -> Instruction -> Memory
memSetInst (Memory m) a =
  Memory . (m /@ a) . (.|.) (m !@ a) . memFlagMask . instTo
