module Instset
  ( Instruction(..)
  , instFrom
  , instTo
  ) where

-- | ...
data Instruction
  = InstLOOPB
  | InstLOOPE
  | InstIFVAL
  | InstBREAK
  | InstMALLB
  | InstMALLF
  | InstBSWAP
  | InstBSPLT
  | InstVINCR
  | InstVDECR
  | InstVSHFL
  | InstVSHFR
  | InstVPUSH
  | InstVPULL
  | InstVFEED
  | InstWHOLE
  deriving (Enum, Show)

-- | ...
instFrom :: Integral a => a -> Instruction
instFrom = toEnum . fromIntegral

-- | ...
instTo :: Integral a => Instruction -> a
instTo = fromIntegral . fromEnum
