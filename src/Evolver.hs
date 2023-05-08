module Evolver
  ( EvoState(..)
  , evoST
  ) where

import Control.Monad.State (State, state)
import Data.Bits ((.<<.), (.>>.), (.^.))
import Data.Word (Word32)

-- | ...
data EvoState =
  EvoState Word32 Word32 Word32 Word32
  deriving (Show)

-- | ...
evoST :: State EvoState Word32
evoST =
  state $ \(EvoState s0 s1 s2 s3) ->
    let i0 = s3 .^. s3 .<<. 11
        i1 = i0 .^. i0 .>>. 8
        i2 = i1 .^. s0
        i3 = i2 .^. s0 .>>. 19
     in (i3, EvoState i3 s0 s1 s2)
