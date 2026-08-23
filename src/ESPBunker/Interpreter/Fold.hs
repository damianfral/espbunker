{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ESPBunker.Interpreter.Fold where

import Control.Monad.Indexed (IxFunctor (..))
import Control.Monad.Indexed.Free (IxFree (..))
import Relude

--------------------------------------------------------------------------------

ifoldFree ::
  (IxFunctor f, Monad n) =>
  (forall i j. f i j (n ()) -> n ()) -> IxFree f k m () -> n ()
ifoldFree _ (Pure _) = pure ()
ifoldFree alg (Free fx) = alg $ imap (ifoldFree alg) fx
