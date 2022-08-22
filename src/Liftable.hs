module Liftable where

import Control.Lens
import Types

class LiftableVal v where
  liftVal :: v -> FV
  unliftVal :: FV -> v

instance LiftableVal Int where
  liftVal = FI
  unliftVal (FI i) = i
  unliftVal v = error ("??? " ++ show v)

instance LiftableVal FV where
  liftVal = id
  unliftVal = id

instance LiftableVal Bool where
  liftVal = FB
  unliftVal (FB i) = i
  unliftVal v = error ("??? " ++ show v)

liftOp :: (LiftableVal a, LiftableVal b) => (a -> b) -> Interpreter ()
liftOp f = stack . _head %= (liftVal . f . unliftVal)

liftOp2 :: (LiftableVal a, LiftableVal b, LiftableVal c) => (a -> b -> c) -> Interpreter ()
liftOp2 f = stack %= \(a : b : xs) -> (liftVal $ f (unliftVal b) (unliftVal a)) : xs

liftLst :: ([FV] -> [FV]) -> Interpreter ()
liftLst f = stack %= f
