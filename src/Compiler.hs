{-# LANGUAGE QuasiQuotes #-}

module Compiler where

import Compiler.Types
import Control.Lens
import Control.Monad.State.Strict (StateT (runStateT), when)
import Text.RawString.QQ (r)

initialSymbState :: SymbState
initialSymbState = SymbState 0 0

symbExecute :: [String] -> StateT SymbState IO ()
symbExecute [] = return ()
symbExecute ("dup" : rest) =
  do
    sl <- use stackLevel
    when (sl <= 0) $ argsWanted %= (+ 1)
    stackLevel %= (+ 1)
    symbExecute rest
symbExecute ("#0" : rest) =
  do
    stackLevel %= (+ 1)
    symbExecute rest
symbExecute (w : rest) = return ()

compile :: [String] -> IO ()
compile code = runStateT (symbExecute code) initialSymbState >>= print

someFunc :: IO ()
someFunc =
  compile $
    words
      [r|
dup #0 swap                (  n (m:=#0)  n )
  [ 
    dup #10 mod            (  n m        n (n%10) )
    swap [ + ] dip         (  n (m+n%10) n )
    #10 [ / ] sip swap [ * ] dip  
                           (  n ((m+n%10) * 10) (n/10)  )
    dup #0 eq? not
  ] while
  drop #10 / eq?
  |]