{-# LANGUAGE TemplateHaskell #-}

module Compiler.Types where

import Control.Lens

data SymbState = SymbState
  { _argsWanted :: Int,
    _stackLevel :: Int
  }
  deriving (Show)

makeLenses ''SymbState
