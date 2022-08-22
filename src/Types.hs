{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens
import Control.Monad.State.Strict (StateT)
import Data.List (singleton)
import qualified Data.Map.Strict as M
import Data.String (IsString)
import Debug.Trace
import Text.RawString.QQ
import Prelude hiding (words)

data FV = FI !Int | FS !String | FB !Bool | FBlock [FCmd] | FMarker !String
  deriving (Show, Eq)

newtype FCmd = FCmd String
  deriving (Show, Eq, IsString)

data St = St
  { _commands :: [FCmd],
    _stack :: ![FV],
    _defs :: [(FCmd, FV)],
    _retStack :: [FV],
    _localDefs :: M.Map String (FV)
  }
  deriving (Show)

defaultState :: St
defaultState = St [] [] [] [] M.empty

type Interpreter a = StateT St IO a

makeLenses ''St