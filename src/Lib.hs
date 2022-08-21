{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib
  ( someFunc,
  )
where

import Control.Applicative (Applicative (liftA2))
import Control.Lens
import Control.Monad.State.Strict (StateT (runStateT), liftIO)
import Data.List (singleton)
import Data.String (IsString)
import Data.Tuple (swap)
import Debug.Trace
import Liftable
import Text.RawString.QQ
import Types

push :: FV -> Interpreter ()
push v = stack %= (v :)

-- collectBrackets :: Char -> Char -> ([FCmd] -> Interpreter ()) -> Interpreter ()
-- collectBrackets opening closing f (St cmds s) =
--   case collect cmds of
--     (bef, aft) -> f bef (St aft s)
--   where
--     collect [] = error $ "Unclosed bracket " ++ [opening]
--     collect (c : cmds)
--       | c == FCmd [closing] = ([], cmds)
--       -- collect (c:cmds) | c == opening = []
--       | otherwise = case collect cmds of (bef, aft) -> (c : bef, aft)

readers :: [(Char, String -> Interpreter ())]
readers =
  [ ('#', push . FI . read)
  -- ('[', \[] -> collectBrackets '[' ']' (push . FBlock)),
  -- (':', \[] -> collectBrackets ':' ';' _)
  ]

getHead :: Interpreter FV
getHead = stack %%= (\(x : xs) -> (x, xs))

builtins :: [(FCmd, Interpreter ())]
builtins =
  [ ("dup", liftLst (\(x : xs) -> (x : x : xs))),
    ("swap", liftLst (\(a : b : xs) -> (b : a : xs))),
    ("+", liftOp2 @Int (+)),
    ("print", getHead >>= liftIO . print),
    ("call", getHead >>= \(FBlock vs) -> (commands %= (vs ++))),
    --  \(St cmds (FBlock cs : xs)) -> return $ St (cs ++ cmds) xs
    ( "times",
      do
        FBlock bl <- getHead
        FI i <- getHead
        commands %= ((concat (replicate i bl)) ++)
    )
  ]

run :: Interpreter ()
-- run = use stack
-- run st | traceShow st False = error "?"
run = do
  cmds <- use commands
  case cmds of
    [] -> return ()
    (FCmd cmd : cmds) -> do
      commands .= cmds

      case lookup (head cmd) readers of
        Just reader -> reader (tail cmd) >> run
        Nothing ->
          case lookup (FCmd cmd) builtins of
            Just f -> f >> run
            Nothing -> error ("unknown command: " ++ cmd)

runCode :: String -> IO [FV]
runCode val = _stack . snd <$> runStateT run defaultState {_commands = map FCmd $ words val}

someFunc = do
  runCode
    [r|

: divides mod 0 eq? ;

  #0 #10 [
    #1 +

    dup print


  ] times

  |]
    >>= print
