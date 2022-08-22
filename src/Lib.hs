{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib
  ( someFunc,
  )
where

import Control.Applicative (Applicative (liftA2))
import Control.Lens
import Control.Monad.State.Strict (MonadState (get), MonadTrans (lift), StateT (runStateT), liftIO)
import Data.List (singleton)
import Data.String (IsString)
import Data.Tuple (swap)
import Debug.Trace
import Liftable
import Text.RawString.QQ
import Types

push :: FV -> Interpreter ()
push v = stack %= (v :)

collectBrackets :: Char -> Char -> Interpreter () -> Interpreter ()
collectBrackets opening closing interpret =
  do
    cmds <- use commands
    case collect cmds of
      (bef, aft) -> do
        commands .= bef
        interpret
        commands .= aft
  where
    --
    collect [] = error $ "Unclosed bracket " ++ [opening]
    collect (c : cmds)
      | c == FCmd [closing] = ([], cmds)
      -- collect (c:cmds) | c == opening = []
      | otherwise = case collect cmds of (bef, aft) -> (c : bef, aft)

readers :: [(Char, String -> Interpreter ())]
readers =
  [ ('#', push . FI . read),
    ('[', \[] -> collectBrackets '[' ']' (use commands >>= push . FBlock)),
    ('(', \[] -> collectBrackets '(' ')' (return ())),
    (':', \[] -> collectBrackets ':' ';' (use commands >>= \((FCmd name) : xs) -> defs %= ((FCmd name, FBlock xs) :)))
  ]

getHead :: Interpreter FV
getHead = stack %%= (\(x : xs) -> (x, xs))

builtins :: [(FCmd, Interpreter ())]
builtins =
  [ ("dup", liftLst (\(x : xs) -> (x : x : xs))),
    ("swap", liftLst (\(a : b : xs) -> (b : a : xs))),
    ("drop", liftLst tail),
    ("+", liftOp2 @Int (+)),
    ("mod", liftOp2 @Int $ flip mod),
    ("eq?", liftOp2 @FV (==)),
    ("print", getHead >>= liftIO . print),
    ("dump-stack", use stack >>= liftIO . print),
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
  -- get >>= liftIO . print
  cmds <- use commands
  case cmds of
    [] -> return ()
    (FCmd cmd : cmds) -> do
      commands .= cmds

      -- st <- use stack
      -- liftIO $ putStrLn $ show st
      -- liftIO $ putStr $ cmd ++ " -> "

      case lookup (head cmd) readers of
        Just reader -> reader (tail cmd) >> run
        Nothing ->
          case lookup (FCmd cmd) builtins of
            Just f -> f >> run
            Nothing -> do
              dfs <- use defs
              case lookup (FCmd cmd) dfs of
                Just (FBlock xs) ->
                  do
                    cont <- commands %%= (,xs)
                    run
                    commands .= cont
                    run
                Just v -> error ("can't run: " ++ cmd ++ " := " ++ show v)
                Nothing -> error ("unknown command: " ++ cmd)

runCode :: String -> IO [FV]
runCode val = _stack . snd <$> runStateT run defaultState {_commands = map FCmd $ words val}

someFunc = do
  v <-
    runCode
      [r|

: divides? ( n-b ) mod #0 eq? ;

 #0 #10 [
    #1 +

    dup #3 divides?
    [ dup #5 divides? ] dip
    
    ||

    dump-stack


    drop
    ( drop )



  ] times

  #2 #1 divides? 

  |]
  putStrLn ""
  putStrLn ""
  putStrLn ""
  print v
