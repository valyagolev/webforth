{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib
  ( someFunc,
  )
where

import Control.Applicative (Applicative (liftA2))
import Control.Lens
import Control.Monad.State.Strict (MonadState (get), MonadTrans (lift), StateT (runStateT), liftIO, void)
import Data.Char (isAsciiLower, isDigit, ord)
import Data.List (singleton)
import Data.String (IsString)
import Data.Tuple (swap)
import Debug.Trace
import Liftable
import Text.RawString.QQ
import Types

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
      -- "[ ["   "a b ] c ] d e" -> "[ a b ] c" "d e"
      | c == FCmd [opening] = case collect cmds of
        (inner, aft) -> case collect aft of
          (still, out) -> ((c : inner) ++ (FCmd [closing] : still), out)
      | otherwise = case collect cmds of (bef, aft) -> (c : bef, aft)

readValue "false" = FB False
readValue "true" = FB True
readValue v = FI (read v)

readers :: [(Char, String -> Interpreter ())]
readers =
  [ ('#', push . readValue),
    ('[', \[] -> collectBrackets '[' ']' (use commands >>= push . FBlock)),
    ('(', \[] -> collectBrackets '(' ')' (return ())),
    (':', \[] -> collectBrackets ':' ';' (use commands >>= \((FCmd name) : xs) -> defs %= ((FCmd name, FBlock xs) :))),
    ('~', tilde)
  ]

-- 'abab
tilde :: String -> Interpreter ()
tilde cs = stack %= \xs -> pick xs ++ drop (depth + 1) xs
  where
    is = map c2i cs
    depth = maximum is
    pick xs = map (xs !!) is
    c2i c
      | isDigit c = ord c - ord '0'
      | isAsciiLower c = ord c - ord 'a'
      | otherwise = error "???"

getHead :: Interpreter FV
getHead = stack %%= (\(x : xs) -> (x, xs))

push :: FV -> Interpreter ()
push v = stack %= (v :)

builtins :: [(FCmd, Interpreter ())]
builtins =
  [ ("dup", liftLst (\(x : xs) -> (x : x : xs))),
    ("swap", liftLst (\(a : b : xs) -> (b : a : xs))),
    ("drop", liftLst tail),
    --
    ("push", getHead >>= \v -> (retStack %= (v :))),
    ("pop", retStack %%= (\(x : xs) -> (x, xs)) >>= push),
    --
    ("+", liftOp2 @Int (+)),
    ("mod", liftOp2 @Int $ flip mod),
    ("eq?", liftOp2 @FV (==)),
    ("||", liftOp2 (||)),
    ("<", liftOp2 @Int $ flip (<)),
    (">", liftOp2 @Int $ flip (>)),
    --
    ("print", getHead >>= liftIO . print),
    ("dump-stack", use stack >>= liftIO . print),
    --
    ("call", getHead >>= \(FBlock vs) -> runBlock vs),
    -- ("escape", commands .= []),
    --
    ( "choose",
      do
        FBlock b1 <- getHead
        FBlock b2 <- getHead
        FB c <- getHead
        runBlock $ if c then b2 else b1
    ),
    --  \(St cmds (FBlock cs : xs)) -> return $ St (cs ++ cmds) xs
    ( "times",
      do
        FBlock bl <- getHead
        FI i <- getHead
        commands %= ((concat (replicate i bl)) ++)
    )
  ]

runBlock :: [FCmd] -> Interpreter ()
runBlock xs = do
  cont <- commands %%= (,xs)
  run
  commands .= cont

run :: Interpreter ()
-- run = use stack
-- run st | traceShow st False = error "?"
run = do
  -- get >>= liftIO . print
  cmds <- use commands
  case cmds of
    [] -> return () -- void $ liftIO $ putStrLn "RET"
    (FCmd cmd : cmds) -> do
      commands .= cmds

      -- st <- use stack
      -- st <- get
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
                  runBlock xs >> run
                Just v -> error ("can't run: " ++ cmd ++ " := " ++ show v)
                Nothing -> error ("unknown command: " ++ cmd)

runCode :: String -> IO [FV]
runCode val = _stack . snd <$> runStateT run defaultState {_commands = map FCmd $ words (stdlib ++ val)}

-- #1 #2 [ #3 ] dip
-- #1 | #2
-- #1 #3 | #2
-- #1 #3 #2

stdlib :: String
stdlib =
  [r|

: dip      ( v bl -  v ) swap push call pop ; 
: divides? ( n n - b )   mod #0 eq? ;
: if       ( b bl - )    [ ] choose ;
: while    ( bl - )      dup [ call ] dip swap [ while ] [ drop ] choose ;

|]

someFunc = do
  v <-
    runCode
      [r|


  |]
  putStrLn ""
  putStrLn ""
  putStrLn "-----"
  print v
