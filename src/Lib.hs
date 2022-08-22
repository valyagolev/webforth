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
import qualified Data.Map.Strict as M
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
    -- liftIO $ print cmds
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

readValue :: [Char] -> FV
readValue (':' : xs) = FMarker xs
readValue "false" = FB False
readValue "true" = FB True
readValue v | all isDigit v = FI (read v)
readValue v = error $ "Can't read: " ++ v

runColon :: String -> Interpreter ()
runColon [] = collectBrackets ':' ';' (use commands >>= \((FCmd name) : xs) -> defs %= ((FCmd name, FBlock xs) :))
runColon mk = do
  val <- use (localDefs . at mk)
  -- get >>= liftIO . print
  case val of
    Just (FBlock bl) -> commands %= (bl ++)
    _ -> error ("not defined marker: :" ++ mk)

readers :: [(Char, String -> Interpreter ())]
readers =
  [ ('#', push . readValue),
    ('[', \[] -> collectBrackets '[' ']' (use commands >>= push . FBlock)),
    ('(', \[] -> collectBrackets '(' ')' (return ())),
    (':', runColon),
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
    ),
    ( "escapable",
      do
        FBlock bl <- getHead
        FMarker mk <- getHead

        last <- localDefs . at mk %%= (,Just $ FBlock [FPush (FMarker mk), FCmd "escape"])
        retStack %= (FBlock [FPush (FMarker mk), FCmd "clearMarker"] :)
        runBlock bl

        return ()
    ),
    ( "escape",
      do
        FMarker mk <- getHead

        commands .= []
        retStack %= dropWhile (/= FBlock [FPush (FMarker mk), FCmd "clearMarker"])

        return ()
    ),
    ("clearMarker", getHead >>= \(FMarker mk) -> (localDefs . at mk) .= Nothing)
  ]

runBlock :: [FCmd] -> Interpreter ()
runBlock xs = do
  cont <- commands %%= (,xs)
  retStack %= (FBlock cont :)

step :: FCmd -> Interpreter ()
step (FPush v) = push v
step (FCmd cmd) = do
  case lookup (head cmd) readers of
    Just reader -> reader (tail cmd)
    Nothing ->
      case lookup (FCmd cmd) builtins of
        Just f -> f
        Nothing -> do
          dfs <- use defs
          case lookup (FCmd cmd) (dfs) of
            Just (FBlock xs) ->
              runBlock xs
            Just v -> error ("can't run: " ++ cmd ++ " := " ++ show v)
            Nothing -> error ("unknown command: " ++ cmd)

run :: Interpreter ()
run = do
  -- get >>= liftIO . print
  cmd <-
    commands %%= \case
      [] -> (Nothing, [])
      (cmd : xs) -> (Just cmd, xs)
  case cmd of
    Nothing -> do
      n <-
        retStack %%= \case
          [] -> (Nothing, [])
          (FBlock bl : vs) -> (Just bl, vs)
      case n of
        Nothing -> return ()
        Just cont -> commands .= cont >> run
    Just cmd -> step cmd >> run

runCode :: String -> IO [FV]
runCode val = _stack . snd <$> runStateT run defaultState {_commands = map FCmd $ words (stdlib ++ val)}

stdlib :: String
stdlib =
  [r|

: dip      ( v bl -  v ) swap push call pop ; 
: divides? ( n n - b )   mod #0 eq? ;
: if       ( b bl - )    [ ] choose ;
: while    ( bl - )      dup [ call ] dip swap [ while ] [ drop ] choose ;

|]

someFunc :: IO ()
someFunc = do
  v <-
    runCode
      [r|

#132132 print

#132132 dup ~ba dump-stack

#0 > dump-stack
[ #999999 print ] if
dump-stack
#66666 print

#13195 #0 #:exit [
  #1 +
  dup print
  [ #1111 print :exit ] call
  #666 print
] escapable

  |]
  putStrLn ""
  putStrLn ""
  putStrLn "-----"
  print v
