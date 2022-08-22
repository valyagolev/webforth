{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib where

import Control.Applicative (Applicative (liftA2))
import Control.Lens
import Control.Monad.State.Strict (MonadState (get), MonadTrans (lift), StateT (runStateT), liftIO, void)
import Data.Char (isAsciiLower, isDigit, ord)
import Data.List (singleton)
import qualified Data.Map.Strict as M
import Data.String (IsString)
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Tuple (swap)
import Debug.Trace
import Liftable
import Text.RawString.QQ (r)
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

timeMillis :: IO Int
timeMillis = round . (1000 *) <$> getPOSIXTime

builtins :: [(FCmd, Interpreter ())]
builtins =
  [ ("dup", liftLst (\(x : xs) -> (x : x : xs))),
    ("swap", liftLst (\(a : b : xs) -> (b : a : xs))),
    ("drop", liftLst tail),
    ( "dip",
      do
        FBlock b <- getHead
        v <- getHead
        scheduleBlock $ b ++ [FPush v]
    ),
    ( "sip",
      do
        FBlock b <- getHead
        v <- head <$> use stack
        scheduleBlock $ b ++ [FPush v]
    ),
    --
    ("push", getHead >>= \v -> (retStack %= (v :))),
    ("pop", retStack %%= (\(x : xs) -> (x, xs)) >>= push),
    --
    ("+", liftOp2 @Int (+)),
    ("-", liftOp2 @Int (-)),
    ("*", liftOp2 @Int (*)),
    ("/", liftOp2 @Int div),
    ("mod", liftOp2 @Int mod),
    ("eq?", liftOp2 @FV (==)),
    ("||", liftOp2 (||)),
    ("not", liftOp not),
    ("<", liftOp2 @Int (<)),
    (">", liftOp2 @Int (>)),
    --
    ("print", getHead >>= liftIO . print),
    ("dump-stack", use stack >>= liftIO . print),
    --
    ("now!", liftIO timeMillis >>= push . FI),
    --
    ("call", getHead >>= \(FBlock vs) -> scheduleBlock vs),
    --
    ( "choose",
      do
        FBlock b1 <- getHead
        FBlock b2 <- getHead
        FB c <- getHead
        scheduleBlock $ if c then b2 else b1
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
        scheduleBlock bl

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

scheduleBlock :: [FCmd] -> Interpreter ()
scheduleBlock xs = do
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
          case lookup (FCmd cmd) dfs of
            Just (FBlock xs) ->
              scheduleBlock xs
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

: divides? ( n n - b )   mod #0 eq? ;
: if       ( b bl - )    [ ] choose ;
: while    ( bl - )      dup [ call ] dip swap [ while ] [ drop ] choose ;
: forever  ( bl - )      dup [ call ] dip forever ;
: loop     ( mk bl - )   swap [ forever ] escapable ;

: time     ( bl - )      now! [ call ] dip now! swap - #:profile print print ;

|]

someFunc' :: IO ()
someFunc' = do
  v <-
    runCode
      [r|

: ispalin ( n - ) 
  dup #0 swap       (  n (m:=#0)  n )
  [ 
    dup #10 mod     (  n m        n (n%10) )
    swap [ + ] dip  (  n (m+n%10) n )
    #10 [ / ] sip swap [ * ] dip  
                    (  n ((m+n%10) * 10) (n/10)  )
    dup #0 eq? not
  ] while
  drop #10 / eq? ;

[ ispalin ] compile 
(
#123 ispalin print
#12321 ispalin print
#123321 ispalin print

[

#0 #100 #100 [ #1 +
  dup
  dup #200 swap -
  [

    ~abab * dup ispalin 
    [
      ~adadbc
      <
      [ swap ] if
      dump-stack
      ~acdb
      ] if
    drop

    #1 +
  ] times
  drop
] times drop

] time )

  |]
  putStrLn ""
  putStrLn ""
  putStrLn "-----"
  print v
