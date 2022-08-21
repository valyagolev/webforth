{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib
  ( someFunc,
  )
where

import Data.List (singleton)
import Data.String (IsString)
import Debug.Trace
import Text.RawString.QQ

data FV = FI Int | FS String | FBlock [FCmd]
  deriving (Show, Eq)

newtype FCmd = FCmd String
  deriving (Show, Eq, IsString)

data St = St {_cmd :: [FCmd], _stack :: [FV]}
  deriving (Show)

push :: FV -> St -> St
push v (St _c _s) = St _c (v : _s)

collectBrackets :: Char -> Char -> ([FCmd] -> St -> St) -> St -> St
collectBrackets opening closing f (St cmds s) =
  case collect cmds of
    (bef, aft) -> f bef (St aft s)
  where
    collect [] = error $ "Unclosed bracket " ++ [opening]
    collect (c : cmds)
      | c == FCmd [closing] = ([], cmds)
      -- collect (c:cmds) | c == opening = []
      | otherwise = case collect cmds of (bef, aft) -> (c : bef, aft)

readers :: [(Char, String -> St -> St)]
readers =
  [ ('#', push . FI . read),
    ('[', \[] -> collectBrackets '[' ']' (push . FBlock))
  ]

builtins :: [(FCmd, St -> IO St)]
builtins =
  [ ("dup", \(St cmds (x : xs)) -> return $ St cmds (x : x : xs)),
    ("swap", \(St cmds (a : b : xs)) -> return $ St cmds (b : a : xs)),
    ("+", \(St cmds (FI a : FI b : xs)) -> return $ St cmds (FI (a + b) : xs)),
    ("print", \(St cmds (x : xs)) -> print x >> return (St cmds xs)),
    ("call", \(St cmds (FBlock cs : xs)) -> return $ St (cs ++ cmds) xs),
    ( "times",
      \(St cmds (FBlock cs : FI i : xs)) ->
        return (St (concat (replicate i cs) ++ cmds) xs)
    )
  ]

run :: St -> IO [FV]
run (St [] s) = return s
-- run st | traceShow st False = error "?"
run (St ((FCmd cmd) : cmds) s) =
  case lookup (head cmd) readers of
    Just reader -> run $ reader (tail cmd) $ St cmds s
    Nothing ->
      case lookup (FCmd cmd) builtins of
        Just f -> f (St cmds s) >>= run
        Nothing -> error ("unknown command: " ++ cmd)

runCode :: String -> IO [FV]
runCode val = run (St (map FCmd $ words val) [])

someFunc = do
  runCode
    [r|
      
  #0 #10 [
    #1 +
    dup print
  ] times

  |]
    >>= print
