{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module HW02.LogAnalysis where

import StrippedPrelude
import HW02.Log
import Text.Read

parseMessage :: String -> LogMessage
parseMessage line = pm $ words line
  where
    pm ("I":ts:msg)    = LogMessage Info              (read ts) $ unwords msg
    pm ("W":ts:msg)    = LogMessage Warning           (read ts) $ unwords msg
    pm ("E":le:ts:msg) = LogMessage (Error $ read le) (read ts) $ unwords msg
    pm _               = Unknown line

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _)            t            = t
insert lm                     Leaf         = Node Leaf lm Leaf
insert lm@(LogMessage _ ts _) (Node l m r) = 
  case m of
    Unknown _             -> Node l m r
    LogMessage _ nodeTS _ ->
      if ts < nodeTS
        then Node (insert lm l) m r
        else Node l             m (insert lm r)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map toMsg . filter isErr50 . inOrder . build
  where
    isErr50 (LogMessage (Error l) _ _) = l >= 50
    isErr50 _                          = False

    toMsg (Unknown msg)        = msg
    toMsg (LogMessage _ _ msg) = msg
