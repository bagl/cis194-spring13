{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}

module HW02.LogAnalysis where

import StrippedPrelude
import HW02.Log            (MessageType(..), LogMessage(..), MessageTree(..))

import Control.Applicative ((<$>), (<*>), pure)
import Data.Maybe          (fromMaybe)
import Text.Read           (readMaybe)

parseMessage :: String -> LogMessage
parseMessage line = fromMaybe (Unknown line) (pm $ words line)
  where
    pm :: [String] -> Maybe LogMessage
    pm ("I":ts:msg)    = LogMessage Info                          <$> readMaybe ts <*> pure (unwords msg)
    pm ("W":ts:msg)    = LogMessage Warning                       <$> readMaybe ts <*> pure (unwords msg)
    pm ("E":le:ts:msg) = LogMessage <$> fmap Error (readMaybe le) <*> readMaybe ts <*> pure (unwords msg)
    pm _               = Nothing

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert m@LogMessage{} Leaf = Node Leaf m Leaf
insert m@(LogMessage _ t _) (Node nodeL nodeM@(LogMessage _ nodeT _) nodeR)
  | t < nodeT = Node (insert m nodeL) nodeM nodeR
  | otherwise = Node nodeL            nodeM (insert m nodeR)
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map toMsg . inOrder . build . filter isErr50
  where
    isErr50 (LogMessage (Error l) _ _) = l >= 50
    isErr50 _                          = False

    toMsg (Unknown msg)        = msg
    toMsg (LogMessage _ _ msg) = msg
