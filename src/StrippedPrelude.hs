{-# LANGUAGE NoImplicitPrelude #-}

module StrippedPrelude (
  module Prelude,
  bool
) where

import Prelude hiding ((!!), head, init, last, tail, read, maximum, minimum)

bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True  = t
