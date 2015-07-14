{-# LANGUAGE NoImplicitPrelude #-}

module StrippedPrelude (
  module Prelude
) where

import Prelude hiding ((!!), head, init, last, tail, read)
