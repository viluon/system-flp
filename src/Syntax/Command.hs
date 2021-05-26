module Syntax.Command (
  Command(..)
) where

import Typechecking.Context

data Command = Quit | Assume Context
             deriving (Eq, Show, Ord)
