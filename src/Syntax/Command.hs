module Syntax.Command (
  Command(..)
) where

import Context

data Command = Quit | Assume Context
