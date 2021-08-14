module Gyurver.Gyurror where

import Data.Text (Text)

data Gyurror
  = FailedReceive
  | FailedParse Text
  deriving (Show)