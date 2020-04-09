module Gyurver.Gyurror where

data Gyurror 
  = FailedReceive
  | FailedParse String
  deriving (Show)