module Types.Common where

data Mode = Dev | Prod deriving (Show, Eq)

data EventMode = Running | Locked | Blocked deriving (Show, Eq)