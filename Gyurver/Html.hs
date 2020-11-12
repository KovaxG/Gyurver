module Gyurver.Html where

import Prelude hiding (showList)
import qualified Data.List as List

data Document = Document [Html] [Html]

instance Show Document where
  show (Document head body) =
    unlines
      ["<!DOCTYPE html>"
      , "<html>"
      , "<head>"
      , showTags head
      , "</head>"
      , "<body>"
      , showTags body
      , "</body>"
      , "</html>"
      ]

data HtmlType
  = P
  | Title
  | H1
  | A

instance Show HtmlType where
  show htmlType = case htmlType of
    P     -> "p"
    A     -> "a"
    H1    -> "h1"
    Title -> "title"

data Html
  = Container HtmlType [Attribute] [Html]
  | Text String

instance Show Html where
  show html = case html of
    Container htmlType as hs -> make htmlType as hs
    Text str -> str
    where
      make :: HtmlType -> [Attribute] -> [Html] -> String
      make type_ attributes htmls =
        "<" ++ show type_ ++ showSpaces attributes ++ ">"
        ++ showTags htmls
        ++ "</" ++ show type_ ++ ">"

data Attribute = Attribute String String

instance Show Attribute where
  show attribute = case attribute of
    Attribute key val -> key ++ "=\"" ++ val ++ "\""

attribute :: String -> String -> Attribute
attribute = Attribute

type Container = [Attribute] -> [Html] -> Html

p :: Container
p = Container P

title :: Container
title = Container Title

h1 :: Container
h1 = Container H1

a :: Container
a = Container A

text :: String -> Html
text = Text

showTags :: Show a => [a] -> String
showTags = List.intercalate "\n" . map show

showSpaces :: Show a => [a] -> String
showSpaces = concatMap ((++) " " . show)

test :: Document
test =
  Document
    [ title [] [text "Gyurver"] ]
    [ h1 [] [text "Welcome to Gyurver"]
    , p [] [text "Welcome to my site!"]
    , p []
        [ text "Check out my"
        , a [attribute "href" "http://totallysafelink.xyz/cv"] [text "CV"]
        , text "."
        ]
    ]
