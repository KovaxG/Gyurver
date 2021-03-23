module Cokkolo2021.Views.Store exposing (..)

import Html exposing (Html, text, h2, h3, br, div)
import Html.Attributes exposing (style, class, src)
import Bootstrap.Grid as Grid
import Bootstrap.Table as Table
import Bootstrap.Card.Block as Block
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Utilities.Spacing as Spacing

import Cokkolo2021.Common exposing (..)

type alias ViewState =
  { user : User
  , items : List Item
  }

type alias Item =
  { index : Int
  , name : String
  , image : String
  , cost : Int
  }

type Message
  = SwitchToDashboard
  | PopulateItems (List Item)

init : User -> ViewState
init user =
  { user = user
  , items = [user.base]
  }

view : ViewState -> Html Message
view state =
  [ [ h2 [] [text "Üzlet"]
    , Button.button
      [ Button.outlineSecondary
      , Button.attrs [ Spacing.m2 ]
      , Button.onClick SwitchToDashboard
      ] [text "Vissza"]
    , br [] []
    , displayImage state.user.base.image 250 250
    ] |> Grid.col []
  , [ Table.table
        { options = [ Table.striped ]
        , thead =
            Table.simpleThead
                [ Table.th [] [text "Elnevezés"]
                , Table.th [] [text "Ár"]
                , Table.th [] []
                ]
        , tbody = List.map (itemToRow state.user) state.items |> Table.tbody []
        }
    ] |> Grid.col []
  ] |> Grid.row []

itemToRow : User -> Item -> Table.Row Message
itemToRow user item =
  [ Table.td [] [displayImage item.image 100 100, text item.name]
  , Table.td [] [text <| String.fromInt item.cost ++ " 💦"]
  , Table.td []
    <| if item.index == user.base.index
       then []
       else if List.member item.index (user.items ++ [user.base.index])
       then [Button.button [Button.primary] [text "Equip"]]
       else [Button.button [Button.success] [text "Buy"]]
  ] |> Table.tr []
