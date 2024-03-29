module Cokkolo2021.Views.Store exposing (..)

import Html exposing (Html, text, h2, h3, br, div)
import Html.Attributes exposing (style, class, src, attribute)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Table as Table
import Bootstrap.Card.Block as Block
import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Utilities.Spacing as Spacing
import List.Extra as List

import Json.Encode as Encode exposing (Value)

import Cokkolo2021.Common exposing (..)
import Types.EventState as EventState
import Settings

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
  | BuyItem Int
  | BuySuccess Int
  | BuyFailure
  | EquipItem Int
  | EquipSuccess Int
  | EquipFailure

init : User -> ViewState
init user =
  { user = user
  , items = [user.base]
  }

encodeItemRequest : Int -> ViewState -> Value
encodeItemRequest index s = Encode.object
  [ ("username", Encode.string s.user.username)
  , ("password", Encode.string s.user.password)
  , ("index", Encode.int index)
  ]

bought : Int -> ViewState -> ViewState
bought index state =
  let user = state.user
  in { state
     | user =
      { user
      | items = user.items ++ [index]
      , base = state.items |> List.find (\i -> i.index == index) |> Maybe.withDefault user.base
      }
     }

equipped : Int -> ViewState -> ViewState
equipped index state =
  let user = state.user
  in { state
     | user = { user | base = state.items |> List.find (\i -> i.index == index) |> Maybe.withDefault user.base }
     }

view : ViewState -> Html Message
view state =
  [ h2 [] [text "Üzlet"]
  , description
  , [ [ Button.button
        [ Button.outlineSecondary
        , Button.attrs [ Spacing.m2 ]
        , Button.onClick SwitchToDashboard
        ] [text "Vissza"]
      , br [] []
      , displayEgg state.user.base.image
      , br [] []
      , br [] []
      , h2 [] [text <| "Kölni: " ++ String.fromInt state.user.perfume ++ " 💦"]
      ] |> Grid.col [Col.xs4]
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
  ] |> div []

itemToRow : User -> Item -> Table.Row Message
itemToRow user item =
  [ Table.td [] [displayImage item.image 123 165, text item.name]
  , Table.td [] [text <| String.fromInt item.cost ++ " 💦"]
  , Table.td []
    <| if item.index == user.base.index
       then []
       else if List.member item.index (user.items ++ [user.base.index])
       then [Button.button [Button.primary, Button.onClick (EquipItem item.index)] [text "Felszerel"]]
       else
        let cantBuy = user.perfume < item.cost
        in
          if Settings.cokk2021 == EventState.Blocked
          then []
          else
            [ Button.button
                [ if cantBuy then Button.secondary else Button.success
                , Button.onClick (BuyItem item.index)
                , Button.disabled cantBuy
                ] [text "Megveszem"]
            ]
  ] |> Table.tr []

description : Html Message
description =
  [ text "Minden tojás egyedi. De a tiéd a legegyedibb! Itt kiszínezheted a tojásod tetszésedre, meg külömböző tárgyakat is vásárolhatsz neki, persze ezeket mások is láthatják."
  , br [] []
  , br [] []
  , text "Szeretnél egy legeslegegyedibb tojást? Lehet ha meggyöződ a verseny alkotóját ki lehet hozni valamit. Én nem tudom hisz csak egy leírás vagyok."
  , br [] []
  ] |> div []
