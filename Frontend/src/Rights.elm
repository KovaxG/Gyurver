module Rights exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Table as Table
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Html exposing (Html, div, text, br)
import Json.Decode as Decode exposing (Decoder)
import Endpoints
import Settings
import Http
import Util

type alias PasswordScr =
  { message : String
  , password : String
  }

type alias OkScr =
  { data : List Row
  , password : String
  }

type Model
  = PasswordScreen PasswordScr
  | Ok OkScr

initModel : Model
initModel = PasswordScreen { message = "", password = "" }

type alias Row =
  { enabled : Bool
  , secret : String
  , rights : List String
  }

rowDecoder : Decoder Row
rowDecoder =
  Decode.map3
    Row
    (Decode.field "enabled" Decode.bool)
    (Decode.field "secret" Decode.string)
    (Decode.field "rights" (Decode.list Decode.string))

type Msg
  = PasswordChanged String
  | GetData String
  | PasswordCheckFailed
  | Populate String (List Row)
  | DeleteRow Row String
  | DeleteRowSuccess Row
  | DeleteRowFailed String

init : (Model, Cmd Msg)
init = (initModel, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PasswordChanged pass -> case model of
      PasswordScreen pswdm -> (PasswordScreen {pswdm | password = pass }, Cmd.none)
      _ -> (model, Cmd.none)
    Populate pass data -> (Ok { password = pass, data = data }, Cmd.none)
    PasswordCheckFailed -> (model, Cmd.none)
    GetData pass ->
      ( model,
        Http.request
        { method = "GET"
        , headers = [ Http.header "Gyurpass" pass ]
        , url = Settings.path ++ Endpoints.rightsPageJson
        , body = Http.emptyBody
        , expect = Http.expectJson
                    (Util.processMessage (Populate pass) (always PasswordCheckFailed))
                    (Decode.list rowDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }
      )
    DeleteRow row pass ->
      ( model
      , Http.request
        { method = "DELETE"
        , headers = [ Http.header "Gyurpass" pass ]
        , url = Settings.path ++ Endpoints.rightsPageJson
        , body = Http.stringBody "" row.secret
        , expect = Http.expectWhatever <| Util.processMessage (always <| DeleteRowSuccess row) DeleteRowFailed
        , timeout = Nothing
        , tracker = Nothing
        }
      )
    DeleteRowFailed _ -> (model, Cmd.none)
    DeleteRowSuccess row -> case model of
      Ok okmodel ->
        ( Ok { okmodel | data = okmodel.data |> List.filter (\r -> r.secret /= row.secret) }
        , Cmd.none
        )
      _ -> (model, Cmd.none)

view : Model -> Document Msg
view model =
  { title = "Films"
  , body =
    [ CDN.stylesheet
    , [ [ [ case model of
              PasswordScreen info -> passwordPage info
              Ok data -> dataPage data
          ] |> Grid.col []
        ] |> Grid.row []
      ] |> Grid.container []
    ]
  }

dataPage : OkScr -> Html Msg
dataPage info =
  [ Table.table
      { options = []
      , thead =
          Table.simpleThead
            [ Table.th [] []
            , Table.th [] [text "Enabled"]
            , Table.th [] [text "Secret"]
            , Table.th [] [text "Rights"]
            ]
      , tbody = info.data |> List.map (dataRow info.password) |> Table.tbody []
      }
  , Button.button
      [ Button.outlineSuccess
      , Button.onClick <| GetData info.password
      ] [text "Refresh"]
  ] |> div []

dataRow : String -> Row -> Table.Row Msg
dataRow pass row =
  Table.tr []
    [ Table.td [] [deleteButton pass row]
    , Table.td [] [text <| if row.enabled then "enabled" else "disabled"]
    , Table.td [] [text row.secret]
    , Table.td [] [row.rights |> List.intersperse ", " |> String.concat |> text]
    ]

deleteButton : String -> Row -> Html Msg
deleteButton pass row = Button.button [ Button.danger, Button.onClick <| DeleteRow row pass ] [text "🗑️"]

passwordPage : PasswordScr -> Html Msg
passwordPage info =
  [ text info.message
  , br [] []
  , InputGroup.config
    ( InputGroup.password [ Input.onInput PasswordChanged, Input.value info.password ] )
    |> InputGroup.successors
      [ InputGroup.button [ Button.outlinePrimary, Button.onClick <| GetData info.password ] [ text "Go"] ]
      |> InputGroup.view
  ] |> div []
