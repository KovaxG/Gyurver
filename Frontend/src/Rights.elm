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
import Json.Encode as Encode exposing (Value)
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
  , newRow : InputRow
  , editRow : InputRow
  , message : String
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

type alias InputRow =
  { enabled : Bool
  , secret : String
  , rights : String
  }

blankInputRow : InputRow
blankInputRow = InputRow True "" ""

rowDecoder : Decoder Row
rowDecoder =
  Decode.map3
    Row
    (Decode.field "enabled" Decode.bool)
    (Decode.field "secret" Decode.string)
    (Decode.field "rights" (Decode.list Decode.string))

encodeRow : Row -> Value
encodeRow row =
  Encode.object
    [ ("enabled", Encode.bool row.enabled)
    , ("secret", Encode.string row.secret)
    , ("rights", Encode.list Encode.string row.rights)
    ]

parseNewRow : InputRow -> Row
parseNewRow row =
  { enabled = row.enabled
  , secret = row.secret
  , rights =
    row.rights
    |> String.replace "," " "
    |> String.words
  }

type Msg
  = PasswordChanged String
  | GetData String
  | PasswordCheckFailed
  | Populate String (List Row)
  | DeleteRow Row String
  | DeleteRowSuccess Row
  | DeleteRowFailed String
  | NewRowEnabledChanged
  | NewRowSecretChanged String
  | NewRowRightsChanged String
  | AddRow String
  | AddRowSuccess Row
  | AddRowFailure String

init : (Model, Cmd Msg)
init = (initModel, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PasswordChanged pass -> case model of
      PasswordScreen pswdm -> (PasswordScreen {pswdm | password = pass }, Cmd.none)
      _ -> (model, Cmd.none)
    Populate pass data -> (Ok { password = pass, data = data, newRow = blankInputRow, editRow = blankInputRow, message = ""}, Cmd.none)
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
    DeleteRowFailed err -> case model of
      Ok okmodel -> (Ok { okmodel | message = err }, Cmd.none)
      _ -> (model, Cmd.none)
    DeleteRowSuccess row -> case model of
      Ok okmodel -> (Ok { okmodel | data = okmodel.data |> List.filter (\r -> r.secret /= row.secret) }, Cmd.none)
      _ -> (model, Cmd.none)
    NewRowEnabledChanged -> case model of
      Ok info -> let newRow = info.newRow in (Ok { info | newRow = { newRow | enabled = not newRow.enabled } }, Cmd.none)
      _ -> (model, Cmd.none)
    NewRowSecretChanged new -> case model of
      Ok info -> let newRow = info.newRow in (Ok { info | newRow = { newRow | secret = new } }, Cmd.none)
      _ -> (model, Cmd.none)
    NewRowRightsChanged new -> case model of
      Ok info -> let newRow = info.newRow in (Ok { info | newRow = { newRow | rights = new } }, Cmd.none)
      _ -> (model, Cmd.none)
    AddRow pass -> case model of
      Ok info ->
        let newRow = parseNewRow info.newRow
        in
          ( model
          , Http.request
            { method = "POST"
            , headers = [ Http.header "Gyurpass" pass ]
            , url = Settings.path ++ Endpoints.rightsPageJson
            , body = Http.jsonBody <| encodeRow newRow
            , expect = Http.expectWhatever <| Util.processMessage (always <| AddRowSuccess newRow) AddRowFailure
            , timeout = Nothing
            , tracker = Nothing
            }
          )
      _ -> (model, Cmd.none)
    AddRowSuccess row -> case model of
      Ok okmodel -> (Ok { okmodel | data = okmodel.data ++ [row], newRow = blankInputRow }, Cmd.none)
      _ -> (model, Cmd.none)
    AddRowFailure err -> case model of
      Ok okmodel -> (Ok { okmodel | message = err }, Cmd.none)
      _ -> (model, Cmd.none)

view : Model -> Document Msg
view model =
  { title = "Films"
  , body =
    [ CDN.stylesheet
    , [ [ [ case model of
              PasswordScreen info -> passwordPage info
              Ok data -> dataPage data
          --, text <| Debug.toString model
          ] |> Grid.col []
        ] |> Grid.row []
      ] |> Grid.container []
    ]
  }

dataPage : OkScr -> Html Msg
dataPage info =
  [ Table.table
      { options = [ Table.hover ]
      , thead =
          Table.simpleThead
            [ Table.th [] []
            , Table.th [] [text "Enabled"]
            , Table.th [] [text "Secret"]
            , Table.th [] [text "Rights"]
            ]
      , tbody = (info.data |> List.map (dataRow info.password)) ++ [newDataRow info] |> Table.tbody []
      }
  , text info.message
  , br [] []
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

newDataRow : OkScr -> Table.Row Msg
newDataRow info =
  Table.tr []
    [ Table.td [] [Button.button [Button.success, Button.onClick <| AddRow info.password] [text "➕"]]
    , Table.td [] [Button.button [Button.onClick NewRowEnabledChanged] [text <| if info.newRow.enabled then "enabled" else "disabled"]]
    , Table.td [] [Input.text [Input.small, Input.value info.newRow.secret, Input.onInput NewRowSecretChanged]]
    , Table.td [] [Input.text [Input.small, Input.value info.newRow.rights, Input.onInput NewRowRightsChanged]]
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
