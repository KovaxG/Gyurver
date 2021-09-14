module Rights exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Table as Table
import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Html exposing (Html, div, text, br)
import Html.Events exposing (onClick)
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
  , editIndex : Maybe Int
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

populateRow : Row -> InputRow
populateRow row = InputRow row.enabled row.secret (row.rights |> String.join ", ")

parseInputRow : InputRow -> Row
parseInputRow row =
  { enabled = row.enabled
  , secret = row.secret
  , rights =
    row.rights
    |> String.replace "," " "
    |> String.words
    |> List.filter (\w -> w /= "")
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
  | EditRow Row Int
  | CancelEdit
  | EditRowEnabledChanged
  | EditRowRightsChanged String
  | UpdateRow Int String
  | UpdateRowSuccess Int Row
  | UpdateRowFailure String

init : (Model, Cmd Msg)
init = (initModel, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PasswordChanged pass -> case model of
      PasswordScreen pswdm -> (PasswordScreen {pswdm | password = pass }, Cmd.none)
      _ -> (model, Cmd.none)
    Populate pass data -> (Ok { password = pass, data = data, newRow = blankInputRow, editRow = blankInputRow, editIndex = Nothing, message = ""}, Cmd.none)
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
    EditRowEnabledChanged -> case model of
      Ok info -> let editRow = info.editRow in (Ok { info | editRow = { editRow | enabled = not editRow.enabled } }, Cmd.none)
      _ -> (model, Cmd.none)
    EditRowRightsChanged new -> case model of
      Ok info -> let editRow = info.editRow in (Ok { info | editRow = { editRow | rights = new } }, Cmd.none)
      _ -> (model, Cmd.none)
    AddRow pass -> case model of
      Ok info ->
        let newRow = parseInputRow info.newRow
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
    EditRow row index -> case model of
      Ok okmodel -> if okmodel.editIndex == Nothing then (Ok { okmodel | editIndex = Just index, editRow = populateRow row}, Cmd.none) else (model, Cmd.none)
      _ -> (model, Cmd.none)
    CancelEdit -> case model of
      Ok okmodel -> (Ok { okmodel | editIndex = Nothing, editRow = blankInputRow }, Cmd.none)
      _ -> (model, Cmd.none)
    UpdateRow index pass -> case model of
      Ok info ->
        let updateRow = parseInputRow info.editRow
        in
          ( model
          , Http.request
            { method = "PUT"
            , headers = [ Http.header "Gyurpass" pass ]
            , url = Settings.path ++ Endpoints.rightsPageJson
            , body = Http.jsonBody <| encodeRow updateRow
            , expect = Http.expectWhatever <| Util.processMessage (always <| UpdateRowSuccess index updateRow) UpdateRowFailure
            , timeout = Nothing
            , tracker = Nothing
            }
          )
      _ -> (model, Cmd.none)
    UpdateRowSuccess index row -> case model of
      Ok okmodel ->
        ( Ok { okmodel
             | editIndex = Nothing
             , editRow = blankInputRow
             , data = okmodel.data |> List.indexedMap (\i r -> if (i == index) then row else r)
             }
        , Cmd.none
        )
      _ -> (model, Cmd.none)
    UpdateRowFailure err -> case model of
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
      , tbody = (info.data |> List.indexedMap (dataRow info)) ++ [newDataRow info] |> Table.tbody []
      }
  , text info.message
  , br [] []
  , case info.editIndex of
      Just _ ->
        Button.button
          [ Button.outlineDark
          , Button.onClick CancelEdit
          ] [text "Cancel"]
      Nothing ->
        Button.button
          [ Button.outlineSuccess
          , Button.onClick <| GetData info.password
          ] [text "Refresh"]
  ] |> div []

dataRow : OkScr -> Int -> Row -> Table.Row Msg
dataRow info index row =
  info.editIndex
    |> Maybe.andThen (\eindex ->
      if eindex == index
      then
        Just <| Table.tr []
        [ Table.td [] [updateButton info.password eindex]
        , Table.td [] [Button.button [Button.onClick EditRowEnabledChanged] [text <| showEnabled info.editRow.enabled]]
        , Table.td [] [text row.secret]
        , Table.td [] [Input.text [Input.small, Input.value info.editRow.rights, Input.onInput EditRowRightsChanged]]
        ]
      else
        Nothing
    ) |> Maybe.withDefault (
      Table.tr [ Table.rowAttr <| onClick <| EditRow row index ]
        [ Table.td [] [deleteButton info.password row]
        , Table.td [] [text <| showEnabled row.enabled]
        , Table.td [] [text row.secret]
        , Table.td [] [row.rights |> List.intersperse ", " |> String.concat |> text]
        ]
    )

newDataRow : OkScr -> Table.Row Msg
newDataRow info =
  Table.tr []
    [ Table.td [] [Button.button [Button.success, Button.onClick <| AddRow info.password] [text "➕"]]
    , Table.td [] [Button.button [Button.onClick NewRowEnabledChanged] [text <| showEnabled info.newRow.enabled]]
    , Table.td [] [Input.text [Input.small, Input.value info.newRow.secret, Input.onInput NewRowSecretChanged]]
    , Table.td [] [Input.text [Input.small, Input.value info.newRow.rights, Input.onInput NewRowRightsChanged]]
    ]

deleteButton : String -> Row -> Html Msg
deleteButton pass row = Button.button [ Button.danger, Button.onClick <| DeleteRow row pass ] [text "🗑️"]

updateButton : String -> Int -> Html Msg
updateButton pass index = Button.button [ Button.primary, Button.onClick <| UpdateRow index pass ] [text "✔️"]

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

showEnabled : Bool -> String
showEnabled b = if b then "✔️" else "❌"
