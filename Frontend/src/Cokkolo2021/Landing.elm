module Cokkolo2021.Landing exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Html exposing (Html, button, div, text, h1, h2, h3, p, ol, li, br, a, img)
import Html.Attributes exposing (src, alt, style)
import Http as Http
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Debug
import List.Extra as List

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button
import Bootstrap.Spinner as Spinner
import Bootstrap.Text as Text
import Bootstrap.Form.Input as Input
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Table as Table

import Types.DateTime as DateTime exposing (DateTime)
import Settings
import Endpoints
import Util

type ViewState = Normal | Waiting | Problem String

type alias LoginViewState =
  { username : String
  , password : String
  , state : ViewState
  }

encodeLoginInfo : String -> String -> Value
encodeLoginInfo username password =
  Encode.object
    [ ("user", Encode.string username)
    , ("pass", Encode.string password)
    ]

loginViewInitState : LoginViewState
loginViewInitState =
  { username = ""
  , password = ""
  , state = Normal
  }

type alias RegisterViewState =
  { username : String
  , password1 : String
  , password2 : String
  , eggname : String
  , state : ViewState
  }

encodeRegisterViewState : RegisterViewState -> Value
encodeRegisterViewState s =
  Encode.object
    [ ("username", Encode.string s.username)
    , ("password", Encode.string s.password1)
    , ("eggname", Encode.string s.eggname)
    ]

registerViewInitState : RegisterViewState
registerViewInitState =
  { username = ""
  , password1 = ""
  , password2 = ""
  , eggname = ""
  , state = Normal
  }


type alias User =
  { username : String
  , password : String
  , eggName : String
  , perfume : Int
  , image : String
  }

userDecoder : Decoder User
userDecoder =
  Decode.map5
    User
    (Decode.field "username" Decode.string)
    (Decode.field "password" Decode.string)
    (Decode.field "eggname" Decode.string)
    (Decode.field "perfume" Decode.int)
    (Decode.field "image" Decode.string)

type alias DashboardViewState =
  { user : User
  , logs : List Log
  }

dashboardViewStateDecoder : Decoder DashboardViewState
dashboardViewStateDecoder =
  Decode.map2
    DashboardViewState
    (Decode.field "user" userDecoder)
    (Decode.field "events" <| Decode.list logDecoder)

populateTemporaryDashboard : User -> DashboardViewState
populateTemporaryDashboard user = { user = user, logs = [] }

type alias Log =
  { source : String
  , target : String
  , datetime : DateTime
  }

logDecoder : Decoder Log
logDecoder =
  Decode.map3
    Log
    (Decode.field "source" Decode.string)
    (Decode.field "target" Decode.string)
    (Decode.field "time" DateTime.decode)

type alias ContestantViewState =
  { user : User
  , items : List Contestant
  }

type alias Contestant =
  { username : String
  , eggname : String
  , image : String
  }

contestantDecoder : Decoder Contestant
contestantDecoder =
  Decode.map3
    Contestant
    (Decode.field "username" Decode.string)
    (Decode.field "eggname" Decode.string)
    (Decode.field "image" Decode.string)

initializeContestantsViewState : User -> ContestantViewState
initializeContestantsViewState u =
  { user = u
  , items = []
  }

type View
  = LoginView LoginViewState
  | RegisterView RegisterViewState
  | DashboardView DashboardViewState
  | ContestantView ContestantViewState

type alias Model = View
type Msg
  = LoginViewUsernameFieldChange String
  | LoginViewPasswordFieldChange String
  | LoginViewLogin
  | LoginViewSwitchToRegisterView
  | DashboardFetchSuccess DashboardViewState
  | DashboardFetchFailure String
  | RegisterViewUsernameFieldChange String
  | RegisterViewPassword1FieldChange String
  | RegisterViewPassword2FieldChange String
  | RegisterViewEggnameFieldChange String
  | RegisterViewSwitchToLoginView
  | RegisterViewRegister
  | DashboardViewLogout
  | DashboardViewSwitchToContestantView
  | ContestantViewSwitchToDashboardView
  | ContestantViewPopulateList (List Contestant)

init : (Model, Cmd Msg)
init = (LoginView loginViewInitState, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case (msg, model) of
  (LoginViewUsernameFieldChange d, LoginView s) -> (LoginView { s | username = d }, Cmd.none)
  (LoginViewPasswordFieldChange d, LoginView s) -> (LoginView { s | password = d }, Cmd.none)
  (LoginViewSwitchToRegisterView, LoginView s) -> (RegisterView registerViewInitState, Cmd.none)
  (DashboardFetchFailure err, LoginView s) -> (LoginView { s | state = Problem err }, Cmd.none)
  (DashboardFetchSuccess dashboardState, LoginView s) -> (DashboardView dashboardState, Cmd.none)
  (LoginViewLogin, LoginView s) ->
    ( LoginView { s | state = Waiting }
    , Http.post
        { url = Settings.path ++ Endpoints.cokk2021LoginJson
        , body = Http.jsonBody (encodeLoginInfo s.username s.password)
        , expect = expectDashboardState
        }
    )
  (RegisterViewUsernameFieldChange d, RegisterView s) -> (RegisterView { s | username = d }, Cmd.none)
  (RegisterViewPassword1FieldChange d, RegisterView s) -> (RegisterView { s | password1 = d }, Cmd.none)
  (RegisterViewPassword2FieldChange d, RegisterView s) -> (RegisterView { s | password2 = d }, Cmd.none)
  (RegisterViewEggnameFieldChange d, RegisterView s) -> (RegisterView { s | eggname = d }, Cmd.none)
  (RegisterViewSwitchToLoginView, RegisterView s) -> (LoginView loginViewInitState, Cmd.none)
  (DashboardFetchSuccess dashboardState, RegisterView s) -> (DashboardView dashboardState, Cmd.none)
  (DashboardFetchFailure err, RegisterView s) -> (RegisterView { s | state = Problem err }, Cmd.none)
  (RegisterViewRegister, RegisterView s) ->
    ( RegisterView { s | state = Waiting }
    , Http.post
        { url = Settings.path ++ Endpoints.cokk2021RegisterJson
        , body = Http.jsonBody (encodeRegisterViewState s)
        , expect = expectDashboardState
        }
    )
  (DashboardViewLogout, DashboardView _) -> (LoginView loginViewInitState, Cmd.none)
  (DashboardViewSwitchToContestantView, DashboardView s) ->
    ( ContestantView <| initializeContestantsViewState s.user
    , Http.get
      { url = Settings.path ++ Endpoints.cokk2021ParticipantsJson
      , expect = Http.expectJson (Util.processMessage ContestantViewPopulateList (always <| ContestantViewPopulateList [])) (Decode.list contestantDecoder)
      }
    )
  (ContestantViewPopulateList items, ContestantView s) -> (ContestantView { s | items = items }, Cmd.none)
  (ContestantViewSwitchToDashboardView, ContestantView s) ->
    ( DashboardView <| populateTemporaryDashboard s.user
    , Http.post
      { url = Settings.path ++ Endpoints.cokk2021DashboardJson
      , body = Http.jsonBody (encodeLoginInfo s.user.username s.user.password)
      , expect = expectDashboardState
      }
    )
  (DashboardFetchSuccess dashboardState, DashboardView s) -> (DashboardView dashboardState, Cmd.none)
  (DashboardFetchFailure errorMessage, DashboardView s) -> (DashboardView s, Cmd.none)
  _ -> (LoginView loginViewInitState, Cmd.none)


expectDashboardState : Http.Expect Msg
expectDashboardState =
  Http.expectJson
    (Util.processMessage DashboardFetchSuccess DashboardFetchFailure)
    dashboardViewStateDecoder

view : Model -> Document Msg
view model =
  { title = "Cokkolo 2021"
  , body =
    [ [ CDN.stylesheet
      , showPage model
      ] |> Grid.container []
    ]
  }

showPage : View -> Html Msg
showPage v = case v of
  LoginView state ->
    [ [ h1 [] [text "2021 Húsvéti játékok"]
      , text "Itt kene legyen a leiras"
      , h2 [] [text "Belépés"]
      , text "Gazda"
      , Input.text [Input.value state.username, Input.onInput LoginViewUsernameFieldChange]
      , text "Jelszó"
      , Input.password [Input.value state.password, Input.onInput LoginViewPasswordFieldChange]
      , Button.button
        [ Button.primary
        , Button.attrs [ Spacing.m2 ]
        , Button.onClick LoginViewLogin
        ] [text "Engeddj be!"]
      , Button.button
        [ Button.secondary
        , Button.attrs [ Spacing.m2 ]
        , Button.onClick LoginViewSwitchToRegisterView
        ] [text "Én is akarok tojást!"]
      , case state.state of
          Problem str -> text str
          Waiting -> text "waiting..."
          Normal -> text ""
      ] |> Grid.col []
    ] |> Grid.row []
  RegisterView state ->
    [ [ h1 [] [text "2021 Húsvéti játékok"]
      , h2 [] [text "Regisztrálás"]
      , text "Gazda neve"
      , Input.text [Input.value state.username, Input.onInput RegisterViewUsernameFieldChange]
      , text "Jelszó"
      , Input.text
        [ Input.value state.password1
        , Input.onInput RegisterViewPassword1FieldChange
        ]
      , text "Jelszó de mégegyszer"
      , Input.text
        [ Input.value state.password2
        , Input.onInput RegisterViewPassword2FieldChange
        , if state.password1 /= state.password2 || state.password1 == "" then Input.danger else Input.success
        ]
      , text "Tojás Neve"
      , Input.text [Input.value state.eggname, Input.onInput RegisterViewEggnameFieldChange]
      , Button.button
        [ Button.primary
        , Button.attrs [ Spacing.m2 ]
        , Button.onClick RegisterViewRegister
        ] [text "Regisztrálás!"]
      , Button.button
        [ Button.secondary
        , Button.attrs [ Spacing.m2 ]
        , Button.onClick RegisterViewSwitchToLoginView
        ] [text "Vissza"]
      , case state.state of
           Problem str -> text str
           Waiting -> text "waiting..."
           Normal -> text ""
      ] |> Grid.col []
    ] |> Grid.row []
  DashboardView state ->
    [ [ h2 [] [text <| state.user.eggName]
      , displayImage state.user.image 250 250
      ] |> Grid.col []
    , [ Button.button
        [ Button.primary
        , Button.attrs [ Spacing.m2 ]
        , Button.onClick DashboardViewLogout
        ] [text "Logout"]
      , br [] []
      , text <| "Kölni: " ++ String.fromInt state.user.perfume ++ " 💦"
      , br [] []
      , Button.button
        [ Button.outlineSecondary
        , Button.attrs [ Spacing.m2 ]
        , Button.onClick DashboardViewSwitchToContestantView
        ] [text "Résztvevők"]
      , br [] []
      , displayLogs state.user.username state.logs
      ] |> Grid.col []
    ] |> Grid.row []
  ContestantView state ->
    [ [ h1 [] [text "Résztvevők"]
      , Button.button
        [ Button.primary
        , Button.attrs [ Spacing.m2 ]
        , Button.onClick ContestantViewSwitchToDashboardView
        ] [text "Vissza"]
      , Table.table
        { options = []
        , thead =
            Table.simpleThead
              [ Table.th [] []
              , Table.th [] [text "Tojás"]
              , Table.th [] [text "Gazda"]
              , Table.th [] []
              ]
        , tbody =
              state.items
                |> List.map (\c ->
                  [ Table.td [] [ displayImage c.image 50 50 ]
                  , Table.td [] [ text c.eggname ]
                  , Table.td [] [ text c.username ]
                  , Table.td [] [ if c.username == state.user.username
                                  then text "(te vagy)"
                                  else Button.button [ Button.outlineSecondary] [text "💦"]
                                ]
                  ] |> Table.tr []
                )
              |> Table.tbody []
        }
      ] |> Grid.col []
    ] |> Grid.row []

displayImage : String -> Int -> Int -> Html Msg
displayImage image width height = img
  [ src <| getImageURL image
  , alt "Jaj ne nem töltödött be a kép! Most mi lesz 😢 Pls szólj Gyurinak"
  , style "height" (String.fromInt height ++ "px")
  , style "width" (String.fromInt width ++ "px")
  ] []

getImageURL : String -> String
getImageURL name = case name of
  "pucer" -> "https://www.pinclipart.com/picdir/middle/68-682374_egg-balancing-by-ofirma85-fnaf-puppet-pixel-art.png"
  _ -> "https://clipground.com/images/omg-emoji-clipart-3.jpg"

displayLogs : String -> List Log -> Html Msg
displayLogs username logs =
  div []
  <| List.map (\(byYearEx, byYears) ->
    div []
    <| (++) [text <| String.fromInt byYearEx.datetime.year, br [] []]
    <| List.map (\(byMonthEx, byMonths) ->
      div []
      <| List.map (\(sortedByDayEx, sortedByDays) ->
        div []
        <| (++) [text <| showMonthAndDay sortedByDayEx.datetime]
        <| List.map (\l -> div [] [text <| logToText username l])
        <| sortedByDayEx :: sortedByDays
      )
      <| List.groupWhile (groupOn <| \a -> a.datetime.day)
      <| List.sortWith (compareOn <| \a -> a.datetime.day)
      <| byMonthEx :: byMonths
    )
    <| List.groupWhile (groupOn <| \a -> a.datetime.month)
    <| List.sortWith (compareOn <| \a -> a.datetime.month)
    <| byYearEx :: byYears
  )
  <| List.groupWhile (groupOn <| \a -> a.datetime.year)
  <| List.sortWith (compareOn <| \a -> a.datetime.year)
  <| logs

logToText : String -> Log -> String
logToText you l =
  let msg =
        if l.source == you
        then "Megöntözted " ++ l.target ++ "-t"
        else "Megontözött " ++ l.source ++ " (+💦)"
      showDigits = String.padLeft 2 '0' << String.fromInt
  in showDigits l.datetime.hour ++ ":"  ++ showDigits l.datetime.minutes ++ " " ++ msg

compareOn : (a -> comparable) -> a -> a -> Order
compareOn f a b = compare (f a) (f b)

groupOn : (a -> b) -> a -> a -> Bool
groupOn f a b = f a == f b

monthToString : Int -> String
monthToString m = case m of
  1 -> "Jan"
  2 -> "Feb"
  3 -> "Mar"
  4 -> "Apr"
  5 -> "Maj"
  6 -> "Jun"
  7 -> "Jul"
  8 -> "Aug"
  9 -> "Sep"
  10 -> "Nov"
  11 -> "Okt"
  12 -> "Dec"
  _ -> "HOPSZ"

showMonthAndDay : DateTime -> String
showMonthAndDay dt = monthToString dt.month ++ " " ++ String.fromInt dt.day
