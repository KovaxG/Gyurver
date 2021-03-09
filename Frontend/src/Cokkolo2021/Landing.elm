module Cokkolo2021.Landing exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Html
import Http as Http
import Json.Decode as Decode exposing (Decoder)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid

import Cokkolo2021.Views.Login as Login
import Cokkolo2021.Views.Skills as Skills
import Cokkolo2021.Views.Register as Register
import Cokkolo2021.Views.Dashboard as Dashboard
import Cokkolo2021.Views.Contestants as Contestants
import Cokkolo2021.Message exposing (..)
import Cokkolo2021.Common exposing (..)
import Cokkolo2021.View exposing (..)

import Settings
import Endpoints
import Util

type alias Model = View
type alias Msg = Message

init : (Model, Cmd Msg)
init = (LoginView Login.init, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case (msg, model) of
  (LoginMsg (Login.UsernameFieldChange d), LoginView s) -> (LoginView { s | username = d }, Cmd.none)
  (LoginMsg (Login.PasswordFieldChange d), LoginView s) -> (LoginView { s | password = d }, Cmd.none)
  (LoginMsg Login.SwitchToRegisterView, LoginView s) -> (RegisterView Register.init, Cmd.none)
  (DashboardMsg (Dashboard.FetchFailure err), LoginView s) -> (LoginView { s | state = Problem err }, Cmd.none)
  (DashboardMsg (Dashboard.FetchSuccess dashboardState), LoginView s) -> (DashboardView dashboardState, Cmd.none)
  (LoginMsg Login.Login, LoginView s) ->
    ( LoginView { s | state = Waiting }
    , Http.post
        { url = Settings.path ++ Endpoints.cokk2021LoginJson
        , body = Http.jsonBody (Login.encode s.username s.password)
        , expect = expectDashboardState
        }
    )
  (RegisterMsg (Register.UsernameFieldChange d), RegisterView s) -> (RegisterView { s | username = d }, Cmd.none)
  (RegisterMsg (Register.Password1FieldChange d), RegisterView s) -> (RegisterView { s | password1 = d }, Cmd.none)
  (RegisterMsg (Register.Password2FieldChange d), RegisterView s) -> (RegisterView { s | password2 = d }, Cmd.none)
  (RegisterMsg (Register.EggnameFieldChange d), RegisterView s) -> (RegisterView { s | eggname = d }, Cmd.none)
  (RegisterMsg Register.SwitchToLoginView, RegisterView s) -> (LoginView Login.init, Cmd.none)
  (DashboardMsg (Dashboard.FetchSuccess dashboardState), RegisterView s) -> (DashboardView dashboardState, Cmd.none)
  (DashboardMsg (Dashboard.FetchFailure err), RegisterView s) -> (RegisterView { s | state = Problem err }, Cmd.none)
  (RegisterMsg Register.Register, RegisterView s) ->
    ( RegisterView { s | state = Waiting }
    , Http.post
        { url = Settings.path ++ Endpoints.cokk2021RegisterJson
        , body = Http.jsonBody (Register.encode s)
        , expect = expectDashboardState
        }
    )
  (DashboardMsg Dashboard.Logout, DashboardView _) -> (LoginView Login.init, Cmd.none)
  (DashboardMsg Dashboard.SwitchToContestantView, DashboardView s) ->
    ( ContestantView <| Contestants.init s.user
    , Http.post
      { url = Settings.path ++ Endpoints.cokk2021ParticipantsJson
      , body = Http.jsonBody (Login.encode s.user.username s.user.password)
      , expect =
          Http.expectJson (Util.processMessage (ContestantsMsg << Contestants.PopulateList) (always <| ContestantsMsg <| Contestants.PopulateList []))
                          (Decode.list Contestants.decode)
      }
    )
  (ContestantsMsg (Contestants.PopulateList items), ContestantView s) -> (ContestantView { s | items = items }, Cmd.none)
  (ContestantsMsg Contestants.SwitchToDashboardView, ContestantView s) ->
    ( DashboardView <| Dashboard.populateTemporary s.user
    , Http.post
      { url = Settings.path ++ Endpoints.cokk2021DashboardJson
      , body = Http.jsonBody (Login.encode s.user.username s.user.password)
      , expect = expectDashboardState
      }
    )
  (ContestantsMsg (Contestants.WaterUser target), ContestantView s) ->
    ( ContestantView { s | items = Util.mapIf (\c -> c.username == target) (\c -> { c | waterable = False }) s.items }
    , Http.post
      { url = Settings.path ++ Endpoints.cokk2021Water
      , body = Http.jsonBody (Contestants.encodeWaterBody target s.user)
      , expect = Http.expectWhatever (always <| ContestantsMsg Contestants.WateringSuccess)
      }
    )
  (ContestantsMsg Contestants.WateringSuccess, ContestantView s) -> (ContestantView s, Cmd.none)
  (DashboardMsg (Dashboard.FetchSuccess dashboardState), DashboardView s) -> (DashboardView dashboardState, Cmd.none)
  (DashboardMsg (Dashboard.FetchFailure errorMessage), DashboardView s) -> (DashboardView s, Cmd.none)
  (DashboardMsg Dashboard.SwitchToSkillsView, DashboardView s) -> (SkillsView <| Skills.init s.user, Cmd.none)
  (SkillsMsg Skills.SwitchToDashboard, SkillsView s) ->
    ( DashboardView <| Dashboard.populateTemporary s.user
    , Http.post
      { url = Settings.path ++ Endpoints.cokk2021DashboardJson
      , body = Http.jsonBody (Login.encode s.user.username s.user.password)
      , expect = expectDashboardState
      }
    )
  _ -> (LoginView Login.init, Cmd.none)

expectDashboardState : Http.Expect Msg
expectDashboardState =
  Http.expectJson
    (Util.processMessage (DashboardMsg << Dashboard.FetchSuccess) (DashboardMsg << Dashboard.FetchFailure))
    Dashboard.decode

view : Model -> Document Msg
view model =
  { title = "Cokkolo 2021"
  , body =
    [ [ CDN.stylesheet
      , case model of
          LoginView state -> Html.map LoginMsg <| Login.view state
          SkillsView state -> Html.map SkillsMsg <| Skills.view state
          RegisterView state -> Html.map RegisterMsg <| Register.view state
          DashboardView state -> Html.map DashboardMsg <| Dashboard.view state
          ContestantView state -> Html.map ContestantsMsg <| Contestants.view state
      ] |> Grid.container []
    ]
  }
