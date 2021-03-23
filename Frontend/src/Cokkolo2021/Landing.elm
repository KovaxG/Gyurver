module Cokkolo2021.Landing exposing (Model, Msg, init, update, view)

import Browser exposing (Document)
import Html
import Http as Http
import Json.Decode as Decode exposing (Decoder)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Cokkolo2021.Views.Egg as Egg
import Cokkolo2021.Views.Login as Login
import Cokkolo2021.Views.Store as Store
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
        , body = Http.jsonBody (Login.encode s)
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
      , body = Http.jsonBody (Login.encodeGeneric s.user.username s.user.password)
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
      , body = Http.jsonBody (Login.encodeGeneric s.user.username s.user.password)
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
  (ContestantsMsg (Contestants.SwitchToEggView contestant), ContestantView s) -> (EggView <| Egg.init s contestant, Cmd.none)
  (DashboardMsg (Dashboard.FetchSuccess dashboardState), DashboardView s) -> (DashboardView dashboardState, Cmd.none)
  (DashboardMsg (Dashboard.FetchFailure errorMessage), DashboardView s) -> (DashboardView s, Cmd.none)
  (DashboardMsg Dashboard.SwitchToSkillsView, DashboardView s) -> (SkillsView <| Skills.init s.user, Cmd.none)
  (DashboardMsg Dashboard.SwitchToStoreView, DashboardView s) ->
    ( StoreView <| Store.init s.user
    , Http.get
      { url = Settings.path ++ Endpoints.cokk2021Items
      , expect = Http.expectJson (Util.processMessage (StoreMsg << Store.PopulateItems) (always <| StoreMsg <| Store.PopulateItems [])) itemsDecoder
      }
    )
  (DashboardMsg (Dashboard.HoweringOverEggName b), DashboardView s) -> (DashboardView { s | showEggnameEdit = b }, Cmd.none)
  (DashboardMsg (Dashboard.EditEggName en), DashboardView s) -> (DashboardView { s | eggNameInput = en, showEggnameEdit = False }, Cmd.none)
  (DashboardMsg (Dashboard.ChangeEggnameSuccess en), DashboardView s) -> let user = s.user in (DashboardView { s | user = { user | eggName = en }, eggNameInput = Nothing, showEggnameEdit = False }, Cmd.none)
  (DashboardMsg (Dashboard.ChangeEggnameFailure er), DashboardView s) -> (DashboardView { s | eggNameInputError = er }, Cmd.none)
  (DashboardMsg (Dashboard.ChangeEggnameRequest en), DashboardView s) ->
    (DashboardView s
    , Http.post
      { url = Settings.path ++ Endpoints.cokk2021updateEggNameJson
      , body = Http.jsonBody <| Dashboard.encodeEggnameChangeRequest s en
      , expect = Http.expectWhatever <| Util.processMessage (\_ -> DashboardMsg <| Dashboard.ChangeEggnameSuccess en) (DashboardMsg << Dashboard.ChangeEggnameFailure)
      }
    )
  (SkillsMsg Skills.SwitchToDashboard, SkillsView s) ->
    ( DashboardView <| Dashboard.populateTemporary s.user
    , Http.post
      { url = Settings.path ++ Endpoints.cokk2021DashboardJson
      , body = Http.jsonBody (Login.encodeGeneric s.user.username s.user.password)
      , expect = expectDashboardState
      }
    )
  (SkillsMsg (Skills.IncSkill skill cost), SkillsView s) ->
    ( SkillsView s
    , Http.post
      { url = Settings.path ++ Endpoints.cokk2021IncSkillJson
      , body = Http.jsonBody <| Skills.toIncSkillRequest s skill
      , expect = Http.expectWhatever <| Util.processMessage (\_ -> SkillsMsg <| Skills.IncSkillSuccess skill cost) (SkillsMsg << Skills.IncSkillFailure)
      }
    )
  (SkillsMsg (Skills.IncSkillSuccess skill cost), SkillsView s) -> (SkillsView <| Skills.update skill cost s, Cmd.none)
  (SkillsMsg (Skills.IncSkillFailure _), SkillsView s) -> (SkillsView s, Cmd.none)
  (EggMsg Egg.SwitchToContestantsView, EggView s) -> (ContestantView s.contestantsState, Cmd.none)
  (StoreMsg (Store.PopulateItems items), StoreView s) -> (StoreView { s | items = items }, Cmd.none)
  (StoreMsg Store.SwitchToDashboard, StoreView s) ->
    ( DashboardView <| Dashboard.populateTemporary s.user
    , Http.post
      { url = Settings.path ++ Endpoints.cokk2021DashboardJson
      , body = Http.jsonBody (Login.encodeGeneric s.user.username s.user.password)
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
          EggView state -> Html.map EggMsg <| Egg.view state
          LoginView state -> Html.map LoginMsg <| Login.view state
          StoreView state -> Html.map StoreMsg <| Store.view state
          SkillsView state -> Html.map SkillsMsg <| Skills.view state
          RegisterView state -> Html.map RegisterMsg <| Register.view state
          DashboardView state -> Html.map DashboardMsg <| Dashboard.view state
          ContestantView state -> Html.map ContestantsMsg <| Contestants.view state
      ] |> Grid.container []
    ]
  }
