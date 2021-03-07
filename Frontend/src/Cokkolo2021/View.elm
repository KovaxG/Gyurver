module Cokkolo2021.View exposing (..)

import Cokkolo2021.Views.Login as Login
import Cokkolo2021.Views.Register as Register
import Cokkolo2021.Views.Dashboard as Dashboard
import Cokkolo2021.Views.Contestants as Contestants

type View
  = LoginView Login.ViewState
  | RegisterView Register.ViewState
  | DashboardView Dashboard.ViewState
  | ContestantView Contestants.ViewState
