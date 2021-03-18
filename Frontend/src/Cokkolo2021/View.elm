module Cokkolo2021.View exposing (..)

import Cokkolo2021.Views.Egg as Egg
import Cokkolo2021.Views.Login as Login
import Cokkolo2021.Views.Skills as Skills
import Cokkolo2021.Views.Register as Register
import Cokkolo2021.Views.Dashboard as Dashboard
import Cokkolo2021.Views.Contestants as Contestants

type View
  = EggView Egg.ViewState
  | LoginView Login.ViewState
  | SkillsView Skills.ViewState
  | RegisterView Register.ViewState
  | DashboardView Dashboard.ViewState
  | ContestantView Contestants.ViewState
