module Cokkolo2021.Message exposing (..)

import Cokkolo2021.Views.Egg as Egg
import Cokkolo2021.Views.Login as Login
import Cokkolo2021.Views.Skills as Skills
import Cokkolo2021.Views.Register as Register
import Cokkolo2021.Views.Dashboard as Dashboard
import Cokkolo2021.Views.Contestants as Contestants

type Message
  = EggMsg Egg.Message
  | LoginMsg Login.Message
  | SkillsMsg Skills.Message
  | RegisterMsg Register.Message
  | DashboardMsg Dashboard.Message
  | ContestantsMsg Contestants.Message
