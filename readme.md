## Dependencies

You will need to pull the following dependencies in order to compile the project:
- `network-simple`
- `random`
- `parsec`

`cabal install <element> --lib`

## Configuration file

You need to place a configuration file in order for a server to work: `gyurver.settings`

Example:
```
host_address 0.0.0.0
port 80
password very_safe_password
mode prod
cokk2021 blocked
```

The settings are loaded in the following object:
```haskell
data Settings = Settings
  { hostAddress :: IP
  , port :: Port
  , password :: String
  , mode :: Mode
  , cokk2021 :: EventMode
  } deriving (Show)
```

If you have no config file the following will be loaded:
```haskell
defaultSettings = Settings
  { hostAddress = IP "localhost"
  , port = Port 8080
  , password = "nincs jelszo"
  , mode = Dev
  , cokk2021 = Blocked
  }
```
## Endpoints

### Pages
- `/` Landing Page
- `/articles` Get a html list of articles with links
- `/videos` Get a list of embedded videos
- `/videos/new` Add new video page
- `/cokk2020` Landing page of 2020 Cokkoles contest
- `/cokk2020/results` The results of the 2020 Cokkoles contest
- `/cokk2021` Landing page of 2021 Cokkoles contest
- `/cokk2021/results` The results of the 2021 Cokkoles contest


### Resources
- `/favicon.ico` Get the favicon
- `/cv` alternative route for CV
- `/res/cv.pdf` Get the CV
- `/res/anomaly_detection_taxonomy.pdf` Taxonomy article
- `/res/anomaly_detection_platform.pdf` Anomaly detection article
- `/res/anomaly_detection_metrics.pdf` Anomaly detection metrics article

### JSON
- `/api/videos` list of videos endpoint (Get, Post, Options)
- `/api/video/#` data about a certain video (Get, Post, Delete)
- `/api/cokk2020` list of contestants of the 2020 Cokkoles
- `/api/cokk2021/login` login endpoint for 2021 Cokkoles (Post)
- `/api/cokk2021/register` register endpoint for 2021 Cokkoles (Post)
- `/api/cokk2021/resztvevok` list of participants for 2021 Cokkoles (Get, Post)
- `/api/cokk2021/water` water someone for 2021 Cokkoles (Post)
- `/api/cokk2021/dashboard` refresh dashboard for 2021 Cokkoles (Post)
- `/api/cokk2021/skills/inc` increase skill endpoint for 2021 Cokkoles (Post)
- `/api/cokk2021/update/eggname` ability to change egg name for 2021 Cokkoles (Post)
- `/api/cokk2021/items` get list of items for 2021 Cokkoles (Get)
- `/api/cokk2021/items/buy` buy item endpoint for 2021 Cokkoles (Post)
- `/api/cokk2021/items/equip` equip item endpoint for 2021 Cokkoles (Post)
- `/api/cokk2021/fight` run a match between 2 eggs for 2021 Cokkoles (Post)
