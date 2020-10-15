## Dependencies

You will need to pull the following dependencies in order to compile the project:
- `network-simple`
- `random`

`cabal install <element>`

## Configuration file

You need to place a configuration file in order for a server to work: `gyurver.settings`

Example:
```
host_address 0.0.0.0
port 80
password very_safe_password
```

The settings are loaded in the following object:
```haskell
data Settings = Settings
  { hostAddress :: IP
  , port :: Port
  , password :: String
  } deriving (Show)
```

If you have no config file the following will be loaded:
```haskell
defaultSettings = Settings
  { hostAddress = IP "localhost"
  , port = Port 8080
  , password = "nincs jelszo"
  }
```
## Endpoints

### Resources
- `/cv` CV as a PDF
- `/favicon.ico` Get the favicon

### Pages
- `/` Landing Page
- `/articles` Get a html list of articles with links
- `/vids` Get a list of embedded videos
- `/cokk` The contestants of the 2020 cokkoles
- `/cokk/eredmeny` The results of the 2020 Cokkoles contest

### JSON
- `/cokk/list` list of contestants of the 2020 Cokkoles
- `/api/vids` list of videos

