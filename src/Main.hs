import Gyurver (runServer, IP(..), Port(..))

main :: IO ()
main = runServer (IP "127.0.0.1") (Port 8000) show
