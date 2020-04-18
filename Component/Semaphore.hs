module Component.Semaphore (Semaphore, new, block, unblock) where

import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)

data Semaphore = Semaphore (MVar ())

new :: IO Semaphore
new = fmap Semaphore $ newMVar ()

block :: Semaphore -> IO ()
block (Semaphore mvar) = takeMVar mvar

unblock :: Semaphore -> IO ()
unblock (Semaphore mvar) = putMVar mvar ()
