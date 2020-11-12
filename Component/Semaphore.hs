module Component.Semaphore (Semaphore, new, block, unblock) where

import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)

newtype Semaphore = Semaphore (MVar ())

new :: IO Semaphore
new = Semaphore <$> newMVar ()

block :: Semaphore -> IO ()
block (Semaphore mvar) = takeMVar mvar

unblock :: Semaphore -> IO ()
unblock (Semaphore mvar) = putMVar mvar ()
