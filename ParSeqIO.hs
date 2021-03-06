module Control.Concurrent.ParSeqIO (parSeqIO, parSeqIO_) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad

{-|
Runs each IO operation in the input list in parallel.
Returns the output of each corresponding operation.
NOTE: Each IO operation should be mutually exclusive
as their execution order is non-deterministic
-}
parSeqIO :: [IO b] -> IO [b]
parSeqIO ms =
    mapM takeMVar <=< forM ms $ \io ->
        do mvar <- newEmptyMVar
           forkIO (io >>= putMVar mvar)
           return mvar

{-|
Same as parSeqIO, but discard the result.
-}
parSeqIO_ :: [IO b] -> IO ()
parSeqIO_ ms = parSeqIO ms >> return ()

