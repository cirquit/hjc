module TimeMonad where

import           Control.Monad.Trans.State
import           Control.Monad.Trans.Class          (lift)
import           Control.Monad.IO.Class
import           Control.Lens
import           Control.Monad                      (when, zipWithM_)

import           System.CPUTime                     (getCPUTime)

-- | Wrap an 'IO' computation so that it returns execution time is seconds as well as the real value
--
timeItT :: IO a -> IO (Double, a)
timeItT ioa = do
    t1 <- getCPUTime
    a <- ioa
    t2 <- getCPUTime
    let t :: Double
        t = fromIntegral (t2-t1) * 1e-8
    return (t, a)

timeItT_ :: IO a -> IO Double
timeItT_ ioa = fst <$> timeItT ioa

type Time m = StateT TimeState m

data TimeState = TimeState { _ms :: Double }

time :: IO a -> Time IO a
time f = do
    (time, res) <- io $ timeItT f
    ms %= (+ time)
    return res

time_ :: IO a -> Time IO ()
time_ f = time f >> return ()

ms   :: Lens' TimeState Double
ms = lens _ms (\x y -> x { _ms = y })

evaluateTimed :: Monad m => StateT TimeState m a -> m TimeState
evaluateTimed f = snd <$> runTimed f

getTimed :: Monad m => StateT TimeState m a -> m a
getTimed f = fst <$> runTimed f

runTimed :: Monad m => StateT TimeState m a -> m (a, TimeState)
runTimed f = runStateT f defaultTime 


getTimeInMs :: Monad m => StateT TimeState m Double
getTimeInMs = view ms <$> get

io :: MonadIO m => IO a -> m a
io = liftIO

defaultTime :: TimeState
defaultTime = TimeState { _ms = 0 }