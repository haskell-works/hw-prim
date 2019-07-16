module HaskellWorks.Control.Monad.Lazy
  ( interleaveSequenceIO
  , interleaveSequenceM
  ) where

import Control.Monad.IO.Unlift

import qualified System.IO.Unsafe as IO

interleaveSequenceIO :: [IO a] -> IO [a]
interleaveSequenceIO []       = return []
interleaveSequenceIO (fa:fas) = do
  a <- fa
  as <- IO.unsafeInterleaveIO (interleaveSequenceIO fas)
  return (a:as)

interleaveSequenceM :: MonadUnliftIO m => [m a] -> m [a]
interleaveSequenceM as = do
  f <- askUnliftIO
  liftIO $ interleaveSequenceIO (fmap (unliftIO f) as)
