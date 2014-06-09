module Court.Utils
  ( createPipeHandles
  ) where

import System.IO
import System.Posix.IO

createPipeHandles :: IO (Handle, Handle)
createPipeHandles = do
  (r, w) <- createPipe
  r' <- fdToHandle r
  w' <- fdToHandle w
  return (r', w')
