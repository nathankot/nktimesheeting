-- | Some tools for handlers
module Handler.Extension ( ResponseView (..)
                         , fromMaybeM
                         ) where

import ClassyPrelude.Yesod

newtype ResponseView e = ResponseView e

-- | Unwrap a maybe wrapped in a monad. Useful for getting
--   I/O results whilst fallbacking to a response/value
--   This differs from @liftM2 fromMaybe@ because it's lazy.
fromMaybeM :: Monad m =>  m a -> m (Maybe a) -> m a
fromMaybeM e h = do
  h' <- h
  case h' of
    Just a -> return a
    Nothing -> e
