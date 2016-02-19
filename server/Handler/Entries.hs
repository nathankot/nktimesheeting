module Handler.Entries where

import Import hiding (Proxy)
import Model.Entry ()
import Control.Monad.Trans.Maybe
import Database.Persist.Sql
import Data.Proxy

getEntriesR :: Handler Value
getEntriesR = do
  filters <- filtersFromQueryParams
  entries <- runDB $ selectList filters [Asc EntryId]
  sendResponseStatus status200 $ object [
    "entries" .= (ResponseView <$> entries) ]
  where
    filtersFromQueryParams :: Handler [Filter Entry]
    filtersFromQueryParams = ((:[]) . (EntryUserId ==.))
      <$> (fromMaybeM requireCachedAuthenticatedUserId
           $ runMaybeT
           $ unpack <$> (MaybeT $ lookupGetParam "userId")
           >>= MaybeT . return . liftM toSqlKey . readMaybe)
  
postEntriesR :: Handler Value
postEntriesR = do
  uid <- requireCachedAuthenticatedUserId
  entry <- validate =<<
           requireEntity (Just $ object ["userId" .= uid]) =<<
           updatableProperties (Proxy :: Proxy Entry)
           :: Handler Entry
  eid <- runDB $ insert entry
  sendResponseStatus status201 $ object [
    "entry" .= (ResponseView (Entity eid entry)) ]
