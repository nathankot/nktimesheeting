module Handler.Entries where

import Import
import Model.Entry

getEntriesR :: Handler Value
getEntriesR = do
  filters <- filtersFromQueryParams
  entries <- runDB $ selectList filters [Asc EntryId]
  sendResponseStatus status200 $ object [
    "entries" .= (ResponseView <$> entries) ]
  where
    filtersFromQueryParams :: Handler [Filter Entry]
    filtersFromQueryParams = do
      uid <- requireCachedAuthenticatedUserId
      return [EntryUserId ==. uid]


postEntriesR :: Handler Value
postEntriesR = do
  uid <- requireCachedAuthenticatedUserId
  RequestView entry <- requireJsonEntity [ "userId" .= uid ] entryWhitelist
                       :: Handler (RequestView  Entry)

  let (isvalid, errors) = validate entry
  unless isvalid $ invalidArgsI errors
  eid <- runDB $ insert entry
  sendResponseStatus status201 $ object [
    "entry" .= (ResponseView (Entity eid entry)) ]
