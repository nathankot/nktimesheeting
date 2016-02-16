module Handler.Entry where

import Import
import Model.Entry ()

patchEntryR :: EntryId -> Handler Html
patchEntryR entryId = do
  e' <- fromMaybeM notFound $ runDB (get entryId)
  e <- requireUpdatedJsonEntity (Entity entryId e')
  runDB $ replace entryId e
  sendResponseStatus status200 $ object [
    "entry" .= (ResponseView (Entity entryId e))]

deleteEntryR :: EntryId -> Handler Html
deleteEntryR entryId = do
  runDB $ delete entryId
  sendResponseStatus status204 ()
