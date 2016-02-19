module Handler.EntrySpec (spec) where

import TestImport
import Data.Time.Clock (addUTCTime)
import Model.Entry ()
import Model.Role
import qualified Database.Persist as DB

spec :: Spec
spec = withApp $ patchEntrySpec >> deleteEntrySpec

patchEntrySpec :: SpecWith App
patchEntrySpec = describe "patchEntryR" $ do

  it "fails with the wrong user" $ do
    user <- runDB $ factoryUser id
    otheruser <- runDB $ factoryUser $ \u -> u { userEmail = "other@email.com" }
    entry <- runDB $ factoryEntry user id
    makeRequest otheruser entry $ \e -> e { entryNote = "New note" }
    statusIs 403

  it "fails for managers" $ do
    user <- runDB $ factoryUser id
    otheruser <- runDB $ factoryUser $ \u -> u { userEmail = "other@email.com"
                                               , userRoles = Roles [Common, Manager] }
    entry <- runDB $ factoryEntry user id
    makeRequest otheruser entry $ \e -> e { entryNote = "New note" }
    statusIs 403

  it "allows admins to make changes" $ do
    user <- runDB $ factoryUser id
    otheruser <- runDB $ factoryUser $ \u -> u { userEmail = "other@email.com"
                                               , userRoles = Roles [Common, Admin] }
    entry <- runDB $ factoryEntry user id
    makeRequest otheruser entry $ \e -> e { entryNote = "New note" }
    statusIs 200

  it "can update the entry" $ do
    user <- runDB $ factoryUser id
    entry@(Entity eid e') <- runDB $ factoryEntry user id
    start <- liftIO getCurrentTime
    let end = addUTCTime 3600 start
    makeRequest user entry $ \e -> e { entryNote   = "New note"
                                     , entryStart  = start
                                     , entryEnd    = end }
    statusIs 200
    e <- runDB $ getJust eid
    assertEqual "note has been updated" "New note" (entryNote e)
    boolIsFalse "start has been updated" $ (entryStart e') == (entryStart e)
    boolIsFalse "end has been updated" $ (entryEnd e') == (entryEnd e)

  it "responds with the updated entry" $ do
    user <- runDB $ factoryUser id
    start <- liftIO getCurrentTime
    let end = addUTCTime 3600 start
    entry <- runDB $ factoryEntry user id
    makeRequest user entry $ \e -> e { entryNote   = "New note"
                                     , entryStart  = start
                                     , entryEnd    = end }
    statusIs 200
    valueSatisfies "entry returned with new values" $ \(Object o) ->
      let decoded = fromJSON (o ! "entry") :: Result Entry
      in case decoded of Error _   -> False
                         Success e -> entryNote e == "New note"

  where
    makeRequest user (Entity eid e) transform =
      requestJSONWithUser user $ do
        setUrl (EntryR eid)
        setMethod "PATCH"
        setRequestBody . encode .  toJSON $ transform e

deleteEntrySpec :: SpecWith App
deleteEntrySpec = describe "deleteEntryR" $ do

  it "succeeds in removing the entry" $ do
    user <- runDB $ factoryUser id
    entry@(Entity eid _) <- runDB $ factoryEntry user id
    makeRequest user entry
    statusIs 204
    mEntry <- runDB $ DB.get eid
    boolIsTrue "entry no longer exists" $ isNothing mEntry

  it "fails if the user is not authorized" $ do
    user <- runDB $ factoryUser id
    entry <- runDB $ factoryEntry user id
    other <- runDB $ factoryUser $ \u -> u { userEmail = "other@email.com" }
    makeRequest other entry
    statusIs 403

  it "fails for managers" $ do
    user <- runDB $ factoryUser id
    otheruser <- runDB $ factoryUser $ \u -> u { userEmail = "other@email.com"
                                               , userRoles = Roles [Common, Manager] }
    entry <- runDB $ factoryEntry user id
    makeRequest otheruser entry
    statusIs 403

  it "allows admins to delete entries" $ do
    user <- runDB $ factoryUser id
    otheruser <- runDB $ factoryUser $ \u -> u { userEmail = "other@email.com"
                                               , userRoles = Roles [Common, Admin] }
    entry <- runDB $ factoryEntry user id
    makeRequest otheruser entry
    statusIs 204

  where
    makeRequest user (Entity eid _) =
      requestJSONWithUser user $ do
        setUrl (EntryR eid)
        setMethod "DELETE"
