module Handler.EntriesSpec (spec) where

import TestImport
import Model.Role
import Database.Persist.Sql
import Data.Time.Clock (addUTCTime)

spec :: Spec
spec = withApp $
       getEntriesSpec >>
       postEntriesSpec

getEntriesSpec :: SpecWith App
getEntriesSpec = describe "getEntriesR" $ do

  it "returns unauthorized when not logged in" $ do
    _ <- setupEntries
    requestJSON $ setUrl EntriesR >> setMethod "GET"
    statusIs 401

  it "returns all entries of this user by default" $ do
    (user@(Entity uid _), _) <- setupEntries
    makeRequest user user
    statusIs 200
    valueSatisfies "there are 5 entries" $ \(Object o) ->
      let decoded = fromJSON (o ! "entries") :: Result [Entry]
      in case decoded of
      Error _ -> False
      Success entries ->
        length entries == 5 &&
        all ((==uid) . entryUserId) entries

  it "forbids other users to view entries" $ do
    (user, other) <- setupEntries
    makeRequest other user
    statusIs 403

  it "allows other managers to view entries" $ do
    (user, other@(Entity oid _)) <- setupEntries
    runDB $ update oid [UserRoles =. Roles [Common, Manager]]
    makeRequest other user
    statusIs 200
    valueSatisfies "entries are not the other user's" $ \(Object o) ->
      let decoded = fromJSON (o ! "entries") :: Result [Entry]
          check (Error _) = False
          check (Success entries) = all ((/=oid) . entryUserId) entries
      in check decoded
    
  where
    setupEntries = do
      user <- runDB $ factoryUser id

      -- Make 5 different entries at different times, including an entry yesterday
      -- and the day before yesterday.
      sequence_ . flip map [3600, 7200, 9900, (-86400) * 2 + 3600, (-86400) + 3600] $
        \a -> runDB $ factoryEntry user $ \e ->
        let add = addUTCTime a
        in e { entryStart = add (entryStart e)
             , entryEnd   = add (entryEnd e) }

      -- And make entries for another unrelated user
      otheruser <- runDB $ factoryUser $ \u -> u { userEmail = "other@email.com" }
      sequence_ . flip map [3600, 7200, 9900, (-86400) * 2 + 3600, (-86400) + 3600] $
        \a -> runDB $ factoryEntry otheruser $ \e ->
        let add = addUTCTime a
        in e { entryStart = add (entryStart e)
             , entryEnd   = add (entryEnd e) }

      return (user, otheruser)

    makeRequest user (Entity euid _) = do
      requestJSONWithUser user $ do
        setUrl EntriesR
        setMethod "GET"
        addGetParam "userId" $ pack . show . unSqlBackendKey . unUserKey $ euid

postEntriesSpec :: SpecWith App
postEntriesSpec = describe "postEntriesR" $ do

  it "returns unauthorized when not logged in" $ do
    start <- liftIO getCurrentTime
    let end = addUTCTime 3600 start
    requestJSON $ do
      setUrl EntriesR
      setMethod "POST"
      setRequestBody $ encode $ object [ "start" .= toJSON start
                                       , "end"   .= toJSON end
                                       , "note"  .= ("Hello there 123" :: Text)
                                       ]
    statusIs 401

  it "creates entries for the user successfully" $ do
    makeRequest
    statusIs 201
    entries <- runDB $ selectList [] [] :: YesodExample App [Entity Entry]
    boolIsTrue "Entry created" $ length entries > 0

  it "fails if the start date is greater than the end date" $ do
    user <- runDB $ factoryUser id
    now <- liftIO getCurrentTime
    let start = addUTCTime 3600 now
    makeRequestWithTimes user start now
    statusIs 400

  where
    makeRequestWithTimes user start end = requestJSONWithUser user $ do
      setUrl EntriesR
      setMethod "POST"
      setRequestBody $ encode $ object [ "start" .= toJSON start
                                       , "end"   .= toJSON end
                                       , "note"  .= ("Hello there 123" :: Text)
                                       ]

    makeRequest = do
      user <- runDB $ factoryUser id
      now <- liftIO getCurrentTime
      let end = addUTCTime 3600 now
      makeRequestWithTimes user now end
