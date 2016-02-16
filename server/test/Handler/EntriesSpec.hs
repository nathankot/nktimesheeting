module Handler.EntriesSpec (spec) where

import TestImport
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
    user@(Entity uid _) <- setupEntries
    makeRequest user
    statusIs 200
    valueSatisfies "there are 5 entries" $ \(Object o) ->
      let decoded = fromJSON (o ! "entries") :: Result [Entry]
      in case decoded of
      Error _ -> False
      Success entries ->
        length entries == 5 &&
        all ((==uid) . entryUserId) entries

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

      return user

    makeRequest user = do
      requestJSONWithUser user $ do
        setUrl EntriesR
        setMethod "GET"

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
