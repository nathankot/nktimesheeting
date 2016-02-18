module TestFactory where

import ClassyPrelude
import Database.Persist.Sql (SqlPersistM)
import Model
import Model.Role
import Database.Persist hiding (get)
import Database.Persist.Sql (SqlBackend)
import Data.Time.Clock (addUTCTime)

factoryEntry :: Entity User ->             -- ^ The entry owner
                (Entry -> Entry) -> -- ^ Chance to change the entry
                SqlPersistM (Entity Entry)

factoryEntry (Entity uid _) transform = do
  start <- liftIO getCurrentTime
  let end = addUTCTime 3600 start
  let entry = transform Entry { entryUserId = uid
                              , entryStart = start
                              , entryEnd = end
                              , entryNote = ("Hello there" :: Text)
                              }
  eid <- insert entry
  return $ Entity eid entry

factoryUser :: (User -> User) -> -- ^ Chance to manipulate the user
               SqlPersistM (Entity User)

factoryUser transform = do
  let user = transform User { userPassword = Just "notapasswordhash"
                            , userEmail    = "random@email.com"
                            , userRoles    = Roles [Common] }
  uid <- insert user
  return $ Entity uid user
