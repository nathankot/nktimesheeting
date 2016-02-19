{-# LANGUAGE ConstraintKinds #-}
module Handler.Auth ( isUser
                    , authEntry
                    , authEntries
                    , authUser
                    ) where

import ClassyPrelude.Yesod
import Model
import Handler.Cache
import Text.Read (readMaybe)
import Control.Monad.Trans.Maybe
import Model.Role
import Database.Persist.Sql

type YesodSite site = ( Yesod site
                      , YesodPersist site
                      , YesodPersistBackend site ~ SqlBackend )

type HandlerFor site a = HandlerT site IO a

isUser :: YesodSite site => HandlerFor site AuthResult
isUser = do
  muid <- cachedMaybeAuthenticatedUserId
  return $ maybe AuthenticationRequired (const Authorized) muid

authUser :: YesodSite site => Key User -> Bool -> HandlerFor site AuthResult
authUser userId _ = do
  user <- requireCachedAuthenticatedUser
  return $ auth user userId
  where
    auth (Entity uid u) tuid
      | uid == tuid       = Authorized
      | elem Admin roles  = Authorized
      | otherwise         = Unauthorized "Insufficient permissions"
      where roles = unRoles . userRoles $ u

authEntries :: YesodSite site => Bool -> HandlerFor site AuthResult
authEntries isWrite = do
  user@(Entity uid _) <- requireCachedAuthenticatedUser
  targetUID <- (liftM2 fromMaybe) (return uid) $ runMaybeT
               $ (MaybeT $ lookupGetParam "userId")
               >>= MaybeT . return . liftM toSqlKey . readMaybe . unpack
  return $ auth user targetUID
  where
    auth (Entity uid u) tuid
      | uid == tuid                        = Authorized
      | elem Admin roles                   = Authorized
      | not isWrite && elem Manager roles  = Authorized
      | otherwise                          = Unauthorized "Insufficient permissions"
      where roles = unRoles . userRoles $ u

authEntry :: YesodSite site => Key Entry -> Bool -> HandlerFor site AuthResult
authEntry entryId _ = do
  user <- requireCachedAuthenticatedUser
  me <- runDB $ get entryId
  e <- maybe notFound return me
  return $ auth user (Entity entryId e)
  where
    auth (Entity uid u) (Entity _ e)
      | entryUserId e == uid = Authorized
      | elem Admin roles = Authorized
      | otherwise = Unauthorized "Insufficient permissions"
      where roles = unRoles . userRoles $ u
    
