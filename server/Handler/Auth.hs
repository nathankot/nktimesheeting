{-# LANGUAGE ConstraintKinds #-}
module Handler.Auth ( isUser
                    , authEntry
                    ) where

import ClassyPrelude.Yesod
import Model
import Handler.Cache

type YesodSite site = ( Yesod site
                      , YesodPersist site
                      , YesodPersistBackend site ~ SqlBackend )

type HandlerFor site a = HandlerT site IO a

isUser :: YesodSite site => HandlerFor site AuthResult
isUser = do
  muid <- cachedMaybeAuthenticatedUserId
  return $ maybe AuthenticationRequired (const Authorized) muid

authEntry :: YesodSite site => Key Entry -> Bool -> HandlerFor site AuthResult
authEntry entryId isWrite = do
  uid <- requireCachedAuthenticatedUserId
  mEntry <- runDB $ get entryId
  entry <- maybe notFound return mEntry
  if entryUserId entry == uid
    then return Authorized
    else return $ Unauthorized "Insufficient permissions"
