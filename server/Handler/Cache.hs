{-# LANGUAGE ConstraintKinds #-}
module Handler.Cache ( requireCachedAuthenticatedUserId
                     , CachedAuthenticatedUserId (..)
                     , cachedMaybeAuthenticatedUserId
                     , requireCachedAuthenticatedUser
                     , CachedAuthenticatedUser (..)
                     , cachedMaybeAuthenticatedUser ) where

import ClassyPrelude.Yesod
import Model
import Control.Monad.Trans.Maybe (MaybeT (..))

type YesodSite site = ( Yesod site
                      , YesodPersist site
                      , YesodPersistBackend site ~ SqlBackend )

type HandlerFor site a = HandlerT site IO a

newtype CachedAuthenticatedUser = CachedAuthenticatedUser
                                  { uncachedAuthenticatedUser :: Maybe (Entity User) }
                                deriving Typeable

requireCachedAuthenticatedUser :: YesodSite site => HandlerFor site (Entity User)
requireCachedAuthenticatedUser = do
  user <- cachedMaybeAuthenticatedUser
  maybe notAuthenticated return user

cachedMaybeAuthenticatedUser :: YesodSite site => HandlerFor site (Maybe (Entity User))
cachedMaybeAuthenticatedUser = fmap uncachedAuthenticatedUser $
                               cached $
                               fmap CachedAuthenticatedUser
                               maybeAuthenticatedUser
  where
    maybeAuthenticatedUser = runMaybeT $ do
      uid <- MaybeT $ cachedMaybeAuthenticatedUserId
      user <- MaybeT $ runDB $ get uid
      return $ Entity uid user

newtype CachedAuthenticatedUserId = CachedAuthenticatedUserId
                                    { uncachedAuthenticatedUserId :: Maybe UserId }
                                  deriving Typeable

requireCachedAuthenticatedUserId :: YesodSite site => HandlerFor site UserId
requireCachedAuthenticatedUserId = do
  muid <- cachedMaybeAuthenticatedUserId
  maybe notAuthenticated return muid

-- | Retrieve a cached version of our authenticated user's id.
cachedMaybeAuthenticatedUserId :: YesodSite site => HandlerFor site (Maybe UserId)
cachedMaybeAuthenticatedUserId = fmap uncachedAuthenticatedUserId $
                                 cached $
                                 fmap CachedAuthenticatedUserId
                                 maybeAuthenticatedUserId
  where
    maybeAuthenticatedUserId = runMaybeT $ do
      now <- liftIO getCurrentTime
      s <- MaybeT $ lookupHeader hAuthorization
      token <- MaybeT $ return $ stripPrefix "Bearer " s
      Entity _ a <- MaybeT $ runDB $ getBy $ UniqueApiKey (decodeUtf8 token)
      unless (apiKeyExpires a > now) $ MaybeT $ return Nothing
      return $ apiKeyUserId a
