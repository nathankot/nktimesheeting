{-# LANGUAGE ConstraintKinds, ScopedTypeVariables #-}
module Model.Extension ( Updatable (..)
                       , Validatable (..)
                       , rule
                       , runRules
                       , requireUpdatedEntity
                       , requireEntity
                       , buildEntityFromJSON )
       where

import ClassyPrelude.Yesod hiding (Proxy)
import Foundation
import Data.Aeson
import Data.Proxy
import Data.Bool (bool)
import qualified Data.HashMap.Strict as H

type YesodSite site = ( Yesod site
                      , YesodPersist site
                      , YesodPersistBackend site ~ SqlBackend
                      , RenderMessage site AppMessage )

type HandlerFor site a = HandlerT site IO a

-- | Represents an entity that has validation logic
class Validatable e where
  -- | Validate and return potential error messages
  validations :: YesodSite site => e -> HandlerFor site [AppMessage]
  validations _ = return []

  -- | Used in the handler to validate the given object or return
  --   an invalid argument response with the respective errors.
  validate :: YesodSite site => e -> HandlerFor site e
  validate e = do
    errors <- validations e
    unless (null errors) $ invalidArgsI errors
    return e

-- | A helper to semantically create validation rules.
rule :: AppMessage -> (e -> Bool) -> e -> Maybe AppMessage
rule m f = bool (Just m) Nothing . f

runRules :: [e -> Maybe AppMessage] -> e -> [AppMessage]
runRules rules e = catMaybes $ (\r -> r e) <$> rules

-- | Allows any json decodable type to be 'updatable',
--   which means that given an existing value, it can
--   update itself by merging in a default whitelist of
--   properties.
class (FromJSON e, ToJSON e) => Updatable e where
  updatableProperties :: YesodSite site => proxy e -> HandlerFor site [Text]

-- | Merge in the request object with an existing entity,
--   taking care to only consider updatable properties.
requireUpdatedEntity :: forall e site.
                        (Updatable e, YesodSite site) =>
                        e ->              -- ^ The existing entity 
                        HandlerFor site e -- ^ The new updated entity
requireUpdatedEntity e = do
  let defaults = toJSON e
  whitelist <- updatableProperties (Proxy :: Proxy e)
  requireEntity (Just  defaults) whitelist

requireEntity :: (FromJSON e, YesodSite site) =>
                 Maybe Value -> -- ^ Default values for the entity
                 [Text]      -> -- ^ Whitelist of allowed properties
                 HandlerFor site e

requireEntity defaults whitelist = do
  new <- requireJsonBody :: HandlerFor site Value
  buildEntityFromJSON defaults whitelist new

buildEntityFromJSON :: (FromJSON e, YesodSite site) =>
                       Maybe Value -> -- ^ Default values for the entity
                       [Text]      -> -- ^ Whitelist of allowed properties
                       Value       -> -- ^ The entity JSON value
                       HandlerFor site e

buildEntityFromJSON defaults' whitelist new = do
  let defaults = fromMaybe (Object H.empty) defaults'
  case fromJSON $ mergeJsonValues whitelist defaults new of
    Success e' -> return e'
    Error err -> invalidArgs $ [pack err]
  
-- | Performs a shallow merge on the given JSON object
mergeJsonValues :: [Text] -> -- ^ Whitelist of allowed keys
                   Value  -> -- ^ Base object
                   Value  -> -- ^ Overriding object
                   Value

mergeJsonValues whitelist (Object defaults) (Object value) =
  Object $ sanitized `H.union` defaults
  where
    sanitized = H.filterWithKey f value
    f k _ = k `elem` whitelist

-- Don't perform anything if we don't have two objects
mergeJsonValues _ b _ = b
