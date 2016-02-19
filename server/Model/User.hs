module Model.User where

import Import
import Model.Role
import qualified Data.HashMap.Strict as H
import qualified Text.Email.Validate as EV

instance ToJSON (ResponseView (Entity User)) where
  toJSON (ResponseView (Entity uid u)) =
    Object $
    H.insert "id" (toJSON uid) $
    H.delete "password" o
   where (Object o) = toJSON u

instance Updatable (Entity User) where
  updatableProperties Nothing = return []
  updatableProperties (Just (Entity tuid _)) = do
    Entity cuid cu <- requireCachedAuthenticatedUser
    let roles = unRoles . userRoles $ cu
    return $ bool [] ["password"]          (tuid == cuid)
          <> bool [] ["roles", "password"] (Admin `elem` roles)
    
instance Validatable User where
  validations = return . runRules [
    rule MsgInvalidEmailAddress $ EV.isValid . encodeUtf8 . userEmail ]
