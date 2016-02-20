module Model.Entry where

import Import
import qualified Data.HashMap.Strict as H
import Model.Role

instance ToJSON (ResponseView (Entity Entry)) where
  toJSON (ResponseView (Entity eid e)) =
    Object $
    H.insert "id" (toJSON eid) $ o
    where (Object o) = toJSON e

instance Validatable Entry where
  validations = return . runRules [
    rule MsgEntryEndDateEarlierThanStart $ (\(e') -> entryEnd e' > entryStart e') ]

instance Updatable (Entity Entry) where
  -- When updating, never allow changing the user id
  updatableProperties (Just _) = return ["start", "end", "note"]
  -- However admins can create entries for other users
  updatableProperties Nothing = do
    Entity _ cu <- requireCachedAuthenticatedUser
    let roles = unRoles . userRoles $ cu
    let updatable = ["start", "end", "note"]
    bool (return updatable) (return $ updatable ++ ["userId"]) $ Admin `elem` roles
