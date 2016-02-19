module Model.Entry where

import Import
import qualified Data.HashMap.Strict as H

instance ToJSON (ResponseView (Entity Entry)) where
  toJSON (ResponseView (Entity eid e)) =
    Object $
    H.insert "id" (toJSON eid) $ o
    where (Object o) = toJSON e

instance Validatable Entry where
  validations = return . runRules [
    rule MsgEntryEndDateEarlierThanStart $ (\(e') -> entryEnd e' > entryStart e') ]

instance Updatable (Entity Entry) where
  updatableProperties _ = return ["start", "end", "note"]
