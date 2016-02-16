module Model.Entry where

import Import
import qualified Data.HashMap.Strict as H

instance FromJSON (RequestView Entry) where
  parseJSON = liftM RequestView . parseJSON

instance ToJSON (ResponseView (Entity Entry)) where
  toJSON (ResponseView (Entity eid e)) =
    Object $
    H.insert "id" (toJSON eid) $ o
    where (Object o) = toJSON e

instance Validatable Entry where
  validations e = [ ((entryEnd e) > (entryStart e), MsgEntryEndDateEarlierThanStart) ]

-- | Writable properties that handlers should respect.
entryWhitelist :: Maybe [Text]
entryWhitelist = whitelist ["start", "end", "note"]
