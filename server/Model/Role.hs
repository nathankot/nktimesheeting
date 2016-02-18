{-# LANGUAGE BinaryLiterals #-}
module Model.Role ( Role (..)
                  , Roles (..)
                  , toOctet
                  , hasRole
                  , rolesToOctet
                  , rolesFromOctet
                  )
       where

import ClassyPrelude.Yesod
import Database.Persist.Sql
import Data.Int
import Data.Bits
import Text.Read (readMaybe)

data Role = Common
          | Manager
          | Admin
          deriving (Eq, Show, Read)

newtype Roles = Roles { getRoles :: [Role] }

instance PersistField Roles where
  toPersistValue = toPersistValue . rolesToOctet . getRoles
  fromPersistValue v = Roles . rolesFromOctet <$> fromPersistValue v

instance PersistFieldSql Roles where
  sqlType _ = SqlInt32

instance ToJSON Roles where
  toJSON = toJSON . getRoles

instance FromJSON Roles where
  parseJSON v = Roles <$> parseJSON v

instance ToJSON [Role] where
  toJSON = toJSON . map show

instance FromJSON [Role] where
  parseJSON v = catMaybes <$> fmap readMaybe <$> parseJSON v

availableRoles :: [Role]
availableRoles = [Common, Manager, Admin]
  
hasRole :: Role -> Int8 -> Bool
hasRole r = elem r . rolesFromOctet

rolesToOctet :: [Role] -> Int8
rolesToOctet = foldl' (.|.) 0b00000000 . fmap toOctet

rolesFromOctet :: Int8 -> [Role]
rolesFromOctet i = [ x | x <- availableRoles
                   , (toOctet x) .&. i == (toOctet x) ]

toOctet :: Role -> Int8
toOctet Common  = 0b00000000
toOctet Manager = 0b00000001
toOctet Admin   = 0b00000010
