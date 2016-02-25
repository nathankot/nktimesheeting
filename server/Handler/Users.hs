module Handler.Users (postUsersR, getUsersR) where

import Import
import Data.Aeson
import Model.User ()
import Model.ApiKey
import Model.Role
import Handler.Sessions (SessionResponse (..))
import Yesod.Auth.Email (saltPass)

getUsersR :: Handler Value
getUsersR = do
  Entity uid u <- requireCachedAuthenticatedUser
  let roles = unRoles . userRoles $ u
  users <- runDB $ selectList (makeFilters [UserId ==. uid] roles)
           $ []
           :: Handler [Entity User]
  sendResponseStatus status200 $ object [
    "users" .= (ResponseView <$> users) ]
  where
    makeFilters = foldl' $ \filters ->
      bool filters [] . (`elem` [Admin, Manager])
    
postUsersR :: Handler Value
postUsersR = do
  userreq <- validate =<< requireJsonBody :: Handler CreateUserRequest
  salted <- liftIO . saltPass . requestPassword $ userreq
  u <- validate $ User (requestEmail userreq) (Just salted) $ Roles [Common]
  uid <- fromMaybeM (invalidArgsI [MsgEmailTaken]) $ runDB (insertUnique u)
  let user = Entity uid u
  apikey <- runDB $ generateApiKeyForUser user
  sendResponseStatus status201 $ object ["user" .= SessionResponse user apikey]

-- | User passwords need extra processing, so we use this
--   intermediate type first.
data CreateUserRequest = CreateUserRequest { requestEmail     :: Text
                                           , requestPassword  :: Text }

instance Validatable CreateUserRequest where
  validations = return . runRules [
    rule (MsgPasswordTooShort 6) $ (6<=) . length . requestPassword ]

instance FromJSON CreateUserRequest where
  parseJSON = withObject "user" $ \o ->
    CreateUserRequest
    <$> (toLower <$> o .: "email")
    <*> o .: "password"
