module Handler.UserSpec (spec) where

import TestImport
import Model.User ()
import Model.Role
import qualified Database.Persist as DB

spec :: Spec
spec = withApp $ patchUserSpec >> deleteUserSpec

patchUserSpec :: SpecWith App
patchUserSpec = describe "patchUserR" $ do

  it "allows a user to update their password" $ do
    user@(Entity _ u) <- runDB $ factoryUser id
    makeRequest user $ [ "password" .= ("newpassword123" :: Text) ]
    statusIs 200
    makeLoginRequest (userEmail u) "newpassword123"
    statusIs 201

  it "can't update other user's passwords" $ do
    user@(Entity _ u) <- runDB $ factoryUser id
    other <- runDB $ factoryUser $ \x -> x { userEmail = "other@email.com" }
    makeRequestWithUserForUser other user $ [ "password" .= ("newpassword123" :: Text) ]
    statusIs 403
    makeLoginRequest (userEmail u) "newpassword123"
    statusIs 401

  it "can't update other user's passwords as a manager" $ do
    user@(Entity _ u) <- runDB $ factoryUser id
    other <- runDB $ factoryUser $ \x -> x { userEmail = "other@email.com"
                                           , userRoles = Roles [Manager, Common] }
    makeRequestWithUserForUser other user $ [ "password" .= ("newpassword123" :: Text) ]
    statusIs 403
    makeLoginRequest (userEmail u) "newpassword123"
    statusIs 401

  it "can update other user passwords as an admin" $ do
    user@(Entity _ u) <- runDB $ factoryUser id
    other <- runDB $ factoryUser $ \x -> x { userEmail = "other@email.com"
                                           , userRoles = Roles [Admin, Common] }
    makeRequestWithUserForUser other user $ [ "password" .= ("newpassword123" :: Text) ]
    statusIs 200
    makeLoginRequest (userEmail u) "newpassword123"
    statusIs 201

  it "doesn't allow common users to update roles" $ do
    user@(Entity uid _) <- runDB $ factoryUser id
    makeRequest user $ [ "roles" .= ["Admin" :: Text] ]
    statusIs 200 -- It should silently fail 
    u <- runDB $ retrieve $ DB.get uid
    boolIsFalse "roles do not have admin"
      $ (Admin `elem`) . unRoles . userRoles $ u

  it "doesn't allow managers to update roles" $ do
    user@(Entity uid _) <- runDB $ factoryUser
                           $ \u -> u { userRoles = Roles [Manager] }
    makeRequest user $ [ "roles" .= ["Admin" :: Text] ]
    statusIs 200 -- It should silently fail 
    u <- runDB $ retrieve $ DB.get uid
    boolIsFalse "roles do not have admin"
      $ (Admin `elem`) . unRoles . userRoles $ u

  it "allows admins to update their own role" $ do
    user@(Entity uid _) <- runDB $ factoryUser
                           $ \u -> u { userRoles = Roles [Admin] }
    makeRequest user $ [ "roles" .= (["Manager", "Admin"] :: [Text]) ]
    statusIs 200 -- It should silently fail 
    u <- runDB $ retrieve $ DB.get uid
    boolIsTrue "roles has the existing admin role"
      $ (Admin `elem`) . unRoles . userRoles $ u
    boolIsTrue "roles has the new manager role"
      $ (Manager `elem`) . unRoles . userRoles $ u

  it "allows admins to update other user roles" $ do
    user <- runDB $ factoryUser $ \u -> u { userRoles = Roles [Admin] }
    other@(Entity uid _) <- runDB $ factoryUser $ \u -> u { userEmail = "other@email.com"}
    makeRequestWithUserForUser user other $ [ "roles" .= (["Manager"] :: [Text]) ]
    statusIs 200
    u <- runDB $ retrieve $ DB.get uid
    boolIsTrue "roles has the new manager role"
      $ (Manager `elem`) . unRoles . userRoles $ u
    
  where
    makeRequestWithUserForUser user (Entity tid _) body = do
      requestJSONWithUser user $ do
        setUrl $ UserR tid
        setMethod "PATCH"
        setRequestBody $ encode $ object $ body

    makeRequest user = makeRequestWithUserForUser user user

    makeLoginRequest :: Text -> Text -> YesodExample App ()
    makeLoginRequest email password = do
      requestJSON $ do
        setUrl $ SessionsR
        setMethod "POST"
        setRequestBody $ encode $ object [ "email" .= email
                                         , "password" .= password ]

deleteUserSpec :: SpecWith App
deleteUserSpec = describe "deleteUserR" $ return ()
