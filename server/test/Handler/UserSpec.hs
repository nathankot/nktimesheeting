module Handler.UserSpec (spec) where

import TestImport
import Model.User ()
import Model.Role

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
