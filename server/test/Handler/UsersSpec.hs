module Handler.UsersSpec (spec) where

import TestImport
import Model.Role
import Model.User ()
import Data.List ((!!))

spec :: Spec
spec = withApp $ postUsersSpec >> getUsersSpec

getUsersSpec :: SpecWith App
getUsersSpec = describe "getUsersR" $ do

  it "returns a list of one for normal users" $ do
    user@(Entity _ u) <- runDB $ factoryUser $ id
    makeRequest user
    statusIs 200
    valueSatisfies "only one user" $ \(Object o) ->
      let result = fromJSON $ o ! "users" :: Result [User]
      in case result of
      Error _ -> False
      Success users -> length users == 1 &&
                       userEmail (users !! 0) == userEmail u

  it "returns a list of all users for managers" $ do
    user <- runDB $ factoryUser $ \u -> u { userRoles = Roles [Manager] }
    _ <- runDB $ factoryUser $ \u -> u { userEmail = "other@email.com" }
    makeRequest user
    statusIs 200
    valueSatisfies "all users" $ \(Object o) ->
      let result = fromJSON (o ! "users") :: Result [User]
      in case result of
      Error _ -> False
      Success users -> length users == 2

  it "returns a list of all users for admins" $ do
    user <- runDB $ factoryUser $ \u -> u { userRoles = Roles [Admin] }
    _ <- runDB $ factoryUser $ \u -> u { userEmail = "other@email.com" }
    makeRequest user
    statusIs 200
    valueSatisfies "all users" $ \(Object o) ->
      let result = fromJSON $ o ! "users" :: Result [User]
      in case result of
      Error _ -> False
      Success users -> length users == 2

  where
    makeRequest user = do
      requestJSONWithUser user $ do
        setUrl UsersR
        setMethod "GET"

postUsersSpec :: SpecWith App
postUsersSpec = describe "postUsersR" $ do

  it "creates a user" $ do
    makeRequest
    statusIs 201
    Entity _ u <- runDB $ retrieve $ selectFirst [UserEmail ==. email] []
    assertEqual "correct email" "me@nathankot.com" $ userEmail u

  it "sends back an api key" $ do
    makeRequest
    statusIs 201
    valueSatisfies "Response has api key value" $ \(Object v) ->
      let Object u = v ! "user"
          Object a = u ! "apiKey"
          String s = a ! "value"
      in not (null s)

  it "sends back the user" $ do
    makeRequest
    statusIs 201
    valueSatisfies "Response has users email" $ \(Object v) ->
      let Object u = v ! "user"
          String n = u ! "email"
      in n == email

  it "fails on a malformed email address" $ do
    requestJSON $
      setUrl UsersR >>
      setMethod "POST" >>
      setRequestBody (encode $
                      object [ "email" .= ("notanemail" :: Text)
                             , "password" .= password ])
    statusIs 400

  it "fails on a short email address" $ do
    requestJSON $
      setUrl UsersR >>
      setMethod "POST" >>
      setRequestBody (encode (object
                       [ "email" .= email
                       , "password" .= ("short" :: Text) ]))
    statusIs 400

  where
    email = "me@nathankot.com" :: Text
    password = "testing123" :: Text
    makeRequest = requestJSON $ do
      setUrl UsersR
      setMethod "POST"
      setRequestBody $ encode $ object [ "email"     .= email
                                       , "password"  .= password ]
        
