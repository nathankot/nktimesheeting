module Handler.User ( patchUserR
                    , deleteUserR ) where

import Import
import Model.User ()
import Yesod.Auth.Email (saltPass)
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.HashMap.Strict as H

patchUserR :: UserId -> Handler Html
patchUserR userId = do
  j' <- requireJsonBody :: Handler Value
  u <- fromMaybeM notFound $ runDB $ get userId
  updatable <- updatableProperties $ Just (Entity userId u)

  -- Users are a little bit of an edge case because of the extra one-way
  -- processing of the password. So we need to do extra work to handle this.
  _ <- runMaybeT $ do
    unless ("password" `elem` updatable) mzero
    newPassword <- MaybeT . return
                   $ flip parseMaybe j'
                   $ withObject "user with updated pasword"
                   (.: "password")
    salted <- liftIO . liftM Just . saltPass $ newPassword
    lift $ runDB $ update userId [UserPassword =. salted]

  let Object obj = j'
  let j = Object $ H.delete "password" obj
  u' <- runDB $ get userId
  u'' <- buildEntityFromJSON (Just . toJSON $ u') updatable j
  runDB $ replace userId u''

  sendResponseStatus status200 $ object [
    "user" .= (ResponseView (Entity userId u'')) ]
    
  
deleteUserR :: UserId -> Handler Html
deleteUserR userId = error "Not yet implemented: deleteUserR"
