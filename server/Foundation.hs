module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Yesod.Core.Types     (Logger)
import qualified Handler.Auth as Auth
import qualified Yesod.Core.Unsafe as Unsafe

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFileNoCheck "config/routes")

-- Internationalization
mkMessage "App" "messages" "en"

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRelative

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = return Nothing

    -- CSRF has been turned off, because we are using Bearer token authentication as opposed to
    -- session/cookie based authentication which does not have the same cross site request vulnerabilities
    -- i.e browsers will not implicitly create the correct Authorization header based on the stored cookie.
    -- yesodMiddleware = defaultCsrfMiddleware . defaultYesodMiddleware
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout widget = do
      pc <- widgetToPageContent widget
      withUrlRenderer $ pageBody pc

    -- The page to be redirected to when authentication is required.
    authRoute _ = Nothing

    -- Routes not requiring authentication.
    isAuthorized HomeR _                  = return Authorized
    isAuthorized FaviconR _               = return Authorized
    isAuthorized RobotsR _                = return Authorized
    isAuthorized SessionsR _              = return Authorized
    isAuthorized UsersR _                 = return Authorized
    isAuthorized (UserR userId) isWrite   = Auth.authUser userId isWrite 
    isAuthorized EntriesR isWrite         = Auth.authEntries isWrite
    isAuthorized (EntryR entryId) isWrite = Auth.authEntry entryId isWrite
    -- By default check that we have a user
    isAuthorized _ _                      = Auth.isUser

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent _ _ _ = return Nothing

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
