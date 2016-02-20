{-# Language CPP #-}
module Settings.StaticFiles where

import ClassyPrelude.Yesod
import Settings     (appStaticDir, compileTimeAppSettings)
import Yesod.EmbeddedStatic

-- This generates easy references to files in the static directory at compile time,
-- giving you compile-time verification that referenced files exist.
-- Warning: any files added to your static directory during run-time can't be
-- accessed this way. You'll have to use their FilePath or URL to access them.
--
-- For example, to refer to @static/js/script.js@ via an identifier, you'd use:
--
--     js_script_js
--
-- If the identifier is not available, you may use:
--
--     StaticFile ["js", "script.js"] []

#if DEVELOPMENT
#define DEV_BOOL True
#else
#define DEV_BOOL False
#endif
       
mkEmbeddedStatic DEV_BOOL "embeddedStatic" [
  embedDir (appStaticDir compileTimeAppSettings)]
