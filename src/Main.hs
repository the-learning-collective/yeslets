{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Exception        (SomeException (..), catch)
import           Control.Logging
import           Data.Monoid
import           Network.Wai.Handler.Warp
import           System.Environment

import           Site

main :: IO ()
main = withStdoutLogging $
       do port <- maybe 8000 read <$> lookupEnv "PORT"
          log' "Starting server..."
          app' <- app
          catch (run port app')
                (\(_ :: SomeException) ->
                        log' "Shutting down...")
