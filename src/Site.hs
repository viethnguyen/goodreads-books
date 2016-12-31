{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import Control.Applicative
import Data.ByteString (ByteString)
import Data.Map.Syntax (( ## ))
import qualified Data.Text as T
import qualified Heist.Interpreted as I
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Util.FileServe
import Network.Wreq
import Control.Lens

------------------------------------------------------------------------------
import Application

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [("", serveDirectory "static")]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app =
  makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    addRoutes routes
    return $ App h

-------------------------------------------------------------------------------
-- | Read Goodreads key and password 
goodreadsKey :: IO String
goodreadsKey = fmap (takeWhile (/= '\n')) $ readFile "src/.goodreadskey"

goodreadsPass :: IO String
goodreadsPass = fmap (dropWhile (/= '\n')) $ readFile "src/.goodreadskey"

-------------------------------------------------------------------------------
-- | Experimental Wreq query to Goodreads
response :: IO ByteString
response = do
  key <- goodreadsKey
  r <- get query 
  return r
  where query = "https://www.goodreads.com/review/list/5285276.xml?key=" + key +"&v=2?shelf=read"
