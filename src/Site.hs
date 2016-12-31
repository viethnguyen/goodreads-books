{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import Data.ByteString.Lazy as BLI (writeFile)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Heist.Interpreted as I
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Util.FileServe
import Network.Wreq
import Control.Lens
import Text.HTML.TagSoup

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
-- | Use wreq to save response body from Goodreads 
saveGoodreadsResponseBody :: IO ()
saveGoodreadsResponseBody = do
  key <- goodreadsKey
  r <-  get ("https://www.goodreads.com/review/list/5285276.xml?key=" ++ key ++ "&v=2?shelf=read")
  BLI.writeFile "src/.goodreadsresponse" (r ^. responseBody)

-- | Experimetal response in tags
sampleResponseInTags = do
  key <- goodreadsKey
  r <-  get ("https://www.goodreads.com/review/list/5285276.xml?key=" ++ key ++ "&v=2?shelf=read")
  return $ parseTags (r ^. responseBody) 

-- | A single review
-- links = do
--   tags <- sampleResponseInTags
--   let l = takeWhile (~/= ("</review>"::BLI.ByteString)) $ dropWhile (~/= ("<review>"::BLI.ByteString)) tags
--   return l
