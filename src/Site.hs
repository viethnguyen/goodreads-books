{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BLI
import Data.Map.Syntax
import qualified Data.Text as T
import Heist
import qualified Heist.Interpreted as I
import Network.Wreq
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Util.FileServe
import System.Directory
import Text.HTML.TagSoup
import Text.XmlHtml hiding (render)
import System.Environment

------------------------------------------------------------------------------
import Application

------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app =
  makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    addRoutes routes
    return $ App h

-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [("/book", bookHandler), ("media", serveDirectory "static/media")]

-------------------------------------------------------------------------------
-- | Read Goodreads key and password from a file
goodreadsKey :: IO String
goodreadsKey = getEnv "GOODREADS_KEY"

-- | Bood data type
data Book = Book
  { title :: String
  , image_url :: String
  , description :: String
  , author :: String
  , comment :: String
  , link :: String
  } deriving (Eq, Show)

-- | goodread response file name
goodreadsResFilename :: FilePath
goodreadsResFilename = "src/.goodreadsresponse"

-- | Use wreq to save response body from Goodreads into a file
saveGoodreadsResponseBody :: IO ()
saveGoodreadsResponseBody
  -- Get Goodreads key
 = do
  key <- goodreadsKey
  -- Get all the read books
  r <-
    get
      ("https://www.goodreads.com/review/list/5285276.xml?key=" ++
       key ++ "&v=2&shelf=read&sort=date_read&per_page=200")
  BLI.writeFile goodreadsResFilename (r ^. responseBody)

-- | For testing, a sample book review. A book review is a collection of TagSoup tags between the two tags <review> and </review>
sampleBookReview :: IO [Tag String]
sampleBookReview = do
  tags <- parseTags <$> readFile goodreadsResFilename
  let br =
        takeWhile (~/= ("</review>" :: String)) $
        dropWhile (~/= ("<review>" :: String)) tags
  return br

-- | All reviews from the file in which we save the Goodreads response.
getBookReviews :: IO [[Tag String]]
getBookReviews = do
  tags <- parseTags <$> readFile goodreadsResFilename
  let brs = go [] tags
  return $ reverse brs
  where
    go bs [] = bs
    go bs tags =
      let br =
            takeWhile (~/= ("</review>" :: String)) $
            drop 1 $ dropWhile (~/= ("<review>" :: String)) tags
          remain = drop 1 $ dropWhile (~/= ("</review>" :: String)) tags
      in if (length br == 0)
           then go bs remain
           else go (br : bs) remain

-- | A single book to test
sampleBook :: IO Book
sampleBook = do
  br <- sampleBookReview
  return $ parseReview br

-- | parse a review to get book data
parseReview :: [Tag String] -> Book
parseReview rev =
  let t =
        case takeWhile (~/= ("</title>" :: String)) $
             drop 1 $ dropWhile (~/= ("<title>" :: String)) rev of
          [] -> ""
          t:_ ->
            case maybeTagText t of
              Nothing -> ""
              Just s -> (s :: String)
      iu =
        case takeWhile (~/= ("</image_url>" :: String)) $
             drop 1 $ dropWhile (~/= ("<image_url>" :: String)) rev of
          [] -> ""
          t:_ ->
            case maybeTagText t of
              Nothing -> ""
              Just s -> (s :: String)
      d =
        case takeWhile (~/= ("</description>" :: String)) $
             drop 1 $ dropWhile (~/= ("<description>" :: String)) rev of
          [] -> ""
          t:_ ->
            case maybeTagText t of
              Nothing -> ""
              Just s -> (s :: String)
      a =
        case takeWhile (~/= ("</name>" :: String)) $
             drop 1 $ dropWhile (~/= ("<name>" :: String)) rev of
          [] -> ""
          t:_ ->
            case maybeTagText t of
              Nothing -> ""
              Just s -> (s :: String)
      c =
        case takeWhile (~/= ("</body>" :: String)) $
             drop 1 $ dropWhile (~/= ("<body>" :: String)) rev of
          [] -> ""
          t:_ ->
            case maybeTagText t of
              Nothing -> ""
              Just s -> (s :: String)
      l =
        case takeWhile (~/= ("</link>" :: String)) $
             drop 1 $ dropWhile (~/= ("<link>" :: String)) rev of
          [] -> ""
          t:_ ->
            case maybeTagText t of
              Nothing -> ""
              Just s -> (s :: String)
  in Book t iu d a c l

-- | test data, a list of Books
books :: [Book]
books =
  [ Book
    { title = "Learn You a Haskell"
    , image_url = " "
    , description = "Beginner book in Haskell"
    , author = "Miran Lipova"
    , comment = "a good beginner book"
    , link = ""
    }
  , Book
    { title = "Parallel and Concurrent Programming in Haskell"
    , image_url = " "
    , description = "Good book in parallel and concurrent Haskell"
    , author = "Simon Marlow"
    , comment = "nice, short book"
    , link = ""
    }
  ]

-- | handler
bookHandler :: Handler App App ()
bookHandler = do
  liftIO saveGoodreadsResponseBody
  brs <- liftIO getBookReviews
  liftIO $ removeFile goodreadsResFilename
  let bs = map parseReview brs
  renderWithSplices "book" (allBooksSplices bs)

-- | convert a list of books to splices
allBooksSplices :: [Book] -> Splices (SnapletISplice App)
allBooksSplices bs = "allBooks" ## (renderBooks bs)

renderBooks :: [Book] -> SnapletISplice App
renderBooks = I.mapSplices $ I.runChildrenWith . splicesFromBook

splicesFromBook
  :: Monad n
  => Book -> Splices (I.Splice n)
splicesFromBook b = do
  "bookTitle" ## I.textSplice (T.pack $ stripTags $ title b)
  "bookImageUrl" ## I.textSplice (T.pack $ stripTags $ image_url b)
  "bookDescription" ## I.textSplice (T.pack $ stripTags $ description b)
  "bookAuthor" ## I.textSplice (T.pack $ stripTags $ author b)
  "bookComment" ## I.textSplice (T.pack $ stripTags $ comment b)
  "bookLink" ## I.textSplice (T.pack $ stripTags $ link b)

-- | strip all HTML tags inside a string
stripTags :: String -> String
stripTags s = foldr (++) [] $ map fromTagText $ filter pred $ parseTags s
  where
    pred (TagOpen _ _) = False
    pred (TagClose _) = False
    pred _ = True
