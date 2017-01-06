{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BLI
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Map.Syntax 
import Heist
import qualified Heist.Interpreted as I
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Util.FileServe
import Network.Wreq
import Control.Lens
import Text.HTML.TagSoup
import Text.XmlHtml hiding (render)
import Control.Monad.IO.Class

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

-------------------------------------------------------------------------------
-- | Read Goodreads key and password 
goodreadsKey :: IO String
goodreadsKey = fmap (takeWhile (/= '\n')) $ readFile "src/.goodreadskey"

goodreadsPass :: IO String
goodreadsPass = fmap (dropWhile (/= '\n')) $ readFile "src/.goodreadskey"

-------------------------------------------------------------------------------
-- | sample goodread response file name
goodreadsResFilename :: FilePath
goodreadsResFilename = "src/.goodreadsresponse"

-- | Use wreq to save response body from Goodreads 
saveGoodreadsResponseBody :: IO ()
saveGoodreadsResponseBody = do
  key <- goodreadsKey
  r <-  get ("https://www.goodreads.com/review/list/5285276.xml?key=" ++ key ++ "&v=2&shelf=read&sort=date_read&per_page=200")
  BLI.writeFile goodreadsResFilename (r ^. responseBody)

-- | A single review
sampleBookReview :: IO [Tag String]
sampleBookReview = do
  tags <- parseTags <$> readFile goodreadsResFilename
  let br = takeWhile (~/= ("</review>"::String)) $ dropWhile (~/= ("<review>"::String)) tags
  return br

-- | All reviews inside a response
bookReviews :: IO [[Tag String]]
bookReviews = do
  tags <- parseTags <$> readFile goodreadsResFilename
  let brs = go [] tags
  return $ reverse brs 
  where go bs [] = bs
        go bs tags = let br = takeWhile (~/= ("</review>"::String)) $ drop 1 $ dropWhile (~/= ("<review>"::String)) tags
                         remain = drop 1 $ dropWhile (~/= ("</review>"::String)) tags
                      in if (length br == 0) then go bs remain else go (br : bs) remain

-- | A single book to test
sampleBook :: IO Book
sampleBook = do
  br <- sampleBookReview
  return $ parseReview br
  
-------------------------------------------------------------------------------
-- | Bood data type
data Book = Book
  { title :: String
  , image_url :: String
  , description :: String
  , author :: String
  , comment :: String
  } deriving (Eq, Show)

-- | parse a review to get book data
parseReview :: [Tag String] -> Book
parseReview rev =
  let t = case takeWhile (~/= ("</title>" :: String)) $ drop 1 $ dropWhile (~/= ("<title>"::String)) rev of
        [ ] -> ""
        t : _ -> case maybeTagText t of
          Nothing -> ""
          Just s -> (s :: String)
      iu = case takeWhile (~/= ("</image_url>" :: String)) $ drop 1 $ dropWhile (~/= ("<image_url>"::String)) rev of
        [ ] -> ""
        t : _ -> case maybeTagText t of
          Nothing -> ""
          Just s -> (s :: String)
      d = case takeWhile (~/= ("</description>" :: String)) $ drop 1 $ dropWhile (~/= ("<description>"::String)) rev of
        [ ] -> ""
        t : _ -> case maybeTagText t of
          Nothing -> ""
          Just s -> (s :: String)
      a = case takeWhile (~/= ("</name>" :: String)) $ drop 1 $ dropWhile (~/= ("<name>"::String)) rev of
        [ ] -> ""
        t : _ -> case maybeTagText t of
          Nothing -> ""
          Just s -> (s :: String)
      c = case takeWhile (~/= ("</body>" :: String)) $ drop 1 $ dropWhile (~/= ("<body>"::String)) rev of
        [ ] -> ""
        t : _ -> case maybeTagText t of
          Nothing -> ""
          Just s -> (s :: String)

  in Book t iu d a c 

-- | test
books :: [Book]
books = [
  Book {
      title = "Learn You a Haskell",
      image_url = " ",
      description = "Beginner book in Haskell",
      author = "Miran Lipova",
      comment = "a good beginner book"
      },
  Book {
      title = "Parallel and Concurrent Programming in Haskell",
      image_url = " ",
      description = "Good book in parallel and concurrent Haskell",
      author = "Simon Marlow",
      comment = "nice, short book"
      }
  ] 
      

bookHandler :: Handler App App ()
bookHandler = do
  liftIO saveGoodreadsResponseBody
  brs <- liftIO bookReviews
  let bs = map parseReview brs
  renderWithSplices "book" (allBooksSplices bs)

allBooksSplices :: [Book] -> Splices (SnapletISplice App)
allBooksSplices bs = "allBooks" ## (renderBooks bs)

renderBooks :: [Book] -> SnapletISplice App
-- renderBooks = I.mapSplices $ I.runChildrenWith . splicesFromBook
renderBooks = I.mapSplices $ I.runChildrenWithText . splicesFromBook2


splicesFromBook ::Monad n => Book -> Splices (I.Splice n)
splicesFromBook b = do
  "bookTitle" ## I.textSplice (T.pack $ title b)
  "bookImageUrl" ## I.textSplice (T.pack $ image_url b)
  "bookDescription" ## I.textSplice (T.pack $ description b)
  "bookAuthor" ## I.textSplice (T.pack $ author b)
  "bookComment" ## I.textSplice (T.pack $ comment b)

splicesFromBook2 :: Book -> Splices T.Text
splicesFromBook2 b = do
  "bookTitle" ## T.pack $ stripTags $ title b
  "bookImageUrl" ## T.pack $ stripTags $ image_url b
  "bookDescription" ## T.pack $ stripTags $ description b
  "bookAuthor" ## T.pack $ stripTags $ author b
  "bookComment" ## T.pack $ stripTags $ comment b

-- work around to strip tags
stripTags :: String -> String
stripTags s = foldr (++) [] $ map fromTagText $ filter pred $ parseTags s 
  where pred (TagOpen _ _) = False
        pred (TagClose _) = False
        pred _ = True
        
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/book", bookHandler)
         , ("media", serveDirectory "static/media")]

