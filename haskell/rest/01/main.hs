{-# LANGUAGE DeriveGeneric #-}

{-
  Fetches the posts url, sorts them by title
  and display certain records
-}

import Data.Aeson (decode, FromJSON, ToJSON)
import Data.ByteString.Lazy (ByteString)
import Data.List (sortBy)
import Data.Maybe
import Data.Ord (comparing)
import GHC.Generics (Generic)
import Network.HTTP.Conduit (simpleHttp)

-- Import Prelude to avoid "ambiguous occurrence id"
import Prelude (
  String, Show, Int, mapM_, putStrLn, show,
  IO, return, (++), (.), ($))

postsURL :: String
postsURL = "http://jsonplaceholder.typicode.com/posts"

data Post = Post {
  id :: Int,
  userId :: Int,
  title :: String
} deriving (Show, Generic)

instance FromJSON Post
instance ToJSON Post

displayPost :: Post -> String
displayPost p = "Id: " ++ show (id p) ++ "\n"
  ++ "Title: '" ++ (title p) ++ "'\n"
  ++ "User Id: " ++ show (userId p)

printPosts :: [Post] -> IO ()
printPosts = mapM_ $ (\str -> putStrLn $ str ++ "\n") . displayPost

fetchPostsStr :: IO (ByteString)
fetchPostsStr = simpleHttp postsURL

fetchPosts :: IO (Maybe [Post])
fetchPosts = do
  postsStr <- fetchPostsStr
  return (decode postsStr)

sortPostsByTitle :: [Post] -> [Post]
sortPostsByTitle initialPosts = sortBy (comparing title) initialPosts

handlePostsResponse :: Maybe [Post] -> IO ()
handlePostsResponse posts = case posts of
  Nothing -> putStrLn "There was an error reading the JSON data."
  Just postsValue  ->  printPosts $ sortPostsByTitle postsValue

main :: IO ()
main = do
  posts <- fetchPosts
  handlePostsResponse posts
