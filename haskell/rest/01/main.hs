{-# LANGUAGE DeriveGeneric #-}

{-
  Fetches the posts url, sorts them by title
  and display certain records
-}

import Data.Aeson (decode, FromJSON, ToJSON)
import Data.ByteString.Lazy (ByteString)
import Data.List (sortBy)
import Data.Ord (comparing)
import GHC.Generics (Generic)
import Network.HTTP.Conduit (simpleHttp)

postsURL :: String
postsURL = "http://jsonplaceholder.typicode.com/posts"

data Post = Post {
  userId :: Int,
  title :: String
} deriving (Show, Generic)

instance FromJSON Post
instance ToJSON Post

displayPost :: Post -> String
displayPost p = "Title: '" ++ (title p) ++ "'\n"
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
