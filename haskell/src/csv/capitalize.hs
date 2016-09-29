{-# LANGUAGE OverloadedStrings #-}

-- This script accepts a CSV file with albums (see sample)
-- and it will capitalize all words in the authors and the
-- first word of the title (with some extra cleverness)

import Album (artist, title, decodeAlbums, printNewAlbumsRecords, Album)
import Data.Text (strip)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Vector as V

capitalizeAlbum :: Album -> Album
capitalizeAlbum oldAlbum = oldAlbum {
  artist = T.toTitle $ artist oldAlbum
  , title = title oldAlbum
}

trimAlbumProps :: Album -> Album
trimAlbumProps oldAlbum = oldAlbum {
  title = strip $ title oldAlbum
  , artist = strip $ artist oldAlbum
}

modifyAlbum :: Album -> Album
modifyAlbum = trimAlbumProps . capitalizeAlbum

handleParsedAlbums :: Either String (V.Vector Album) -> IO ()
handleParsedAlbums eiAlbums = case eiAlbums of
  Left _ -> putStrLn "No albums found"
  Right oldAlbums -> do
    let newAlbums = V.map modifyAlbum oldAlbums
    printNewAlbumsRecords newAlbums

main :: IO ()
main = do
  args <- getArgs
  contentBS <- B.readFile $ head args
  handleParsedAlbums $ decodeAlbums contentBS

