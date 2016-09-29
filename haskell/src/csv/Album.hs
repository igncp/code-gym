{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Album ( Album
  , artist
  , title
  , decodeAlbums
  , printNewAlbumsRecords
  )
where

import Data.Text (Text)
import qualified Data.Vector as V
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Csv
  ( FromNamedRecord(parseNamedRecord)
  , ToNamedRecord(toNamedRecord)
  , DefaultOrdered(headerOrder)
  , (.:)
  , (.=)
  )
import qualified Data.Csv as CS

data Album =
  Album
    { artist :: Text
    , title :: Text
    }
  deriving (Eq, Show)

instance FromNamedRecord Album where
  parseNamedRecord m =
    Album
      <$> m .: "Artist"
      <*> m .: "Title"

instance ToNamedRecord Album where
  toNamedRecord Album{..} =
    CS.namedRecord
      [ "Artist" .= artist
      , "Title" .= title
      ]

instance DefaultOrdered Album where
  headerOrder _ =
    CS.header
      [ "Artist"
      , "Title"
      ]

myOptions :: CS.EncodeOptions
myOptions = CS.defaultEncodeOptions {
  CS.encQuoting = CS.QuoteAll
}

decodeAlbums :: ByteString -> Either String (V.Vector Album)
decodeAlbums = fmap snd . CS.decodeByName

printNewAlbumsRecords :: V.Vector Album -> IO ()
printNewAlbumsRecords albums = putStr $ C.unpack $ CS.encodeDefaultOrderedByNameWith myOptions $ V.toList albums

