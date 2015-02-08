{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Lyrics where
import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Text
import GHC.Generics

lyricsFile :: FilePath
lyricsFile = "wu_tang.json"

data Song = Song 
	{ title :: Text
	, lyrics :: Text
	} deriving (Show, Generic)
instance FromJSON Song
instance ToJSON Song

data Album = Album
	{ name :: Text
	, songs :: [Song]
	} deriving (Show, Generic)
instance FromJSON Album
instance ToJSON Album

data LyricsCorpus = LyricsCorpus
	{ description :: Text
	, artist :: Text
	, albums :: [Album]
	} deriving (Show, Generic)
instance FromJSON LyricsCorpus
instance ToJSON LyricsCorpus

loadLyrics = do
	d <- (eitherDecode <$> B.readFile lyricsFile) :: IO (Either String LyricsCorpus)
	case d of
		Left err -> ioError (userError err)
		Right lyrics -> return (lyrics)

allSongs corpus = Prelude.foldl (\acc x -> songs x) [] corpus

allLyrics :: [Song] -> Text
allLyrics songs = Prelude.foldl (\acc x -> append (append acc "") (lyrics x)) "" songs
