{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Bot.Twitter where
import Control.Applicative
import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Data.Text
import Data.Text.Encoding
import qualified Data.Text.IO as TIO
import GHC.Generics
import Network.Wreq

consumerKey = "CONSUMER KEY GOES HERE"
consumerSecret = "CONSUMER SECRET GOES HERE"

firstTweet = "Bleep boop. I'm alive."

credentialsFile :: FilePath
credentialsFile = "credentials.json"

tweetURL = "https://api.twitter.com/1.1/statuses/update.json"

data UserCredentials = UserCredentials 
	{ oauthToken :: Text
	, oauthTokenSecret :: Text
	} deriving (Show, Generic) 
instance FromJSON UserCredentials
instance ToJSON UserCredentials

saveUserCredentials token secret = do
	let jsonData = encode (UserCredentials {oauthToken = token, oauthTokenSecret = secret})
	B.writeFile credentialsFile jsonData
	return ()

loadUserCredentials = do
	d <- (eitherDecode <$> B.readFile credentialsFile) :: IO (Either String UserCredentials)
	case d of
		Left err -> ioError (userError err)
		Right creds -> return (creds)

generateTwitterAuth = do
	c <- loadUserCredentials 
	let token = encodeUtf8 $ oauthToken c
	let secret = encodeUtf8 $ oauthTokenSecret c
	return (oauth1Auth consumerKey consumerSecret token secret)

postFirstTweet = postTweet firstTweet

postTweet x = do
	a <- generateTwitterAuth
	let opts = defaults & param "status" .~ [pack x] & auth ?~ a
	r <- postWith opts tweetURL ("" :: B.ByteString)
	print "Done"
	return (r)

createKeyValuePairsFromBody b = do
	let stripped = stripQuotesAndSquareBrackets b
	splitURLParameters (splitOn "&" stripped)

stripQuotesAndSquareBrackets :: Text -> Text
stripQuotesAndSquareBrackets s = fromJust (stripSuffix "\"]" (fromJust (stripPrefix "[\"" s)))

splitURLParameters :: [Text] -> [(Text, Text)]
splitURLParameters s = [splitURLParameter x | x <- s]

splitURLParameter :: Text -> (Text, Text)
splitURLParameter s = (key s, value s) 
	where
		key x = Prelude.head (splitOn "=" x)
		value x = Prelude.last (splitOn "=" x)

extractOAuthTokenPair xs = extractValueFromKey xs "oauth_token"

extractOAuthSecretPair xs = extractValueFromKey xs "oauth_token_secret"

extractValueFromKey xs key = Prelude.head (Prelude.filter condition xs)
	where
		condition (k, _) = k == key

getTokens = do
	tokenPairs <- requestToken
	let oauthToken = snd (extractOAuthTokenPair tokenPairs)
	let oauthSecret = snd (extractOAuthSecretPair tokenPairs)
	print "Head to: "
	let url = "https://api.twitter.com/oauth/authenticate?oauth_token=" ++ (unpack oauthToken)
	print url
	print "Authorise, and then enter the code back here."

	code <- TIO.getLine
	accessTokenPairs <- accessToken code oauthToken oauthSecret
	let accessToken = snd (extractOAuthTokenPair accessTokenPairs)
	let accessSecret = snd (extractOAuthSecretPair accessTokenPairs)
	saveUserCredentials accessToken accessSecret 

requestToken :: IO [(Text, Text)]
requestToken = do
	let opts = defaults & auth ?~ oauth1Temp consumerKey consumerSecret "oob"
	r <- getWith opts "https://api.twitter.com/oauth/request_token"
	let body = pack (show (r ^.. responseBody))
	return (createKeyValuePairsFromBody body)

accessToken :: Text -> Text -> Text -> IO [(Text, Text)]
accessToken verifier tempToken tempSecret = do
	let opts = defaults & param "oauth_verifier" .~ [verifier] & auth ?~ oauth1ReqAccessToken consumerKey consumerSecret (encodeUtf8 tempToken) (encodeUtf8 tempSecret) (encodeUtf8 verifier)
	r <- getWith opts "https://api.twitter.com/oauth/access_token"
	let body = pack (show (r ^.. responseBody))
	return (createKeyValuePairsFromBody body)
