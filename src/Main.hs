{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where
import qualified Data.Char as DC
import qualified Data.Map.Strict as DMS (empty)
import Data.Text

import Bot.Lyrics 
import Bot.MarkovChain
import Bot.Twitter

autorap x = do
	corpus <- loadLyrics
	let songs = allSongs (albums corpus)
	let lyrics = allLyrics songs
	let ls = unpack lyrics
	let w = Prelude.words ls
	let chain = build w [] (Chain DMS.empty 1)
	output <- generate x chain ["yo"]
	return (output)

joinText xs = Prelude.concat (Prelude.map spaceSeparated xs)
	where spaceSeparated x = x ++ " "

fullWordsLessThan140 xs =
	Prelude.reverse $ 
	Prelude.tail $ 
	Prelude.reverse $ 
	Prelude.words $ 
	Prelude.take 140 xs

trimToLastPunctuationMarkWord xs = Prelude.foldr (\acc x -> if endsInPunctuation x then acc ++ (" " ++ x) else acc) [] xs
	where
		endsInPunctuation [] = False
		endsInPunctuation w = DC.isPunctuation $ Prelude.last w

replaceLastWithFullstop xs = Prelude.reverse $ "." ++ (Prelude.tail $ Prelude.reverse xs)

capitalizeFirstWord xs = (DC.toUpper $ Prelude.head xs) : Prelude.tail xs

generateTweet = do 
	wu <- autorap 40
	let results =
		capitalizeFirstWord $
		replaceLastWithFullstop $ 
		trimToLastPunctuationMarkWord $
		fullWordsLessThan140 $
		joinText wu
	return (results)

tweetLyrics = do
	t <- generateTweet
	print t
	r <- postTweet t
	return (t)

main :: IO ()
main = do
	tweetLyrics
	return ()
