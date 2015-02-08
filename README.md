Wu-Bot
======

Lyrics generator that can tweet in the style of Wu-Tang Clan's 36 Chambers album.

**Sometimes I'm NSFW.**

How?
----
Wu-Bot reads JSON based lyrics, parses the lyrics into a [markov chain](https://en.wikipedia.org/wiki/Markov_chain) to figure out word sequences and frequencies, then generates new lyrics based on this data. 

Why?
----
Fun. Experiment. Learning something new. Mostly fun.

I want to use it!
-----------------
Sure, go ahead. Make sure you read the [license](http://creativecommons.org/licenses/by-nc/4.0/) if you're going to remix this. [Contact me directly](http://hamishrickerby.com) if you want to use it commercially. 

* You'll need [ghc](https://ghc.haskell.org) and [cabal](https://www.haskell.org/cabal/) installed. 
* You'll need to [set up a twitter app](https://apps.twitter.com) and get the Consumer Key and Consumer Secret if you plan on posting to twitter.
* In `src/Bot/Twitter.hs`, replace the fields for the consumer key and consumer secret with the values from your twitter app.
* I like cabal in a sandbox, so in the cloned project directory:
`cabal sandbox init`
* Because we need OAuth1 support, and we use [wreq](https://github.com/bos/wreq) (which doesn't have OAuth1 support), you'll also need [my fork of wreq](https://github.com/rickerbh/wreq). Clone it somewhere on your machine, then add it to the sandbox. (make sure the last parameter points to your cloned wreq)
`cabal sandbox add-source ../wreq/`
* Install everything
`cabal install -j --disable-tests`
* Run the repl
`cabal repl`
* To get twitter setup, type the method below and follow the instructions. It'll give you a URL to go to, and then you respond with the code that Twitter give you once you've allowed the app access to your account.
`getTokens`
* To test that twitter posting is working
`postFirstTweet`
* To generate a random tweet sized lyrics set
`generateTweet`
* To generate and post a random lyrics tweet
`tweetLyrics`

Disclaimer
----------
This code is a mess. I haven't tidied it - probably full of cruft. This was an experiment to try to do something "real world" with Haskell (reading/writing files, processing data, interacting with web services). I'm no haskell expert - I expect viewing this code would result in indignation from any beginner haskeller, let alone people who actually know how to write idiomatic Haskell.

I'd *really really really* appreciate feedback and suggestions on how this code could be more idiomatic.

Created By [Hamish Rickerby](http://twitter.com/rickerbh). Follow me, and get in touch!
