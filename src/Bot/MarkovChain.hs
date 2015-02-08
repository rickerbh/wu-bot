{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- Implementation stolen from https://gist.github.com/mrb/dafcda4479cf330e02d6

module Bot.MarkovChain where
import qualified Data.Map.Strict as DMS (Map,insertWith,empty,member,(!))
import qualified System.Random as R

data Chain = Chain (DMS.Map String [String]) Int deriving (Show)

build :: [String] -> [String] -> Chain -> Chain
build []  _ chain = chain
build [_] _ chain = chain
build (w:ws) p (Chain c l) = 
  let prefix = if Prelude.length p < l then p ++ [w] else (Prelude.tail p) ++ [w]
      m      = (DMS.insertWith (++) (Prelude.unwords prefix) [Prelude.head ws] c)
      chain  = (Chain m l) in
        build ws prefix chain

generate :: Int -> Chain -> [String] -> IO [String]
generate 0 _ _ = return []
generate n (Chain c l) ws = 
  do
    let k = (Prelude.unwords ws)
    choice <- if DMS.member k c then pick (c DMS.! k) else return []
    let prefix = if Prelude.length ws < l then ws ++ [choice] else (Prelude.tail ws) ++ [choice]
    next <- (generate (n - 1) (Chain c l) prefix)
    return (choice:next)

pick :: [a] -> IO a
pick xs = R.randomRIO (0, Prelude.length xs - 1) >>= return . (xs !!)
