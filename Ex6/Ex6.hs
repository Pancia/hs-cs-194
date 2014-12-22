{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module HW06 where

import Data.Aeson
import Data.Monoid
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T

import Data.List
import Safe

ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool v = v

onObject f (Object obj) = Object (f obj)
onObject _ o = o

parseData :: B.ByteString -> Either String Value
parseData bs = eitherDecode bs >>=
        return . onObject (fmap ynToBool)

data Market = Market { marketname :: T.Text
                     , x :: Double
                     , y :: Double
                     , credit :: T.Text
                     , state :: T.Text }
    deriving (Show, Generic, Eq)
instance FromJSON Market

resultToEitherString :: Result a -> Either String a
resultToEitherString (Success a) = Right a
resultToEitherString (Error e) = Left e

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets bs = parseData bs >>=
        resultToEitherString . fromJSON

loadData :: IO [Market]
loadData = B.readFile "markets.json" >>=
        return . either error id . parseMarkets

type Searcher m = T.Text -> [Market] -> m

search :: Monoid m => (Market -> m) -> Searcher m
search f t = foldMap f . filterMarkets
        where filterMarkets = filter (T.isInfixOf t . marketname)
              foldMap f = foldr (mappend . f) mempty

firstFound :: Searcher (Maybe Market)
firstFound = fmap headMay . search (:[])

lastFound :: Searcher (Maybe Market)
lastFound = fmap lastMay . search (:[])

allFound :: Searcher [Market]
allFound = search (:[])

numberFound :: Searcher Int
numberFound = fmap length . allFound

instance Ord Market where
        compare (Market {y = y1}) (Market {y = y2})
            = compare y1 y2
newtype OrdMarkets = OrdMarkets { getOrdMarkets :: [Market] }
instance Monoid OrdMarkets where
        mempty = OrdMarkets []
        mappend (OrdMarkets x) (OrdMarkets y)
            = OrdMarkets $ sort $ x <> y

orderedNtoS :: Searcher [Market]
orderedNtoS t mks = getOrdMarkets $ search (OrdMarkets . (:[])) t mks

test :: Searcher a -> String -> IO a
test f t = loadData >>= return . (f (T.pack t))
