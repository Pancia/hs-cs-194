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
    deriving (Show, Generic)
instance FromJSON Market
instance Eq Market where
        (==) (Market {x = x, y = y}) (Market {x = x', y = y'})
            = x == x' && y == y'
instance Ord Market where
        compare (Market {y = y1}) (Market {y = y2})
            = compare y1 y2

resultToEitherString :: Result a -> Either String a
resultToEitherString (Success a) = Right a
resultToEitherString (Error e) = Left e

parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets bs = parseData bs >>=
        resultToEitherString . fromJSON

loadData :: IO [Market]
loadData = B.readFile "markets.json" >>=
        return . either error id . parseMarkets

data OrdList a = OrdList { getOrdList :: [a] }
    deriving (Eq, Show)
instance Ord a => Monoid (OrdList a) where
        mempty  = OrdList []
        mappend (OrdList x) (OrdList y) = OrdList $ sort $ x <> y

type Searcher m = T.Text -> [Market] -> m

search :: Monoid m => (Market -> m) -> Searcher m
search mk_m t = go . filter (T.isInfixOf t . marketname)
        where go []     = mempty
              go (n:ns) = mk_m n <> go ns

firstFound :: Searcher (Maybe Market)
firstFound t ms = headMay $ search (:[]) t ms

lastFound :: Searcher (Maybe Market)
lastFound t ms = lastMay $ search (:[]) t ms

allFound :: Searcher [Market]
allFound = search (:[])

numberFound :: Searcher Int
numberFound t mks = length $ allFound t mks

orderedNtoS :: Searcher [Market]
orderedNtoS t mks = getOrdList $ search (OrdList . (:[])) t mks

test f t = loadData >>= return . (f (T.pack t))
