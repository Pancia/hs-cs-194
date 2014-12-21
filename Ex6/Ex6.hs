{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module HW06 where

import Data.Aeson
import Data.Monoid
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T

import Control.Monad

ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool v = v

parseData :: B.ByteString -> Either String Value
parseData bs = case (eitherDecode bs) of
                   Left  e -> Left $ e ++ "; eitherDecode failed"
                   Right (Object obj) -> Right $ Object $ fmap ynToBool $ obj

testParseData file =
        B.readFile file >>=
        return . parseData
