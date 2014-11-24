module HW02 where

import Words
import Data.List

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

freqs :: String -> [(String, Int)]
freqs = map (\x -> ([head x], length x)) . group . sort

formableBy :: String -> Hand -> Bool
formableBy "" _ = False
formableBy _ [] = False
formableBy word hand
        | all id $ map (`elem` hand) word =
            all id $ map comp $ zip (freqs word) (freqs hand)
        | otherwise = False
        where comp ((_,b), (_,d)) = b <= d

wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

--Borrowing from http://codereview.stackexchange.com/questions/70527/how-many-guards-patterns-is-too-many
charFitsTemplate :: Char -> Hand -> Char -> (Bool, Hand)
charFitsTemplate t hand c
        | t == '?' && elem c hand = (True, delete c hand)
        | otherwise = (t == c, hand)

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate ts _ [] = all (== '?') ts
wordFitsTemplate tmpl@(t:ts) hand word@(c:cs) =
        ((length tmpl) == (length word)) && isFit && wordFitsTemplate ts newHand cs
        where (isFit, newHand) = charFitsTemplate t hand c
wordFitsTemplate _ _ _ = False

wordsFittingTemplate t h = filter (wordFitsTemplate t h) allWords

scrabbleValueWord :: String -> Int
scrabbleValueWord = sum . (map scrabbleValue)

bestWords :: [String] -> [String]
bestWords words = filter ((== (maximum (map scrabbleValueWord words))) . scrabbleValueWord) words

