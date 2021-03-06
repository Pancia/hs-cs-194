module HW02 where

import Words
import Data.List
import Data.Char

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

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate ts _ [] = all (=='?') ts
wordFitsTemplate tmpl hand word
        = fst $ foldl charFitsTemplate
            ((length tmpl) == (length word), hand)
            $ zip tmpl word
        where charFitsTemplate (b, hand) (t, c)
                | not b = (False, hand)
                | t == '?' && elem c hand = (True, delete c hand)
                | otherwise = (t == c, hand)

wordsFittingTemplate t h = filter (wordFitsTemplate t h) allWords

scrabbleValueWord :: String -> Int
scrabbleValueWord = sum . (map scrabbleValue)

bestWords :: [String] -> [String]
bestWords words = filter ((== (maximum (map scrabbleValueWord words))) . scrabbleValueWord) words

--scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate tmpl word
        = let (val, mult) = foldl charToTemplateValue (0,1) $ zip tmpl word
              in val * mult
        where charToTemplateValue (val, mult) (t, c)
                | t == '?' = (val + scrabbleValue c, mult)
                | elem t ['2','3'] = (val + scrabbleValue c, (digitToInt t))
                | t == 'D' = (val + scrabbleValue c * 2, mult)
                | t == 'T' = (val + scrabbleValue c * 3, mult)
                | otherwise = (val + scrabbleValue c, mult)


