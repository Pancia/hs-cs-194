import Text.Read
import Data.List
import Data.Maybe
import Control.Monad
import Data.Char
import Debug.Trace

import Control.Monad.Random
import Data.Monoid
import Data.Foldable

stringFitsFormat :: String -> Bool
stringFitsFormat = isJust . go
        where go :: String -> Maybe String
              go "" = Just ""
              go (c:cs) = do
                  guard (isDigit c)
                  n <- Just $ digitToInt c
                  trace (show n) Just n
                  cs' <- stripPrefix (take n (repeat 'a')) cs
                  go cs'

specialNumbers :: [Int]
specialNumbers = [x | x <- [1..100]
                    , let div5 = x `rem` 5 == 0
                    , let div7 = x `rem` 7 == 0
                    , div5 && not div7]

type StdRand = Rand StdGen

type Army = Int
data ArmyCounts = ArmyCounts { atks :: Army
                             , defs :: Army }
    deriving Show
instance Monoid ArmyCounts where
        mempty  = ArmyCounts 0 0
        mappend (ArmyCounts atks defs) (ArmyCounts atks' defs')
            = ArmyCounts (atks + atks') (defs + defs')
type DieRoll = Int

dieRoll :: StdRand DieRoll
dieRoll = getRandomR (1,6)

dieRolls :: Int -> StdRand [DieRoll]
dieRolls n = sequence (replicate n dieRoll)

battleResults :: [DieRoll] -> [DieRoll] -> ArmyCounts
battleResults atks defs = fold $ zipWith fightToArmyCounts atks' defs'
        where atks' = reverse . sort $ atks
              defs' = reverse . sort $ defs
              fightToArmyCounts a d
                    | a > d     = ArmyCounts 0 (-1)
                    | otherwise = ArmyCounts (-1) 0

battle :: ArmyCounts -> StdRand ArmyCounts
battle bef@(ArmyCounts atks defs)
        = do atkRolls <- dieRolls $ min 3 (atks - 1)
             defRolls <- dieRolls $ min 2 defs
             return $ bef <> (battleResults atkRolls defRolls)

invade :: ArmyCounts -> StdRand ArmyCounts
invade bef@(ArmyCounts atks defs)
        | defs > 0 && atks > 1 = battle bef >>= invade
        | otherwise = return bef

(//) :: Int -> Int -> Double
a // b = fromIntegral a / fromIntegral b

successful :: ArmyCounts -> Bool
successful (ArmyCounts a d) = d == 0 && a > 1

invasions :: Int -> ArmyCounts -> StdRand [ArmyCounts]
invasions n ac = sequence (replicate n (invade ac))

successProb :: ArmyCounts -> StdRand Double
successProb ac = (invasions 10000 ac) >>=
                 filterM (return . successful) >>=
                 return . (//10000) . length

