import Parser
import Ring
import Data.Maybe

intParsingWorks :: Bool
intParsingWorks = (parse "3" == Just (3 :: Integer, "")) &&
                  (parseRing "1 + 2 * 5" == Just (11 :: Integer)) &&
                  (addId == (0 :: Integer))

data Mod5 = MkMod Integer
          deriving (Show, Eq, Read)

instance Ring Mod5 where
        addId            = MkMod 0
        addInv (MkMod x) = MkMod $ 5 - x
        mulId            = MkMod 1

        add (MkMod x) (MkMod y) = MkMod $ (`mod`5) $ x + y
        mul (MkMod x) (MkMod y) = MkMod $ (`mod`5) $ x * y

instance Parsable Mod5 where
        parse = listToMaybe . reads

mod5ParsingWorks :: Bool
mod5ParsingWorks = (parse "MkMod 3" == Just (MkMod 3 :: Mod5, "")) &&
                   (parseRing "MkMod 3 + MkMod 2 * MkMod 1" == Just (MkMod 0 :: Mod5)) &&
                   (addId == (MkMod 0 :: Mod5)) &&
                   (mulId == (MkMod 1 :: Mod5)) &&
                   (addInv (MkMod 2) == (MkMod 3 :: Mod5))

instance Ring Bool where
        addId  = False
        addInv = not
        mulId  = True

        add = (||)
        mul = (&&)

instance Parsable Bool where
        parse = listToMaybe . reads
