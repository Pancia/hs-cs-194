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

test_mod5 :: Bool
test_mod5 = (parse "MkMod 3" == Just (MkMod 3 :: Mod5, "")) &&
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

swapIdentities :: RingExpr a -> RingExpr a
swapIdentities AddId      = MulId
swapIdentities MulId      = AddId
swapIdentities a = a

distribute :: RingExpr a -> RingExpr a
distribute (Mul (Add a b) y) = (Add (Mul (distribute a) (distribute y))
                                    (Mul (distribute b) (distribute y)))
distribute (Mul x (Add a b)) = (Add (Mul (distribute x) (distribute a))
                                    (Mul (distribute x) (distribute b)))
distribute a = a

squashMulId :: RingExpr a -> RingExpr a
squashMulId (Mul MulId y) = (squashMulId y)
squashMulId (Mul x MulId) = (squashMulId x)
squashMulId a = a

ringMap :: ((RingExpr a) -> (RingExpr a))
        -> (RingExpr a) -> (RingExpr a)
ringMap f (Mul x y) = (f (Mul (ringMap f x) (ringMap f y)))
ringMap f (Add x y) = (f (Add (ringMap f x) (ringMap f y)))
ringMap f (AddInv x) = (f (AddInv (ringMap f x)))
ringMap f r = (f r)
