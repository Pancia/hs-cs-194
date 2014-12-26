module Ex5 where

import Parser
import Ring
import Data.Maybe

intParsingWorks :: Bool
intParsingWorks = (parse "3" == Just (3 :: Integer, "")) &&
                  (parseRing "1 + 2 * 5" == Just (11 :: Integer)) &&
                  (addId == (0 :: Integer))

data Mod5 = MkMod Integer
          deriving (Eq, Read, Show)

instance Ring Mod5 where
        addId            = MkMod 0
        addInv (MkMod x) = MkMod $ 5 - x
        mulId            = MkMod 1

        add (MkMod x) (MkMod y) = MkMod $ (`mod`5) $ x + y
        mul (MkMod x) (MkMod y) = MkMod $ (`mod`5) $ x * y

instance Parsable Mod5 where
        parse = listToMaybe . reads

type Row = (Integer, Integer)
data Mat2x2 = MkMat2x2 Row Row
        deriving (Eq, Read, Show)

--parse "MkMat2x2 (1,2) (3,4)" :: Maybe (Mat2x2, String)
--((parse "MkMat2x2 (1,2) (3,4)") :: Maybe (Mat2x2, [Char])) == (Just (MkMat2x2 (1,2) (3,4),""))
instance Parsable Mat2x2 where
        parse = listToMaybe . reads

instance Ring Mat2x2 where
        addId = MkMat2x2 (0,0)
                         (0,0)
        addInv (MkMat2x2 (a, b)
                         (c, d))
            = MkMat2x2 ((-a), (-b))
                       ((-c), (-d))
        mulId = MkMat2x2 (1,0)
                         (0,1)
        add (MkMat2x2 (a, b)
                      (c, d))
            (MkMat2x2 (e, f)
                      (g, h))
            = MkMat2x2 (a+e, b+f)
                       (c+g, d+h)
        mul (MkMat2x2 (a, b)
                      (c, d))
            (MkMat2x2 (e, f)
                      (g, h))
            = MkMat2x2 (a*e+b*g, a*f+b*h)
                       (c*e+d*g, c*f+d*g) -- FIXME: should be c*f+d*h

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
