import Ex5
import Ring
import Test.QuickCheck

instance Arbitrary Mod5 where
        arbitrary = genMod5

genMod5 :: Gen Mod5
genMod5 = choose (0, 4) >>=
        return . MkMod

instance Arbitrary Mat2x2 where
        arbitrary = genMat2x2
        shrink (MkMat2x2 (a,b) (c,d))
             = [MkMat2x2 (a',b') (c', d')
               | a' <- shrink a
               , b' <- shrink b
               , c' <- shrink c
               , d' <- shrink d]

genMat2x2 :: Gen Mat2x2
genMat2x2 = do a <- choose (-10, 10)
               b <- choose (-10, 10)
               c <- choose (-10, 10)
               d <- choose (-10, 10)
               return $ MkMat2x2 (a, b) (c, d)

--  1. (a + b) + c = a + (b + c) for all a, b, c in R (+ is associative).
prop1 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop1 a b c = (a `add` b) `add` c == a `add` (b `add` c)

prop1Mod5 :: Mod5 -> Mod5 -> Mod5 -> Bool
prop1Mod5 = prop1
prop1Mat2x2 :: Mat2x2 -> Mat2x2 -> Mat2x2 -> Bool
prop1Mat2x2 = prop1

--  2. There is an element 0 in R such that a + 0 = a and 0 + a = a for all a in R (0 is the additive identity).
prop2 :: (Ring a, Eq a) => a -> Bool
prop2 a = a `add` addId == a

prop2Mod5 :: Mod5 -> Bool
prop2Mod5 = prop2
prop2Mat2x2 :: Mat2x2 -> Bool
prop2Mat2x2 = prop2

--  3. For each a in R there exists −a in R such that a + (−a) = (−a) + a = 0 (−a is the additive inverse of a).
prop3 :: (Ring a, Eq a) => a -> Bool
prop3 a = a `add` (addInv a) == addId

prop3Mod5 :: Mod5 -> Bool
prop3Mod5 = prop3
prop3Mat2x2 :: Mat2x2 -> Bool
prop3Mat2x2 = prop3

--  4. a + b = b + a for all a, b in R (+ is commutative).
prop4 :: (Ring a, Eq a) => a -> a -> Bool
prop4 a b = a `add` b == b `add` a

prop4Mod5 :: Mod5 -> Mod5 -> Bool
prop4Mod5 = prop4
prop4Mat2x2 :: Mat2x2 -> Mat2x2 -> Bool
prop4Mat2x2 = prop4

--  5. (a ⋅ b) ⋅ c = a ⋅ (b ⋅ c) for all a, b, c in R (⋅ is associative).
prop5 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop5 a b c = (a `mul` b) `mul` c == a `mul` (b `mul` c)

prop5Mod5 :: Mod5 -> Mod5 -> Mod5 -> Bool
prop5Mod5 = prop5
prop5Mat2x2 :: Mat2x2 -> Mat2x2 -> Mat2x2 -> Bool
prop5Mat2x2 = prop5

--  6. There is an element 1 in R such that a ⋅ 1 = a and 1 ⋅ a = a for all a in R (1 is the multiplicative identity).[2]
prop6 :: (Ring a, Eq a) => a -> Bool
prop6 a = a `mul` mulId == a

prop6Mod5 :: Mod5 -> Bool
prop6Mod5 = prop6
prop6Mat2x2 :: Mat2x2 -> Bool
prop6Mat2x2 = prop6

--  7. a ⋅ (b + c) = (a ⋅ b) + (a ⋅ c) for all a, b, c in R (left distributivity).
prop7 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop7 a b c = a `mul` (b `add` c) == (a `mul` b) `add` (a `mul` c)

prop7Mod5 :: Mod5 -> Mod5 -> Mod5 -> Bool
prop7Mod5 = prop7
prop7Mat2x2 :: Mat2x2 -> Mat2x2 -> Mat2x2 -> Bool
prop7Mat2x2 = prop7

--  8. (b + c) ⋅ a = (b ⋅ a) + (c ⋅ a) for all a, b, c in R (right distributivity).
prop8 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop8 a b c = (b `add` c) `mul` a  == (b `mul` a) `add` (c `mul` a)

prop8Mod5 :: Mod5 -> Mod5 -> Mod5 -> Bool
prop8Mod5 = prop8
prop8Mat2x2 :: Mat2x2 -> Mat2x2 -> Mat2x2 -> Bool
prop8Mat2x2 = prop8

prop_ringMod5 :: Property
prop_ringMod5 = conjoin [prop2Mod5, prop3Mod5, prop6Mod5] -- 1 Mod5
           .&&. conjoin [prop4Mod5] -- 2 Mod5
           .&&. conjoin [prop1Mod5, prop5Mod5, prop7Mod5, prop8Mod5] -- 3 Mod5

prop_ringMat2x2 :: Property
prop_ringMat2x2 = conjoin [prop2Mat2x2, prop3Mat2x2, prop6Mat2x2] -- 1 Mat2x2
             .&&. conjoin [prop4Mat2x2] -- 2 Mat2x2
             .&&. conjoin [prop1Mat2x2, prop5Mat2x2, prop7Mat2x2, prop8Mat2x2] -- 3 Mat2x2
