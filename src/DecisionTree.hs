{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module DecisionTree where

import Data.List
import Data.Maybe (listToMaybe)
import Control.Monad (join)

type Training a b = [a :=> b]

-- | A solution
data a :=> b = !a :=> !b
  deriving (Show, Eq)
infixr 1 :=>

datum :: a :=> b -> a
datum (a :=> _) = a
label :: a :=> b -> b
label (_ :=> b) = b

-- | An attribute is something that
--   can be used to categorize things
data Attr a = Attr {
    -- | Attribute name
    attrName :: !String,
    -- | Attribute categories
    attrTests :: [(String, a -> Bool)]
    }

instance Show (Attr a) where
    show = show . attrName
instance Eq (Attr a) where
    x == y = attrName x == attrName y

data DTree a b =
    Node !(Attr a) ![DTree a b]
  | Leaf !(Maybe b)
  deriving (Show, Eq)

applyTree :: DTree a b -> a -> Maybe b
applyTree (Leaf a) _ = a
applyTree (Node attr children) x =
    join . fmap (flip applyTree x . fst) .
    listToMaybe .
    filter (($ x) . snd . snd) $
    zip children (attrTests attr)

growTree :: forall a b. Ord b => [Attr a] -> Training a b -> DTree a b
growTree []    []       = Leaf Nothing
growTree []    training = Leaf $ mostCommonLabel training
growTree attrs training
    | isLeaf    = Node best $ map (Leaf . mostCommonLabel) grouped
    | otherwise = Node best $ map (growTree attrs')        grouped
  where
    isLeaf :: Bool
    isLeaf = (<= 1) . length . filter (not . null) $ grouped

    grouped :: [Training a b]
    grouped = groupTraining best training

    attrs' :: [Attr a]
    attrs' = delete best attrs

    best :: Attr a
    best = bestAttr training attrs

groupTraining :: Ord b => Attr a -> Training a b -> [Training a b]
groupTraining attr training =
    map (\(_, testAttr) -> filter (testAttr . datum) training)
        (attrTests attr)

bestAttr :: forall a b. Ord b => Training a b -> [Attr a] -> Attr a
bestAttr training = foldr1 best
  where
    best :: Attr a -> Attr a -> Attr a
    best a b
        | quality a > quality b = a
        | otherwise             = b

    quality :: Attr a -> Double
    quality = sum . entropies

    entropies :: Attr a -> [Double]
    entropies =
        let applyTest attrTest = map (attrTest . datum) training
        in map (entropy . applyTest . snd) . attrTests

entropy :: Ord a => [a] -> Double
entropy as = negate . sum . map value . group . sort $ as
  where
    value g =
        let p = flength g / flength as
        in p * logBase 2 p
    flength g = fromIntegral $ length g

mostCommonLabel :: Ord b => Training a b -> Maybe b
mostCommonLabel = mostCommon . map label

-- | Most Common element in list
mostCommon :: Ord a => [a] -> Maybe a
mostCommon [] = Nothing
mostCommon xs =
    Just . bestVal . foldl f (BestRun (head xs) 1 (head xs) 1) . sort $ xs
  where
    f :: Eq a => BestRun a -> a -> BestRun a
    f (BestRun current occ best bestOcc) x
        | x == current  = BestRun current (occ + 1) best bestOcc
        | occ > bestOcc = BestRun x 1 current occ
        | otherwise     = BestRun x 1 best bestOcc

data BestRun a = BestRun {
   currentVal :: a,
   occurrences :: Int,
   bestVal :: a,
   bestOccurrences :: Int
}


-- TESTING

data Grade = F | D | C | B | A
  deriving (Show, Eq, Ord)

data Person = Person {
    personAge :: Int,
    personSex :: Sex
    } deriving (Show, Eq)

data Sex = Male | Female
  deriving (Show, Eq)

myAttrs :: [Attr Person]
myAttrs =
    [Attr "Age"
        [( "0-10", between  0 10 . personAge),
         ("10-20", between 10 20 . personAge),
         ("20-30", between 20 30 . personAge),
         ("30-40", between 30 40 . personAge),
         ("40-50", between 40 50 . personAge),
         ("50-60", between 50 60 . personAge)],
     Attr "Sex"
        [("Male",   (== Male)   . personSex),
         ("Female", (== Female) . personSex)]]
  where
    between l h x = x >= l && x < h


myTraining :: Training Person Grade
myTraining =
    [Person age Male   :=> F                       | age <- [0, 3 ..10]] ++
    [Person age Female :=> F                       | age <- [0, 3 ..10]] ++
    [Person age Male   :=> iff (age `isDiv` 2) B C | age <- [10,13..20]] ++
    [Person age Female :=> iff (age `isDiv` 2) C D | age <- [10,13..20]] ++
    [Person age Male   :=> iff (age `isDiv` 2) A B | age <- [20,26..30]] ++
    [Person age Female :=> iff (age `isDiv` 2) B C | age <- [20,26..30]] ++
    [Person age Male   :=> iff (age `isDiv` 2) C D | age <- [30,36..40]] ++
    [Person age Female :=> iff (age `isDiv` 2) D F | age <- [30,36..40]]
  where
    iff b t f = if b then t else f
    isDiv x m = x `mod` m == 0

myTree = growTree myAttrs myTraining
