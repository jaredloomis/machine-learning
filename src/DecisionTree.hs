{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveFunctor       #-}
module DecisionTree where

import Data.List
import Data.Maybe (isJust, listToMaybe, maybeToList)
import Control.Monad (join)
import Control.Applicative ((<|>))
import Control.DeepSeq

import qualified Data.Text as T

import Attr

data DTree a b =
    Node !(Attr a) ![DTree a b]
  | Leaf !(Maybe b)
  deriving (Show, Eq, Functor)

instance NFData b => NFData (DTree a b) where
    rnf (Node attr xs) = rnf attr `seq` rnf xs
    rnf (Leaf lbl)     = rnf lbl

-- | Use decision tree to label a datum
applyTree :: DTree a b -> a -> Maybe b
applyTree (Leaf lbl)           _ = lbl
applyTree (Node attr children) x =
    join .
    fmap (flip applyTree x . fst) $
    correctBranch
  where
    correctBranch =
        let a = filter (($ x) . snd . snd) childrenTests
        in if null a && not (null childrenTests)
                then Just $ head childrenTests
                else listToMaybe a

    childrenTests = zip children (attrTests attr)
{-
    join .
    fmap (flip applyTree x . fst) .
    filter (($ x) . snd . snd) $
    zip children (attrTests attr)
-}

growTree :: Ord b => [Attr a] -> Training a b -> DTree a b
growTree = growTree' Nothing

-- | Create a decision tree from attributes and training data
growTree' :: forall a b. Ord b =>
    Maybe b -> [Attr a] -> Training a b -> DTree a b
growTree' def []    []       = Leaf def
growTree' def []    training = Leaf $ mostCommonLabel training <|> def
growTree' def attrs training
    -- If it's a leaf, label it with most common label
    | isLeaf    = Node best $ map growLeaf    grouped
    -- Otherwise, recursively grow tree
    | otherwise = Node best $ map growSubTree grouped
  where
    growLeaf :: Training a b -> DTree a b
    growLeaf train =
        let defLbl = mostCommonLabel train <|> defLabel
        in Leaf defLbl

    growSubTree :: Training a b -> DTree a b
    growSubTree train =
        let defLbl = mostCommonLabel train <|> defLabel
        in growTree' defLbl attrs' train

    -- | It's a leaf if only one (or zero) attribute tests
    --   have members
    isLeaf :: Bool
    isLeaf = (<= 1) . length . filter (not . null) $ grouped

    -- | Grouped by label
    grouped :: [Training a b]
    grouped = groupTraining best training

    defLabel :: Maybe b
    defLabel = mostCommonLabel training <|> def

    attrs' :: [Attr a]
    attrs' = delete best attrs

    best :: Attr a
    best = bestAttr training attrs

groupTraining :: Eq b => Attr a -> Training a b -> [Training a b]
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
    Just . bestVal . foldl' f (BestRun (head xs) 1 (head xs) 1) . sort $ xs
  where
    f :: Eq a => BestRun a -> a -> BestRun a
    f (BestRun current occ best bestOcc) x
        | x == current  = BestRun current (occ + 1) best bestOcc
        | occ > bestOcc = BestRun x 1 current occ
        | otherwise     = BestRun x 1 best bestOcc

data BestRun a = BestRun {
    currentVal      :: a,
    occurrences     :: !Int,
    bestVal         :: a,
    bestOccurrences :: !Int
    }
