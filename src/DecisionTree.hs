{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DecisionTree where

import Data.List

type Training a b = [a :=> b]

-- | A solution
data a :=> b = a :=> b
  deriving (Show, Eq)
infixr 1 :=>

solutionX :: a :=> b -> a
solutionX (a :=> _) = a
solutionY :: a :=> b -> b
solutionY (_ :=> b) = b

-- Decision Trees --

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

data DTree a =
    Node !(Attr a) ![DTree a]
  | Leaf !Bool
  deriving (Eq)

instance Show a => Show (DTree a) where
    show = showI 0
      where
        showI :: Show a => Int -> DTree a -> String
        showI i (Leaf x) = tabs i ++ show x ++ "\n"
        showI i (Node a xs) =
            tabs i ++ show a ++
            " {\n" ++
            concatMap (showLine $ i+1) (zip xs . map fst $ attrTests a) ++
            tabs i ++ "}\n"

        showLine :: Show a => Int -> (DTree a, String) -> String
        showLine i (tree, name) =
            tabs i ++ show name ++ " =>\n" ++ showI (i+1) tree

        tabs :: Int -> String
        tabs i = replicate (i*4) ' '

applyTree :: DTree a -> a -> Bool
applyTree (Leaf a) _ = a
applyTree (Node attr children) x =
    flip applyTree x .
    fst . head .
    filter (($ x) . snd . snd) $
    zip children (attrTests attr)

growTree :: [Attr a] -> Training a Bool -> DTree a
growTree = flip growTree' False
  where
    growTree' :: [Attr a] -> Bool -> Training a Bool -> DTree a
    growTree' attrs@(_:_) _ train =
        let xj = bestAttr train attrs
        in applyAttr (growTree' $ delete xj attrs) xj train
    growTree' _ def _ = Leaf def

applyAttr :: forall a.
            (Bool -> Training a Bool -> DTree a) ->
             Attr a ->
             Training a Bool ->
             DTree a
applyAttr grow attr train
    | isLeaf    = Node attr $ map (Leaf . not . null) grouped
    | otherwise = Node attr $ map (grow False)        grouped
  where
    isLeaf :: Bool
    isLeaf = (<= 1) . length . filter (not . null) $ grouped

    grouped :: [Training a Bool]
    grouped = map (\(_, testAttr) -> filter (testAttr . solutionX) train)
                  (attrTests attr)

sortAttrs :: Training a Bool -> [Attr a] -> [Attr a]
sortAttrs _     []    = []
sortAttrs train attrs =
    let best = bestAttr train attrs
    in best : sortAttrs train (delete best attrs)

bestAttr :: forall a. Training a Bool -> [Attr a] -> Attr a
bestAttr train = foldr1 best
  where
    best :: Attr a -> Attr a -> Attr a
    best a b
        | quality a > quality b = a
        | otherwise             = b

    quality :: Attr a -> Double
    quality = sum . entropies

    entropies :: Attr a -> [Double]
    entropies =
        let applyTest attrTest = map (attrTest . solutionX) train
        in map (entropy . applyTest . snd) . attrTests

entropy :: [Bool] -> Double
entropy as = negate . sum . map value . group . sort $ as
  where
    value g =
        let p = flength g / flength as
        in p * logBase 2 p
    flength g = fromIntegral $ length g

--- TEST

data House = House {
    housePrice :: Int,
    houseFeet  :: Int
    }

houses :: [House]
houses = zipWith House prices squareFeet

prices :: [Int]
prices = [385000, 309000, 316000, 149900, 850000, 259900, 799888, 1785, 330000, 409999, 749950, 259000, 475000, 925000, 380000, 395000, 365000, 314526, 585000, 399900, 475000, 439900, 87500, 375000, 235000, 162000, 2950, 115000, 299900]

squareFeet :: [Int]
squareFeet = [1400, 1040, 1042, 1296, 2320, 1548, 1831, 6709, 1514, 2301, 3532, 2016, 3046, 3303, 2017, 1338, 3064, 785, 2828, 2153, 2472, 2156, 1145, 1816, 1612, 884, 3662, 1098, 1183]

houseAttrs :: [Attr House]
houseAttrs =
    []



{-
data Sex = Male | Female deriving (Show, Eq)

data Patient = Patient {
    patientSex    :: Sex,
    patientAge    :: Int,
    patientWeight :: Float
    } deriving (Show, Eq)

patients :: Training Patient Bool
patients =
    [Patient Male   50 170 :=> False,
     Patient Male   60 200 :=> True,
     Patient Female 24 120 :=> False,
     Patient Female 54 160 :=> True,
     Patient Female 57 155 :=> True,
     Patient Male   30 145 :=> False,
     Patient Male   12 100 :=> False,
     Patient Male   21 160 :=> False,
     Patient Female 30 150 :=> True]

patientAttrs :: [Attr Patient]
patientAttrs =
    -- Sex
    [Attr "Sex" 
        [("Male",  (== Male) . patientSex),
         ("Female",(/= Male) . patientSex)],
    -- Age
     Attr "Age"
        [("Age group 1", between 0  20  . patientAge),
         ("Age group 2", between 20 40  . patientAge),
         ("Age group 3", between 40 60  . patientAge),
         ("Age group 4", between 60 80  . patientAge),
         ("Age group 5", between 80 100 . patientAge)],
    -- Weight
     Attr "Weight"
        [("Weight group 1", between 100 120 . patientWeight),
         ("Weight group 2", between 120 140 . patientWeight),
         ("Weight group 3", between 140 160 . patientWeight),
         ("Weight group 4", between 160 180 . patientWeight),
         ("Weight group 5", between 180 200 . patientWeight),
         ("Weight group 6", between 200 220 . patientWeight)]]
  where
    between :: Ord a => a -> a -> a -> Bool
    between l h x = x >= l && x < h

cancerTree :: DTree Patient
cancerTree = growTree patientAttrs patients

iHaveCancer :: Bool
iHaveCancer = cancerTree `applyTree` Patient Male 14 110
-}
