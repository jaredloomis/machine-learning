{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad

import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Char
import Text.Parsec.Combinator

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import DecisionTree
import Attr

-- TODO: Use standard error and maximum values from file

main :: IO ()
main = do
    tree <- cancerTree
    print tree
    print . applyTree tree $ Cancer 84358402 True 20.29 14.34 135.1 1297 0.1003 0.1328 0.198 0.1043 0.1809 0.05883
    testD <- testData
    let results = map (applyTree tree) testD
    putStrLn $
        "Correctly classified " ++
        show (correct testD results) ++
        " of " ++ show (length testD)
  where
    correct testD results = length . filter id $
        zipWith
            (\defined inferred -> maybe False (malignant defined ==) inferred)
                testD
                results

cancerTree :: IO (DTree Cancer Bool)
cancerTree = do
    dat <- cancerData
    growTree (cancerAttrs dat) <$> cancerTraining

cancerAttrs :: [Cancer] -> [Attr Cancer]
cancerAttrs dats =
    [Attr "Radius"    (attrVals radius), -- +4%
     Attr "Texture"   (attrVals texture), -- +2%
     {-Attr "Perimeter" (attrVals perimeter),-} -- -0.2%
     Attr "Area" (attrVals area), -- +0%
     Attr "Smothness" (attrVals smoothness), -- +0%
     Attr "Compactness" (attrVals compactness),
     {-Attr "Concavity" (attrVals concavity),-}
     {-Attr "Concave Points" (attrVals concavePoints),-}
     Attr "Symmetry" (attrVals symmetry)
     {-Attr "Fractal Dimension" (attrVals fractalDim)-}]
  where
    -- split each attr into n groups
    n = 5

    attrVals :: (Cancer -> Double) -> [(T.Text, Cancer -> Bool)]
    attrVals attr =
        let minAttr = attr minCan
            maxAttr = attr maxCan
            step    = (maxAttr - minAttr) / n
            name i  = T.pack $ show i ++ "-" ++ show (i+step)
        in [(name i, between i (i+step) . attr) |
            i <- [minAttr,minAttr+step..maxAttr]]

    minCan = minCancer dats
    maxCan = maxCancer dats

    between l h x = x >= l && x < h

data Cancer = Cancer {
    cancerId      :: !Int,
    malignant     :: !Bool,
    radius        :: !Double,
    texture       :: !Double,
    perimeter     :: !Double,
    area          :: !Double,
    smoothness    :: !Double,
    compactness   :: !Double,
    concavity     :: !Double,
    concavePoints :: !Double,
    symmetry      :: !Double,
    fractalDim    :: !Double
    } deriving (Show, Eq)

-- Info about all data

averageCancer :: [Cancer] -> Cancer
averageCancer = foldrCancerAttrs (\a b -> (a + b) / 2) 0

minCancer :: [Cancer] -> Cancer
minCancer = foldrCancerAttrs min 10e100000

maxCancer :: [Cancer] -> Cancer
maxCancer = foldrCancerAttrs max 0

foldrCancerAttrs ::
    (Double -> Double -> Double) -> Double -> [Cancer] -> Cancer
foldrCancerAttrs f i dats =
    Cancer 0 False
        (avgAttr radius) (avgAttr texture)
        (avgAttr perimeter) (avgAttr area)
        (avgAttr smoothness) (avgAttr compactness)
        (avgAttr concavity) (avgAttr concavePoints)
        (avgAttr symmetry) (avgAttr fractalDim)
  where
    avgAttr attr = foldr (f . attr) i dats

-- Parsing

cancerTraining :: IO (Training Cancer Bool)
cancerTraining = do
    dat <- trainData
    return [datum :=> malignant datum | datum <- dat]

trainData :: IO [Cancer]
trainData = take 200 <$> cancerData
testData  :: IO [Cancer]
testData  = drop 200 <$> cancerData

cancerData :: IO [Cancer]
cancerData = do
    parseResult <- runParser cancers () "wdbc" <$>
        TIO.readFile "data/wdbc.data"
    return $ case parseResult of
        Left  _  -> []
        Right xs -> xs

cancers :: Parser [Cancer]
cancers = many (cancer <* endOfLine)

cancer :: Parser Cancer
cancer = do
    -- Get relevant data (mean values)
    can   <- liftM5 Cancer userId diagnosis value value value
    can'  <- liftM5 can value value value value value
    can'' <- liftM2 can' value value
    -- Strip other data (Standard error and worst)
    skipMany1 (noneOf "\n")
    return can''
  where
    userId :: Parser Int
    userId = decimal <* char ','

    diagnosis :: Parser Bool
    diagnosis = (== 'M') <$> anyChar <* char ','

    value :: (Read f, Floating f) => Parser f
    value = maybeVal <* void (char ',')

    maybeVal :: (Read f, Floating f) => Parser f
    maybeVal = floating <|> (char '?' *> return 0)

floating :: (Read f, Floating f) => Parser f
floating = read <$> many1 (digit <|> oneOf "-.e")

decimal :: (Read i, Integral i) => Parser i
decimal = read <$> many1 digit
