{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe (isJust, fromJust)
import Control.Monad (void, liftM5, liftM2, forM_)
import Data.Word (Word8)

import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Char
import Text.Parsec.Combinator

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import DecisionTree
import Attr

import Neural

--main :: IO ()
--main = mainNeural

main :: IO ()
main = do
    tree <- cancerTree
    testD <- testData
    let results = map (applyTree tree) testD
        resultsLen = length results
        goodLen = correct testD results
    putStr "Correctly categorized "
    putStr $ show goodLen
    putStr $ " of " ++ show (length results)
    putStr " ("
    putStr . show $ (fromIntegral goodLen / fromIntegral resultsLen) * 100
    putStrLn "%)"
  where
    correct testD results = length . filter id $
        zipWith
            (\defined inferred -> maybe False (malignant defined ==) inferred)
                testD
                results

-- Breast cancer

cancerTree :: IO (DTree Cancer Bool)
cancerTree = growTree cancerAttrs <$> cancerTraining

cancerAttrs :: [Attr Cancer]
cancerAttrs =
    [Attr "Clump Thickness"          (eachVal clumpThickness n 1 10),
     Attr "Uniformity of Cell Size"  (eachVal sizeUniformity n 1 10),
     Attr "Uniformity of Cell Shape" (eachVal sizeUniformity n 1 10),
     Attr "Marginal Adhesion"        (eachVal marginAdhesion n 1 10),
     Attr "Single Epi. Cell Size"    (eachVal epithelialSize n 1 10),
     Attr "Bare Nuclei"              (eachVal bareNuclei     n 1 10),
     Attr "Bland Chromatin"          (eachVal blandChromatin n 1 10),
     Attr "Normal Nucleoli"          (eachVal normalNucleoli n 1 10),
     Attr "Mitoses"                  (eachVal mitoses        n 1 10)]
  where
    n = 2

    eachVal :: (Show a, Ord a, Num a, Enum a) =>
        (Cancer -> a) -> a -> a -> a -> [(T.Text, Cancer -> Bool)]
    eachVal f by start end
        | start >= end = []
        | otherwise    =
            let name  = T.pack $ show start ++ "-" ++ show (start+by)
                check = between start (start+by) . f
            in (name, check) : eachVal f by (start+by) end

    between l h x = x >= l && x < h

cancerTraining :: IO (Training Cancer Bool)
cancerTraining = do
    dat <- trainData
    return [datum :=> malignant datum | datum <- dat]

trainData :: IO [Cancer]
trainData = take 500 <$> cancerData
testData  :: IO [Cancer]
testData  = drop 500 <$> cancerData

cancerData :: IO [Cancer]
cancerData = do
    parseResult <- runParser cancers () "breast-cancer-data" <$>
        TIO.readFile "data/breast-cancer-wisconsin.data"
    return $ case parseResult of
        Left  _  -> []
        Right xs -> xs

data Cancer = Cancer {
    sampleCode      :: !Int,
    clumpThickness  :: !Word8,
    sizeUniformity  :: !Word8,
    shapeUniformity :: !Word8,
    marginAdhesion  :: !Word8,
    epithelialSize  :: !Word8,
    bareNuclei      :: !Word8,
    blandChromatin  :: !Word8,
    normalNucleoli  :: !Word8,
    mitoses         :: !Word8,
    malignant       :: !Bool
    } deriving (Show, Eq)

-- Parser

cancers :: Parser [Cancer]
cancers = many (cancer <* endOfLine)

cancer :: Parser Cancer
cancer = do
    f  <- liftM5 Cancer value value value value value
    f' <- liftM5 f      value value value value value
    f' . (== 4) <$> decimal
  where
    value :: (Read i, Integral i) => Parser i
    value = maybeVal <* void (char ',')

    maybeVal :: (Read i, Integral i) => Parser i
    maybeVal = decimal <|> (char '?' *> return 0)

floating :: (Read f, Floating f) => Parser f
floating = read <$> many1 (digit <|> oneOf "-.e")

decimal :: (Read i, Integral i) => Parser i
decimal = read <$> many1 digit
