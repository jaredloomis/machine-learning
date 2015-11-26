{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Neural where

import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy as BS

import Control.Monad
import Data.Ord
import Data.List
import System.Random

import Data.Word (Word8)
import GHC.Int (Int64)

newtype Brain = Brain {
    brainLayers :: [Layer]
    }
data Layer = Layer {
    layerBiases  :: Biases,
    layerWeights :: Weights
    }

newtype Biases  = Biases {
    unBiases :: [Float]
    }
newtype Weights = Weights {
    unWeights :: [[Float]]
    }

type WeightedInputs = [[Float]]
type Activations    = [[Float]]
type Deltas         = [[Float]]

gauss :: Float -> IO Float
gauss scale = do
    x1 <- randomIO
    x2 <- randomIO
    return $ scale * sqrt (-2 * log x1) * cos (2 * pi * x2)

-- | Create a new neural network
newBrain :: [Int] -> IO Brain
newBrain szs@(_:ts) = do
    let biases = map Biases $ flip replicate 1 <$> ts
    weights <- zipWithM mkWeightVector szs ts
    return . Brain $ zipWith Layer biases weights
  where
    mkWeightVector m n =
        Weights <$> replicateM n (replicateM m $ gauss 0.01)
newBrain []         = return $ Brain []

relu :: Float -> Float
relu = max 0

relu' :: Float -> Float
relu' x | x < 0      = 0
        | otherwise  = 1

zLayer :: [Float] -> Layer -> [Float]
zLayer as (Layer bs wvs) =
    zipWith (+) (unBiases bs) $ sum . zipWith (*) as <$> unWeights wvs

feed :: [Float] -> Brain -> [Float]
feed xs = foldl' (((relu <$>) .) . zLayer) xs . brainLayers

-- | Returns a list of (WeightedInputs, Activations) of each
--   layer, from last layer to first
revaz :: [Float] -> Brain -> (WeightedInputs, Activations)
revaz xs = foldl' f ([xs], []) . brainLayers
  where
    f (avs@(av:_), zs) (Layer bs wms) =
        let zs' = zLayer av (Layer bs wms)
        in ((relu <$> zs'):avs, zs':zs)

dCost :: Float -> Float -> Float
dCost !a !y | y == 1 && a >= y = 0
            | otherwise        = a - y

deltas :: [Float] -> [Float] -> Brain -> (Activations, Deltas)
deltas xv yv brain =
    let (avs@(av:_), zv:zvs) = revaz xv brain
        delta0      = zipWith (*) (zipWith dCost av yv) (relu' <$> zv)
        activations = reverse avs
        deltas'     = f
            (transpose . unWeights . layerWeights <$> reverse layers)
            zvs
            [delta0]
    in (activations, deltas')
  where
    layers = brainLayers brain

    f _ [] dvs = dvs
    f (wm:wms) (zv:zvs) dvs@(dv:_) = f wms zvs . (:dvs) $
      zipWith (*) [sum $ zipWith (*) row dv | row <- wm] (relu' <$> zv)

-- | Learning rate
eta :: Float
eta = 0.002

descend :: [Float] -> [Float] -> [Float]
descend av dv = zipWith (-) av ((eta *) <$> dv)

learn :: [Float] -> [Float] -> Brain -> Brain
learn xv yv brain@(Brain layers) =
    let (avs, dvs) = deltas xv yv brain
        biases = map Biases $ zipWith descend
                                (unBiases . layerBiases <$> layers)
                                dvs
    in Brain $
       zipWith Layer biases . map Weights $
       zipWith3 (\wvs av dv ->
       zipWith (\wv d -> descend wv ((d*) <$> av)) wvs dv)
       (unWeights . layerWeights <$> layers) avs dvs

-------------
-- Display --
-------------

getImage :: Num a => BS.ByteString -> Int64 -> [a]
getImage s n =
    fromIntegral .
    BS.index s .
    (n*28^(2::Word8) + 16 +) <$> [0..28^(2::Word8) - 1]

getX :: Fractional a => BS.ByteString -> Int64 -> [a]
getX     s n = (/ 256) <$> getImage s n

getLabel :: Num a => BS.ByteString -> Int64 -> a
getLabel s n = fromIntegral $ BS.index s (n + 8)

getY :: Fractional a => BS.ByteString -> Int64 -> [a]
getY     s n =
    fromIntegral . fromEnum . (getLabel s n ==) <$> [(0::Word8)..9]

render :: Int -> Char
render n =
    let s = " .:oO@"
    in s !! (n * length s `div` 256)

prettyPrint :: Int -> Float -> String
prettyPrint d score =
    show d ++ ": " ++ replicate (round $ 70 * min 1 score) '+'

trainImages :: IO BS.ByteString
trainImages = decompress <$> BS.readFile "train-images-idx3-ubyte.gz"
trainLabels :: IO BS.ByteString
trainLabels = decompress <$> BS.readFile "train-labels-idx1-ubyte.gz"
testImages :: IO BS.ByteString
testImages = decompress <$> BS.readFile "t10k-images-idx3-ubyte.gz"
testLabels :: IO BS.ByteString
testLabels = decompress <$> BS.readFile "t10k-labels-idx1-ubyte.gz"

myMain :: IO ()
myMain = do
    -- Initial data
    brain  <- newBrain [784, 30, 10]
    trainI <- trainImages
    trainL <- trainLabels
    testI  <- testImages
    testL  <- testLabels
    -- Smart brain
    let smart = smartBrain 1 brain trainI trainL
    -- Example
    n <- (`mod` testLen) <$> randomIO
    putStrLn . renderImage n =<< testImages
    print $ bestGuess (getX testI n) smart
    -- Summary
    guesses' <- guesses smart testI
    print . sum $ fromEnum <$> zipWith (==) guesses' (answers testL)
  where
    trainLen = 15000
    testLen  = 9999

    guesses brain testI =
        return $ map (\n -> bestGuess (getX testI n) brain) [0..testLen]

    answers testL = getLabel testL <$> [0..testLen]
        
    bestGuess :: [Float] -> Brain -> Float
    bestGuess image brain = bestOf $ feed image brain

    bestOf :: [Float] -> Float
    bestOf = fst . maximumBy (comparing snd) . zip [0..]

    smartBrain n b trainI trainL =
      foldl'
        (\br -> const $ foldl' (\b' n -> learn (getX trainI n) (getY trainL n) b')
               br
               [0..trainLen])
        b
        [1..n]

    renderImage n testI = unlines $
        take 28 $ take 28 <$> iterate (drop 28) (render <$> getImage testI n)
{-
        foldl'
            (foldl' (\b n -> learn (getX trainI n) (getY trainL n) b))
            b
            [[   0.. 999],
             [1000..2999],
             [3000..5999],
             [6000..9999]]
-}
    

mainNeural :: IO ()
mainNeural = do
    myMain
{-
    [trainI, trainL, testI, testL] <- mapM ((decompress  <$>) . BS.readFile)
        ["train-images-idx3-ubyte.gz", "train-labels-idx1-ubyte.gz",
         "t10k-images-idx3-ubyte.gz",  "t10k-labels-idx1-ubyte.gz"]
    b <- newBrain [784, 30, 10]
    n <- (`mod` 10000) <$> randomIO
    putStr . unlines $
        take 28 $ take 28 <$> iterate (drop 28) (render <$> getImage testI n)

    let example = getX testI n
        bs = scanl (foldl' (\b n -> learn (getX trainI n) (getY trainL n) b)) b 
           [[   0.. 999],
            [1000..2999],
            [3000..5999],
            [6000..9999]]
        smart = last bs
        bestOf = fst . maximumBy (comparing snd) . zip [0..]

    forM_ bs $ putStrLn . unlines . zipWith prettyPrint [0..9] . feed example

    putStrLn $ "best guess: " ++ show (bestOf $ feed example smart)

    let guesses = bestOf . (\n -> feed (getX testI n) smart) <$> [0..9999]
    let answers = getLabel testL <$> [0..9999]
    putStrLn $ show (sum $ fromEnum <$> zipWith (==) guesses answers) ++
        " / 10000"
-}
