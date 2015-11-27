{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE FlexibleContexts #-}
module Neural where

import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy as BS

import Control.DeepSeq
import Control.Monad
import Data.Ord
import Data.List
import System.Random

import Data.Word (Word8)
import GHC.Int (Int64)

import qualified Data.Vector as V

newtype Brain = Brain {
    brainLayers :: V.Vector Layer
    } deriving (Show, Eq)
data Layer = Layer {
    layerBiases  :: !Biases,
    layerWeights :: !Weights
    } deriving (Show, Eq)

newtype Biases  = Biases {
    unBiases :: V.Vector Float
    } deriving (Show, Eq)
newtype Weights = Weights {
    unWeights :: V.Vector (V.Vector Float)
    } deriving (Show, Eq)

type WeightedInputs = V.Vector (V.Vector Float)
type Activations    = V.Vector (V.Vector Float)
type Deltas         = V.Vector (V.Vector Float)

gauss :: Float -> IO Float
gauss scale = do
    x1 <- randomIO
    x2 <- randomIO
    return $ scale * sqrt (-2 * log x1) * cos (2 * pi * x2)

-- | Create a new neural network
newBrain :: V.Vector Int -> IO Brain
newBrain szs | not (V.null szs) = do
    let ts = V.tail szs
    let biases = fmap Biases $ flip V.replicate 1 <$> ts
    weights <- V.zipWithM mkWeightVector szs ts
    return . Brain $ V.zipWith Layer biases weights
  where
    mkWeightVector m n =
        Weights <$> V.replicateM n (V.replicateM m $ gauss 0.01)
newBrain _ = return $ Brain []

relu :: Float -> Float
relu = max 0

relu' :: Float -> Float
relu' x | x < 0      = 0
        | otherwise  = 1

zLayer :: V.Vector Float -> Layer -> V.Vector Float
zLayer as (Layer bs wvs) = V.generate (V.length rawBiases) $ \i ->
    let bias      = rawBiases  V.! i
        weightVec = rawWeights V.! i
        len = min (V.length weightVec) (V.length as)
        x  = foldr (\j total -> total + (as V.! j) * (weightVec V.! i))
                   0
                   ([0..len-1] :: [Int])
    in bias + x
    -- The above code is an optimized form of:
    -- > V.zipWith (+) rawBiases $ sum . V.zipWith (*) as <$> rawWeights
    -- It does only one vector allocation
  where
    rawWeights = unWeights wvs
    rawBiases  = unBiases bs

feed :: V.Vector Float -> Brain -> V.Vector Float
feed xs = foldl' (((relu <$>) .) . zLayer) xs . brainLayers

-- | Returns a list of (WeightedInputs, Activations) of each
--   layer, from last layer to first
revaz :: V.Vector Float -> Brain -> (WeightedInputs, Activations)
revaz xs = foldl' f ([xs], []) . brainLayers
  where
    f (avs, zs) layer@(Layer bs wms) =
        let av  = V.head avs
            zs' = zLayer av layer
        in (V.cons (relu <$> zs') avs, V.cons zs' zs)

dCost :: Float -> Float -> Float
dCost !a !y | y == 1 && a >= y = 0
            | otherwise        = a - y

deltas :: V.Vector Float -> V.Vector Float -> Brain -> (Activations, Deltas)
deltas xv yv brain =
    let (avsi, zvsi) = revaz xv brain
        av          = V.head avsi
        zv          = V.head zvsi
        zvs         = V.tail zvsi

        delta0      = V.zipWith (*) (V.zipWith dCost av yv) (relu' <$> zv)
        activations = V.reverse avsi
        deltas'     = f
            (transposeVect . unWeights . layerWeights <$> V.reverse layers)
            zvs
            [delta0]
    in (activations, deltas')
  where
    layers = brainLayers brain

    f _    zvsi dvsi | V.null zvsi = dvsi
    f wmsi zvsi dvsi =
        let wm  = V.head wmsi
            dv  = V.head dvsi
            zv  = V.head zvsi
            wms = V.tail wmsi
            zvs = V.tail zvsi
            dvs = V.tail dvsi
        in f wms zvs . (`V.cons` dvs) $
            V.zipWith (*) [sum $ V.zipWith (*) row dv | row <- wm]
                          (relu' <$> zv)

transposeVect :: V.Vector (V.Vector Float) -> V.Vector (V.Vector Float)
transposeVect vect
    | V.null vect          = []
    | V.null (V.head vect) = transposeVect (V.tail vect)
    | otherwise            =
        let vh   = V.head vect
            x    = V.head vh
            xs   = V.tail vh
            xss  = V.tail vect
        in (x `V.cons` [V.head h | h <- xss]) `V.cons`
           transposeVect (xs `V.cons` [V.tail t | t <- xss])

-- | Learning rate
eta :: Float
eta = 0.002

descend :: V.Vector Float -> V.Vector Float -> V.Vector Float
descend av dv = V.zipWith (-) av ((eta *) <$> dv)

learn :: V.Vector Float -> V.Vector Float -> Brain -> Brain
learn xv yv brain@(Brain layers) =
    let (avs, dvs) = deltas xv yv brain
        biases = Biases <$> V.zipWith descend
                                (unBiases . layerBiases <$> layers)
                                dvs
    in Brain $
        V.zipWith Layer biases . fmap Weights $
        V.zipWith3
            (\wvs av dv -> V.zipWith (\wv d -> descend wv ((d*) <$> av)) wvs dv)
            (unWeights . layerWeights <$> layers)
            avs
            dvs

-------------
-- Display --
-------------

getImage :: Num a => BS.ByteString -> Int64 -> V.Vector Float
getImage s n =
    fromIntegral .
    BS.index s .
    (n*28^(2::Word8) + 16 +) <$> [0..28^(2::Word8) - 1]

getX :: BS.ByteString -> Int64 -> V.Vector Float
getX     s n =
    let img = getImage s n
    in (img :: V.Vector Float) `deepseq` fmap (/ 256) img
--(/ 256) <$> getImage s n

getLabel :: Num a => BS.ByteString -> Int64 -> a
getLabel s n = fromIntegral $ BS.index s (n + 8)

getY :: Fractional a => BS.ByteString -> Int64 -> V.Vector a
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
trainImages = decompress <$>
    BS.readFile "data/train-images-idx3-ubyte.gz"
trainLabels :: IO BS.ByteString
trainLabels = decompress <$>
    BS.readFile "data/train-labels-idx1-ubyte.gz"
testImages :: IO BS.ByteString
testImages = decompress <$>
    BS.readFile "data/t10k-images-idx3-ubyte.gz"
testLabels :: IO BS.ByteString
testLabels = decompress <$>
    BS.readFile "data/t10k-labels-idx1-ubyte.gz"

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
    print smart
    -- Example
    n <- (`mod` testLen) <$> randomIO
    putStrLn . renderImage n =<< testImages
    print $ bestGuess (getX testI n) smart
    -- Summary
    guesses' <- guesses smart testI
    putStr . show . sum $ fromEnum <$> V.zipWith (==) guesses' (answers testL)
    putStrLn $ "/" ++ show testLen
  where
    trainLen = 15000 --15000
    testLen  = 999--9999

    guesses brain testI =
        return $ V.map (\n -> bestGuess (getX testI n) brain) [0..testLen]

    answers testL = getLabel testL <$> V.fromList [0..testLen]

    bestGuess :: V.Vector Float -> Brain -> Float
    bestGuess image brain = bestOf $ feed image brain

    bestOf :: V.Vector Float -> Float
    bestOf xs = fst . maximumBy (comparing snd) . V.zip [0..fromIntegral (length xs)] $ xs

    smartBrain n b trainI trainL =
      foldl'
        (\br -> const $ foldl'
            (\b' n -> learn (getX trainI n) (getY trainL n) b')
            br
            ([0..trainLen] :: [Int64]))
        b
        ([1..n] :: [Int])

    renderImage n testI = unlines $
        take 28 $ take 28 <$> iterate (drop 28) (render . floor <$> V.toList (getImage testI n))

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
