{-# LANGUAGE TypeOperators #-}
module Attr where

import Control.DeepSeq (NFData(..))

import qualified Data.Text as T

-- | Training data
type Training a b = [a :=> b]

-- | A labeled data point
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
    attrName  :: !T.Text,
    -- | Attribute categories
    attrTests :: [(T.Text, a -> Bool)]
    }

instance Show (Attr a) where
    show = show . attrName
instance Eq (Attr a) where
    x == y = attrName x == attrName y

instance NFData (Attr a) where
    rnf (Attr name xs) = rnf xs
