module ByteCodeSupport where

import Data.Monoid
import qualified Data.Map as Map
import Data.Map(Map)
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Word
import Data.ByteString.Lazy(ByteString)
import Data.Bits
import ByteCode
import ProgInfo

data BranchingInfo = BranchingInfo
  { branchNext :: Bool, branchOther :: [Int] }
  deriving (Show,Eq,Ord)

instance Monoid BranchingInfo where
  mempty = BranchingInfo True []  -- by default we assume that an instruction branches to the next
  (BranchingInfo n1 o1) `mappend` (BranchingInfo n2 o2) =
    BranchingInfo (n1 && n2) (o1 ++ o2)

extraBranch :: Integral a => a -> BranchingInfo
extraBranch = BranchingInfo True . return . fromIntegral

nextBranch :: Integral a => a -> BranchingInfo
nextBranch = BranchingInfo False . return . fromIntegral

dfltBranch :: BranchingInfo
dfltBranch = mempty


data BranchSource
  = SourceException Word32    -- exception index
  | SourceInstruction Int     -- instruction label
  | SourceStart               -- method entry
  deriving (Eq,Ord,Show)


m'mappend :: (Monoid a, Ord k) => Map k a -> Map k a -> Map k a
m'mappend = Map.unionWith mappend



-- Stack effects
noEffect :: Int
noEffect = 0

popEffect :: Integral a => a -> Int
popEffect n = negate (fromIntegral n)

pushEffect :: Integral a => a -> Int
pushEffect = fromIntegral

chaosEffect :: Int
chaosEffect = 0

