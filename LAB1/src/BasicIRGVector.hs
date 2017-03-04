module BasicIRGVector where

import           IRGVector

import           Data.Vector (Vector, (!))
import qualified Data.Vector as V

data BasicIRGVector = BasicIRGVector (Vector Double)

instance IRGVectorClass BasicIRGVector where
    add v1 v2 = IRGVector $ BasicIRGVector $ V.zipWith (+) (toHaskellVector v1) (toHaskellVector v2)
    toHaskellVector (BasicIRGVector v) = v
