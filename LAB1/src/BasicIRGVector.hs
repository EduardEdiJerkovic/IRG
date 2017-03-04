module BasicIRGVector where

import           IRGVector

import           Data.Vector (Vector, (!), (++), (//))
import qualified Data.Vector as V

data BasicIRGVector = BasicIRGVector (Vector Double)

instance IRGVectorClass BasicIRGVector where
    add v1 v2 = IRGVector $ BasicIRGVector $ V.zipWith (+) (toHaskellVector v1) (toHaskellVector v2)
    sub v1 v2 = IRGVector $ BasicIRGVector $ V.zipWith (-) (toHaskellVector v1) (toHaskellVector v2)
    get (BasicIRGVector v) = (v !)
    set (BasicIRGVector v) i a = IRGVector $ BasicIRGVector $ v // [(i, a)]
    getDimension (BasicIRGVector v) = V.length v
    scalarMultiply (BasicIRGVector v) a = IRGVector $ BasicIRGVector $ V.map (*a) v
    scalarProduct v1 v2 =  V.sum $ V.zipWith (*) (toHaskellVector v1) (toHaskellVector v2)
    vectorProduct v1 v2 = IRGVector $ BasicIRGVector $ V.fromList [v1 ! 1 * v2 ! 2 - v1 ! 2 * v2 ! 1, v1 ! 2 * v2 ! 0 - v1 ! 0 * v2 ! 2, v1 ! 0 * v2 ! 1 - v1 ! 1 * v2 ! 0]
    fromHomogeneus v = V.map (* V.last) V.take (V.length v - 1)
    copyPart (v@(BasicIRGVector vb)) n =
        if n > getDimension v
            then IRGVector $ BasicIRGVector $ v ++ (V.replicate (n - getDimension v) 0)
            else IRGVector $ BasicIRGVector $ V.take v n
    toHaskellVector (BasicIRGVector v) = v
