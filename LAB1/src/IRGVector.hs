module IRGVector where

import           Data.Vector (Vector)

class IRGVectorClass v where
    add :: v -> IRGVector -> IRGVector
    sub :: v -> IRGVector -> IRGVector
    get :: v -> Int -> Double
    set :: v -> Int -> Double -> IRGVector
    toHaskellVector :: v -> Vector Double

data IRGVector where
    IRGVector :: IRGVectorClass v => v -> IRGVector

instance IRGVectorClass IRGVector where
    add (IRGVector v) = add v
    sub (IRGVector v) = sub v
    get (IRGVector v) = get v
    set (IRGVector v) = set v
    toHaskellVector (IRGVector v) = toHaskellVector v
