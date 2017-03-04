module IRGVector where

import           Data.Vector (Vector)

class IRGVectorClass v where
    add :: v -> IRGVector -> IRGVector
    sub :: v -> IRGVector -> IRGVector
    get :: v -> Int -> Double
    set :: v -> Int -> Double -> IRGVector
    getDimension :: v -> Int
    scalarMultiply :: v -> Double -> IRGVector
    scalarProduct :: v -> IRGVector -> Double
    norm :: v -> Double
    normalize :: v -> IRGVector
    cosine :: v -> IRGVector -> Double
    vectorProduct :: v -> IRGVector -> IRGVector
    fromHomogeneus :: v -> IRGVector
    copyPart :: v -> Int -> IRGVector
    toHaskellVector :: v -> Vector Double



data IRGVector where
    IRGVector :: IRGVectorClass v => v -> IRGVector

instance IRGVectorClass IRGVector where
    add (IRGVector v) = add v
    sub (IRGVector v) = sub v
    get (IRGVector v) = get v
    set (IRGVector v) = set v
    getDimension (IRGVector v) = getDimension v
    scalarMultiply (IRGVector v) = scalarMultiply v
    scalarProduct (IRGVector v1) = scalarProduct v1
    norm v = sqrt $ scalarProduct v v
    normalize (IRGVector v) = scalarMultiply v ( 1 / norm v)
    cosine (IRGVector v1) v2 = scalarProduct v1 v2 / (norm v1 * norm v2)
    vectorProduct (v@(IRGVector v1)) v2 =
        if getDimension v == 3 && getDimension v2 == 3
            then vectorProduct v1 v2
            else error "Try to calculate vector product of not 3 dimensional vectors."
    fromHomogeneus (IRGVector v) = fromHomogeneus v
    copyPart = copyPart
    toHaskellVector (IRGVector v) = toHaskellVector v
