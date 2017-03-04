module IRGVector where

import           Data.Vector (Vector)

class IRGVectorClass v where
    add :: v -> IRGVector -> IRGVector
    toHaskellVector :: v -> Vector Double

data IRGVector where
    IRGVector :: IRGVectorClass v => v -> IRGVector

instance IRGVectorClass IRGVector where
    add (IRGVector v) = add v
    toHaskellVector (IRGVector v) = toHaskellVector v
