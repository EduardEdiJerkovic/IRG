module IRGMatrix where

class IRGMatrixClass m where
    -- | Gets rows count in matrix.
    getRowsCount :: m -> Int
    -- | Get columnes count in matrix.
    getColsCount :: m -> Int
    -- | Gets value in given coordinates.
    get :: m -> Int -> Int -> Double

data IRGMatrix where
    IRGMatrix :: IRGMatrixClass m => m -> IRGMatrix

instance IRGMatrixClass IRGMatrix where
    getRowsCount (IRGMatrix m) = getRowsCount m

    getColsCount (IRGMatrix m) = getColsCount m

    get (IRGMatrix m) = get m
