module BasicIRGMatrix where

import IRGMatrix

import           Data.Vector (Vector, (!))
import qualified Data.Vector as V

data BasicIRGMatrix where
    BasicIRGMatrix :: Vector (Vector Double) -> BasicIRGMatrix

newBasicIRGMatrix :: Vector (Vector Double) -> BasicIRGMatrix
newBasicIRGMatrix elements = let
    lengths = V.map V.length elements
    in if
        | V.null elements -> error "Empty matrix elemets."
        | V.and $ V.zipWith (==) lengths $ V.tail lengths -> BasicIRGMatrix elements
        | otherwise -> error "Inconsistent row sizes."

instance IRGMatrixClass BasicIRGMatrix where
    -- Returns length of first element in vector of vectores.
    getRowsCount (BasicIRGMatrix m) = V.length (m ! 0)
    -- Returns length of vector of vektors (matrix).
    getColsCount (BasicIRGMatrix m) = V.length m

    get mb@(BasicIRGMatrix m) i j = if
        | (i < 0 || i >= getRowsCount mb -> error "Row index out of boundaries."
        | j < 0 || j >= getColsCount mb -> error "Columnes index out of boundaries."
        | otherwise -> m ! j ! i
