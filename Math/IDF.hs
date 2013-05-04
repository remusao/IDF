
module Math.IDF where

import Data.Packed.Vector
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Util
import Data.Map (Map)
import qualified Data.Map as Map

type Word = String
type Document = [Word]

buildVec :: Int -> Map Word [(Int, Double)] -> Int -> Vector Double
buildVec nbDocs m i =
    let vec = fromList $ map computeCoeff $ Map.elems m
    in vec / constant (norm vec) (dim vec)
    where
        computeCoeff l = logBase 2 $ (fromIntegral $ length l) / fromIntegral nbDocs

countAll :: Int -> Int -> [[Word]] -> Map Word [(Int, Double)] -> Map Word [(Int, Double)]
countAll _ _ [] m = m
countAll n i (d:t) m = countAll n (i + 1) t $ countWords i d m

--
countWords :: Int -> [Word] -> Map Word [(Int, Double)] -> Map Word [(Int, Double)]
countWords _ [] cmap = cmap
countWords i (w:t) cmap = countWords i t $ updateCount
    where
        updateCount =
            case Map.lookup w cmap of
                Nothing -> Map.insert w [(i, 1.0)] cmap
                Just values ->
                    let index = fst . head $ values in
                        if index == i then
                            Map.adjust (\((index, val):t) -> (index, val + 1):t) w cmap
                        else
                            Map.adjust (\l -> (i, 1.0):l) w cmap



idf :: [Document] -> [Vector Double]
idf [] = error "Corpus is empty"
idf documents =
    let n = length documents
        m = countAll n 0 documents $ Map.empty :: Map Word [(Int, Double)]
    in map (buildVec n m) [1..n]
