
module Math.IDF(idf) where

import Data.Packed.Vector(Vector, fromList, dim)
import Numeric.LinearAlgebra(constant)
import Numeric.LinearAlgebra.Util(norm)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as L

-- Just to make code cleaner
type Word = String
type Document = [Word]

--
buildVec :: Map Word [(Int, Double)] -> (Int, Int) -> Vector Double
buildVec m (sizei, i) =
    let vec = fromList $ map computeCoeff $ Map.elems m
    in vec / constant (norm vec) (dim vec)
    where
        computeCoeff :: [(Int, Double)] -> Double
        computeCoeff l = tf * (logBase 2 (1 + (fromIntegral sizei) / df))
            where
                df :: Double
                df = fromIntegral $ length l
                tf :: Double
                tf = case L.find (\e -> fst e == i) l of
                    Nothing -> 0.0
                    Just val -> (snd val) / fromIntegral sizei



-- Call countWords on each text
countAll :: Int -> Int -> [[Word]] -> Map Word [(Int, Double)] -> Map Word [(Int, Double)]
countAll _ _ [] m = m
countAll n i (d:t) m = countAll n (i + 1) t $ countWords i d m


-- In the ith document of the corpus. Takes a map with the counts of the 1, 2, (i - 1)
-- previous text and updates it with the next text.
countWords :: Int -> [Word] -> Map Word [(Int, Double)] -> Map Word [(Int, Double)]
countWords _ [] cmap = cmap
countWords i (w:next) cmap = countWords i next $ updateCount
    where
        updateCount =
            case Map.lookup w cmap of
                Nothing -> Map.insert w [(i, 1.0)] cmap
                Just values ->
                    let documentId = fst . head $ values in
                        if documentId == i then -- Updates the value
                            Map.adjust (\((docId, val):t) -> (docId, val + 1):t) w cmap
                        else -- Create a new tuple at the head of the list
                            Map.adjust (\l -> (i, 1.0):l) w cmap

--
idf :: [Document] -> [Vector Double]
idf [] = error "Corpus is empty"
idf documents =
    let n = length documents
        m = countAll n 0 documents $ Map.empty :: Map Word [(Int, Double)]
    in map (buildVec m) (zip (map length documents) [1..n])
