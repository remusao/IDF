
module Math.IDF(buildFeatureVectors) where

import Data.Packed.Vector(Vector, fromList, mapVector)
import Numeric.LinearAlgebra.Util(norm)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as L


-- Just to make code cleaner
type Word = String
type Document = [Word]


-- Build the feature vector corresponding to the ith document
buildVec :: Int -> Map Word [(Int, Double)] -> Int -> Vector Double
buildVec n m i =
    let vec = fromList $ map computeCoeff $ Map.elems m
        vecNorm = norm vec
    in mapVector (\val -> val / vecNorm) vec
    where
        computeCoeff :: [(Int, Double)] -> Double
        computeCoeff l = tf * idf
            where
                df = fromIntegral $ length l
                tf = case L.find (\e -> fst e == i) l of
                    Nothing -> 0.0
                    Just val -> (snd val)
                idf = log ((fromIntegral n) / df)



-- Parse every documents to count the words and build the map.
-- The map is as follow :
--
--  Word1 -> [(doc1, nbOccurences), ..., (docn, nbOccurences)]
--  ...
--  Wordn -> [(doc1, nbOccurences), ..., (docn, nbOccurences)]
--
-- This is useful to know the number of occurences of a word in a
-- particular document and to know in how many documents a word appears.
--
-- i : id of the current document
-- (d:t) : list of documents
-- m : The map
countAllWords :: Int -> [[Word]] -> Map Word [(Int, Double)] -> Map Word [(Int, Double)]
countAllWords _ [] m = m
countAllWords i (d:t) m = countAllWords (i + 1) t $! countWords i d m


-- Given a document, update the map to add the occurences of its words
-- For each word, two cases are possible :
--  1) The tuple at the head of the list of the entry in the map is
--     already this document => we add one to the number of occurences
--  2) The tuple corresponds to another documents, so we add another tuple
--
--  i : The id of the document
--  document : The list of words
--  map : The map to update
countWords :: Int -> [Word] -> Map Word [(Int, Double)] -> Map Word [(Int, Double)]
countWords _ [] cmap = cmap
countWords currentDoc (w:next) cmap = countWords currentDoc next $! updateCount
    where
        -- Update the map for the current word
        updateCount =
            case Map.lookup w cmap of
                Nothing -> Map.insert w [(currentDoc, 1.0)] cmap
                Just values ->
                    let documentId = fst . head $ values in
                        if documentId == currentDoc then -- Updates the value
                            Map.adjust (\((_ , val):t) -> (currentDoc, val + 1.0):t) w cmap
                        else -- Create a new tuple at the head of the list
                            Map.adjust (\l -> (currentDoc, 1.0):l) w cmap


-- Build one feature vector for each document using the
-- termi frequency and inverse document frequency (tf-idf)
buildFeatureVectors :: [Document] -> [Vector Double]
buildFeatureVectors [] = error "Corpus is empty"
buildFeatureVectors documents =
    let n = length documents
        m = countAllWords 0 documents $ Map.empty :: Map Word [(Int, Double)]
    in map (buildVec n m) [0..n-1]
