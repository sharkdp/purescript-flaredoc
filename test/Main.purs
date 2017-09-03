module Test.Main where

import Prelude
import Data.Array
import Data.Generic (class Generic)
import Data.Int (even)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Test.FlareDoc (class Interactive, withPackage, flareDoc)


newtype UnconsRecord = UnconsRecord { head :: Int, tail :: Array Int }

derive instance genericUnconsRecord :: Generic UnconsRecord

main = do
  withPackage "purescript-arrays.json" $ \dict -> do
    let doc :: forall t. Interactive t => String -> t -> _
        doc = flareDoc dict "Data.Array"

    doc "range" range
    doc "replicate" (replicate :: Int -> String -> _)
    doc "group" (group :: Array Int -> _)
    doc "group'" (group' :: Array Int -> _)
    doc "null" (null :: Array Int -> _)
    doc "length" (length :: Array Int -> _)
    doc "cons" (cons :: Int -> _)
    doc "snoc" (snoc :: _ -> Int -> _)
    doc "insert" (insert :: Int -> _)
    doc "head" (head :: Array Int -> _)
    doc "last" (last :: Array Int -> _)
    doc "tail" (tail :: Array Int -> _)
    doc "init" (init :: Array Int -> _)
    doc "uncons" (map UnconsRecord <<< uncons :: Array Int -> _)
    doc "index" (index :: Array String -> _)
    doc "elemIndex" (elemIndex :: String -> _)
    doc "elemLastIndex" (elemLastIndex :: String -> _)
    doc "findIndex" (findIndex even)
    doc "findLastIndex" (findLastIndex even)
    doc "insertAt" (insertAt :: _ -> String -> _)
    doc "deleteAt" (deleteAt :: _ -> Array String -> _)
    doc "updateAt" (updateAt :: _ -> String -> _)
    doc "modifyAt" $ \i -> modifyAt i (_ * 10)
    doc "reverse" (reverse :: Array Int -> _)
    doc "concat" (concat :: Array (Array Int) -> _)
    doc "concatMap" (concatMap (\n -> [n, n + 1]))
    doc "filter" (filter even)
    doc "partition" (partition even)
    doc "catMaybes" (catMaybes :: Array (Maybe Int) -> _)
    doc "sort" (sort :: Array Int -> _)
    doc "slice" (slice :: Int -> Int -> Array String -> _)
    doc "singleton" (singleton :: Int -> _)
    doc "take" (take :: Int -> Array Int -> _)
    doc "takeWhile" (takeWhile even)
    doc "drop" (drop :: Int -> Array Int -> _)
    doc "dropWhile" (dropWhile even)
    doc "span" (span even)
    doc "nub" (nub :: Array Int -> _)
    doc "union" (union :: Array Int -> _)
    doc "delete" (delete :: Int -> _)
    doc "(\\\\)" ((\\) :: Array Int -> _)
    doc "intersect" (intersect :: Array Int -> _)
    doc "zip" (zip :: Array Int -> Array String -> _)
    doc "unzip" (unzip :: Array (Tuple Int String) -> _)
