module Test.Main where

import Prelude
import Data.Array
import Data.Generic (class Generic)
import Data.Int (even)
import Data.Maybe (Maybe())
import Test.FlareDoc (class Interactive, withPackage, flareDoc)


newtype UnconsRecord = UnconsRecord { head :: Int, tail :: Array Int }

derive instance genericUnconsRecord :: Generic UnconsRecord

main = do
  withPackage "purescript-arrays.json" $ \dict -> do
    let doc :: forall t. Interactive t => String -> t -> _
        doc = flareDoc dict "Data.Array"

    doc "range" range
    doc "replicate" (replicate :: Int -> String -> _)
    doc "uncons" (map UnconsRecord <<< uncons :: Array Int -> _)
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
    doc "index" (index :: Array String -> _)
    doc "elemIndex" (elemIndex :: String -> _)
    doc "elemLastIndex" (elemLastIndex :: String -> _)
    doc "findIndex even" (findIndex even)
    doc "findLastIndex even" (findLastIndex even)
    doc "insertAt" (insertAt :: _ -> String -> _)
    doc "deleteAt" (deleteAt :: _ -> Array String -> _)
    doc "updateAt" (updateAt :: _ -> String -> _)
    doc "modifyAt" $ \i -> modifyAt i (* 10)
    doc "reverse" (reverse :: Array Int -> _)
    doc "concatMap (\\n -> range 0 n)" (concatMap (\n -> range 0 n))
    doc "catMaybes" $ \x y z -> catMaybes [(x :: Maybe String), y, z]
    doc "sort" (sort :: Array Int -> _)
    doc "slice" (slice :: Int -> Int -> Array String -> _)
    doc "singleton" (singleton :: Int -> _)
    doc "take" (take :: Int -> Array Int -> _)
    doc "takeWhile even" (takeWhile even)
    doc "drop" (drop :: Int -> Array Int -> _)
    doc "dropWhile even" (dropWhile even)
    doc "nub" (nub :: Array Int -> _)
    doc "union" (union :: Array Int -> _)
    doc "delete" (delete :: Int -> _)
    doc "(\\\\)" ((\\) :: Array Int -> _)
    doc "intersect" (intersect :: Array Int -> _)
    doc "zip" (zip :: Array Int -> Array String -> _)
