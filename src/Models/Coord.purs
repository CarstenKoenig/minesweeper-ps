module Models.Coord (
    Coord,
    create,
    neighbours,
    random,
    randomCoords
) where

import Prelude

import Control.MonadZero (guard)
import Data.Array as Array
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import Effect.Random (randomInt)


type Coord =
    { row :: Int
    , col :: Int
    }


create :: Int -> Int -> Coord
create row col =
    { row, col }


neighbours :: Coord -> Set Coord
neighbours coord@{row, col} = 
    Set.fromFoldable $ do
        r <- (row + _) <$> Array.range (-1) 1
        c <- (col + _) <$> Array.range (-1) 1
        let neighbourCoord = create r c
        guard $ coord /= neighbourCoord
        pure neighbourCoord


random :: Int -> Int -> Effect Coord
random rows cols = do
    row <- randomInt 0 (rows-1)
    col <- randomInt 0 (cols-1)
    pure { row, col }


randomCoords :: Int -> Int -> Int -> Effect (Set Coord)
randomCoords rows cols n = go Set.empty
    where
    go acc | Set.size acc < n = do
        coord <- random rows cols
        go $ Set.insert coord acc
    go acc = 
        pure acc