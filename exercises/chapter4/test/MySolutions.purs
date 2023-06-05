module Test.MySolutions where

import Prelude

import Data.Array (head, null, tail)
import Data.Maybe (fromMaybe)


isEven :: Int -> Boolean
isEven n =
    if n < 0 then
        isEven (-n)
    else if n == 0 then
        true
    else
        not $ isEven (n - 1)

countEven :: Array Int -> Int
countEven xs =
    if null xs then 0
    else
        ((fromMaybe 0 $ head xs) `mod` 2) + countEven (fromMaybe [] (tail xs))