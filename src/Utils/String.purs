module Utils.String where

import Prelude

import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String.Utils (padStart')

padWithZeroes :: Int -> String -> String
padWithZeroes len =
  replaceAll (Pattern " ") (Replacement "0") <<< padStart' len