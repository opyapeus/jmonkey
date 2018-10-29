module Data.Person where

import           Data.Gender

data Person = Person
  { name :: String
  , age  :: Int
  , gender  :: Gender
  } deriving (Show, Eq)

people :: [Person]
people =
  [ Person "A" 20 Man
  , Person "B" 21 Woman
  , Person "C" 22 Trans
  , Person "D" 23 Man
  , Person "E" 24 Woman
  , Person "F" 25 Trans
  , Person "G" 26 Man
  , Person "H" 27 Woman
  , Person "I" 28 Trans
  , Person "J" 29 Man
  , Person "K" 30 Woman
  , Person "L" 31 Trans
  , Person "M" 32 Man
  , Person "N" 33 Woman
  , Person "O" 34 Trans
  , Person "P" 35 Man
  , Person "Q" 36 Woman
  , Person "R" 37 Trans
  , Person "S" 38 Man
  , Person "T" 39 Woman
  , Person "U" 40 Trans
  , Person "V" 41 Man
  , Person "W" 42 Woman
  , Person "X" 43 Trans
  , Person "Y" 44 Man
  , Person "Z" 45 Woman
  ]
