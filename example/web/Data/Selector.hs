module Data.Selector where

import           Data.Gender

data Selector = Id IdName | Class ClassName
    deriving (Show, Eq)

data IdName
    = Button Gender
    | ButtonAll
    deriving (Show, Eq)

data ClassName
    = First
    | Second
    | Third
    | Hide
    | Tab
    | Current
    | AboveHeader
    | Head
    | HeadFixed
    | Content
    | Box
    | Group Gender
    deriving (Show, Eq)
