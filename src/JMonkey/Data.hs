-- | JMonkey data types.
module JMonkey.Data
    (
    -- * Types
      Selector(..)
    , Target(..)
    , CompOp(..)
    , Repeat(..)
    -- * Target
    , doc
    , win
    )
where

-- | Represents id and class.
data Selector = Id String | Class String
    deriving (Show, Eq)

-- | Represents javascript element(s).
data Target = Elem String | Elems String
    deriving (Show, Eq)

-- | Comparision operator.
data CompOp = Equal | Grater | Less
    deriving (Show, Eq)

-- | Repetition.
data Repeat = Once | Endless
    deriving (Show, Eq)

-- | Document element. 
doc :: Target
doc = Elem "document"

-- | Window element. 
win :: Target
win = Elem "window"
