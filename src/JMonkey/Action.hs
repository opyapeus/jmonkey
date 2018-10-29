{-# LANGUAGE DeriveFunctor #-}

-- | JMonkey actions.
module JMonkey.Action
  (
  -- * Types
    JMonkey
  , JMonkeyM
  , Action(..)
  , Cond(..)
  -- * Show
  , putLog
  , alert
  -- * Selection
  , select
  -- * Selector Modification
  , add
  , adds
  , remove
  , removes
  -- * On Event
  , on
  , onClick
  , onScroll
  , onChange
  -- * Time Triggered Event
  , onTime
  -- * Conditional Action
  , ifel
  , ifThen
  , elThen
  )
where

import           Control.Monad.Free
import           JMonkey.Data

-- | Empty JMonkey monad.
type JMonkey = JMonkeyM ()

-- | JMonkey monad designed with free monad.
type JMonkeyM = Free Action

-- | JMonkey action AST.
data Action n
  = Log String n
  | Alert String n
  | Select Selector (Target -> n)
  | Add Target Selector n
  | Remove Target Selector n
  | On String Target JMonkey n
  | OnTime Repeat Int JMonkey n
  | If Cond JMonkey JMonkey n
  deriving Functor

-- | JMonkey conditions.
-- These mean boolean value.
data Cond
  = Possess Target Selector -- ^ if all the target have the selector
  | YOffset CompOp Double -- ^ if window.pageYOffset satisfies with given comparision operator and threshold value  

-- | Show console log.
putLog :: String -> JMonkey
putLog s = liftF $ Log s ()

-- | Show alert.
alert :: String -> JMonkey
alert s = liftF $ Alert s ()

-- | Select target by given selector.
select :: Selector -> JMonkeyM Target
select sel = liftF $ Select sel id

-- | Add class or id to the target.
-- 
-- For multiple selectors
--
-- @
-- f :: Target -> [Selector] -> JMonkey
-- f t = mapM_ (`add` t)
-- @
add :: Target -> Selector -> JMonkey
add t sel = liftF $ Add t sel ()

-- | Add class or id to the targets.
-- 
-- For multiple selectors
--
-- @
-- f :: [Target] -> [Selector] -> JMonkey
-- f ts = mapM_ (`adds` ts)
-- @
adds :: [Target] -> Selector -> JMonkey
adds ts sel = mapM_ (\t -> add t sel) ts

-- | Remove class or id from the target.
remove :: Target -> Selector -> JMonkey
remove t sel = liftF $ Remove t sel ()

-- | Remove class or id from the targets.
removes :: [Target] -> Selector -> JMonkey
removes ts sel = mapM_ (\t -> remove t sel) ts

-- | Make event.
on
  :: String -- ^ an event name like "click", "scroll", ...etc
  -> Target -- ^ a target related to the event
  -> JMonkey -- ^ an action triggered by the event
  -> JMonkey
on s t act = liftF $ On s t act ()

-- | Same as `on` "click".
onClick :: Target -> JMonkey -> JMonkey
onClick t act = liftF $ On "click" t act ()

-- | Same as `on` "scroll".
onScroll :: Target -> JMonkey -> JMonkey
onScroll t act = liftF $ On "scroll" t act ()

-- | Same as `on` "change".
onChange :: Target -> JMonkey -> JMonkey
onChange t act = liftF $ On "change" t act ()

-- | Make time triggered event.
onTime
  :: Repeat -- ^ repetition
  -> Int -- ^ delay time (milliseconds)
  -> JMonkey -- ^ an action on the time
  -> JMonkey
onTime r t act = liftF $ OnTime r t act ()

-- | Make conditional action.
ifel
  :: Cond -- ^ a condition
  -> JMonkey -- ^ an action do when the condition is satisfied
  -> JMonkey -- ^ an action do when the condition is not satisfied
  -> JMonkey
ifel c tA fA = liftF $ If c tA fA ()

-- | Only true conditional action of `ifel`
ifThen :: Cond -> JMonkey -> JMonkey
ifThen c tA = ifel c tA noop

-- | Only false conditional action of `ifel`
elThen :: Cond -> JMonkey -> JMonkey
elThen c = ifel c noop

noop :: JMonkey
noop = Pure ()
