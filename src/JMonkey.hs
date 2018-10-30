-- | Jmonkey is very restricted but handy EDSL for javascript.
--
-- It's backend uses <http://hackage.haskell.org/package/free free> for designing DSL and <http://hackage.haskell.org/package/jmacro jmacro> for interpreter.
--
-- Here is some examples.
--
-- Click btn element then add "clicked" class to target elements.
--
-- @
-- btn <- select (Id "btn")
-- target <- select (Class "target")
-- onClick btn $ add target (Class "clicked")
-- @
--
-- If target element(s) have "hide" class then remove, otherwise add "hide" class.
--
-- @
-- ifel (Possess target (Class "hide"))
--      (remove target (Class "hide"))
--      (add target (Class "hide"))
-- @
--
-- Once you define some actions like aboves, you can get javascript string easily by `interpretString`.
module JMonkey
  ( module JMonkey.Action
  , module JMonkey.Data
  , module JMonkey.Interpreter
  )
where

import           JMonkey.Action
import           JMonkey.Data
import           JMonkey.Interpreter
