{-# LANGUAGE QuasiQuotes #-}

-- | JMonkey interpreter using jmacro backend.
module JMonkey.Interpreter 
        ( 
        -- * Interpreter
          interpretString
        , interpretJStat
        ) where

import           Control.Monad.Free
import           Language.Javascript.JMacro
import           Text.Casing
import           JMonkey.Action
import           JMonkey.Data

-- | Interpret to javascript string. 
interpretString :: JMonkey -> String
interpretString = show . renderJs . interpretJStat

toStr :: Selector -> String
toStr (Id s) = '#' : s
toStr (Class s) = '.' : s

onEvent :: String -> String
onEvent = mappend "on"

jvar :: String -> JStat
jvar s = DeclStat (StrI s) Nothing

jref :: String -> JExpr
jref = ValExpr . JVar . StrI

-- TODO: check when compiling?
forbidden :: JStat
forbidden = [jmacro| throw "forbidden action: id corresponds to multiple elements." |]

-- | Interpret to jmacro statement. 
interpretJStat :: JMonkey -> JStat
interpretJStat (Free (Log s n)) = [jmacro| console.log `s` |] <> interpretJStat n
interpretJStat (Free (Alert s n)) = [jmacro| alert `s` |] <> interpretJStat n
interpretJStat (Free (Select sel@(Id s) n)) = [jmacro|
        `jvar tmp`;
        `jref tmp` = document.querySelector(`toStr sel`);
|] <> interpretJStat (n $ Elem tmp)
        where tmp = "i_" ++ snake s -- TODO: allocate by generator?
interpretJStat (Free (Select sel@(Class s) n)) = [jmacro|
        `jvar tmps`;
        `jref tmps` = document.querySelectorAll(`toStr sel`);
|] <> interpretJStat (n $ Elems tmps)
        where tmps = "c_" ++ snake s -- TODO: allocate by generator?
interpretJStat (Free (Add (Elem t) (Class s) n)) = [jmacro| `jref t`.classList.add(`s`); |] <> interpretJStat n
interpretJStat (Free (Add (Elem t) sel@(Id _) n)) = [jmacro| `jref t`.id = `toStr sel`; |] <> interpretJStat n
interpretJStat (Free (Add (Elems t) (Class s) n)) = [jmacro|
        `jref t`.forEach( function e {
                e.classList.add(`s`);
        })
|] <> interpretJStat n
interpretJStat (Free (Add (Elems _) (Id _) n)) = forbidden <> interpretJStat n
interpretJStat (Free (Remove (Elem t) (Class s) n)) = [jmacro| `jref t`.classList.remove(`s`); |] <> interpretJStat n
interpretJStat (Free (Remove (Elem t) sel@(Id _) n)) = [jmacro| if (`jref t`.id == `toStr sel`) { `jref t`.id = ""; } |] <> interpretJStat n
interpretJStat (Free (Remove (Elems t) (Class s) n)) = [jmacro|
        `jref t`.forEach( function e {
                e.classList.remove(`s`);
        })
|] <> interpretJStat n
interpretJStat (Free (Remove (Elems _) (Id _) n)) = forbidden <> interpretJStat n        
interpretJStat (Free (On s (Elem t) a n)) = [jmacro|
        `jref t`[`event`] = function {
                `interpretJStat a`;
        }
|] <> interpretJStat n
        where event = onEvent s
interpretJStat (Free (On s (Elems t) a n)) = [jmacro|
        `jref t`.forEach( function e {
                e[`event`] = function { `interpretJStat a`; }
        });
|] <> interpretJStat n
        where event = onEvent s
interpretJStat (Free (OnTime Once i a n)) = [jmacro|
        var f = function { `interpretJStat a`; };
        setTimeout(f, `i`);
|] <> interpretJStat n
interpretJStat (Free (OnTime Endless i a n)) = [jmacro|
        var f = function { `interpretJStat a`; };
        setInterval(f, `i`);
|] <> interpretJStat n
interpretJStat (Free (If (Possess target sel) tA fA n)) = case (target, sel) of
        (Elem t, Class s) -> ifElse [jmacroE| `jref t`.classList.contains(`s`) |] tA fA <> interpretJStat n
        (Elems t, Class s) -> ifElse [jmacroE| Array.from(`jref t`).every(function e { return e.classList.contains(`s`); }) |] tA fA <> interpretJStat n
        (Elem t, Id s) -> ifElse [jmacroE| `jref t`.id == `s` |] tA fA <> interpretJStat n
        (Elems _, Id _) -> forbidden <> interpretJStat n
interpretJStat (Free (If (YOffset op v) tA fA n)) =
        let cond = case op of
                Equal -> [jmacroE| window.pageYOffset == `v` |]
                Grater -> [jmacroE| window.pageYOffset > `v` |]
                Less -> [jmacroE| window.pageYOffset < `v` |]
        in ifElse cond tA fA <> interpretJStat n
interpretJStat (Pure _) = nullStat

ifElse :: JExpr -> JMonkey -> JMonkey -> JStat
ifElse cond tA fA = [jmacro|
        if (`cond`) {
                `interpretJStat tA`;
        } else {
                `interpretJStat fA`;
        }
|] 