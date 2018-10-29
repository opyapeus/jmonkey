{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.ToAttribute where

import           Data.Text                      ( pack )
import           Text.Casing                    ( kebab )
import           Lucid                          ( Attribute
                                                , id_
                                                , class_
                                                )
import qualified Clay                          as C
                                                ( Selector )
import           Clay.Selector                  ( selectorFromText )
import qualified JMonkey                       as J
                                                ( Selector(..) )
import           Data.Selector

class ToAttribute a where
    toAttr :: Selector -> a

instance ToAttribute Attribute where
    toAttr (Id v) = id_ . pack . kebab . show $ v
    toAttr (Class v) = class_ . pack . kebab. show $ v

instance ToAttribute C.Selector where
    toAttr (Id v) = selectorFromText . mappend "#" . pack . kebab . show $ v
    toAttr (Class v) = selectorFromText . mappend "." . pack . kebab . show $ v

instance ToAttribute J.Selector where
    toAttr (Id v) = J.Id . kebab . show $ v
    toAttr (Class v) = J.Class . kebab . show $ v

-- TODO: div [class_ "a", class_ "b"] "X" -> <div class="ab">X</div>
-- NOTE: https://github.com/chrisdone/lucid/issues/84
toClasses :: [ClassName] -> Attribute
toClasses = class_ . pack . unwords . map (kebab . show)
