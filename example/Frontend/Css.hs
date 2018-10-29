{-# LANGUAGE OverloadedStrings #-}

module Frontend.Css where

import           Clay
import           Data.Gender
import           Data.Selector
import           Data.ToAttribute

zero :: Size LengthUnit
zero = 0

p100 :: Size Percentage
p100 = 100

clayCss :: Css
clayCss = do
    body ? do
        sym margin  zero
        sym padding zero
        backgroundColor "#ffffff"

    toAttr (Class AboveHeader) ? do
        height (px 200)
        backgroundColor "#add8e6"

    toAttr (Class Head) ? do
        backgroundColor "#f8f8ff"
        position absolute
        width p100

        toAttr (Class Tab) ? do
            display inlineBlock
            width (px 150)

        toAttr (Class Current) ? do
            backgroundColor "#d3d3d3"
            fontWeight bold

    toAttr (Class HeadFixed) ? do
        position fixed
        top zero

    toAttr (Class Content) ? do
        display flex
        flexWrap (FlexWrap "wrap") -- FIXME: flexWrap wrap
        justifyContent spaceBetween
        listStyleType none
        sym margin  zero
        sym padding zero

        toAttr (Class Box) ? do
            width (px 200)
            sym padding (px 20)
            marginBottom (px 30)

    toAttr (Class First) ? backgroundColor "#ff0000"
    toAttr (Class Second) ? backgroundColor "#00ff00"
    toAttr (Class Third) ? backgroundColor "#0000ff"

    toAttr (Class $ Group Man) ? backgroundColor "#00ced1"
    toAttr (Class $ Group Woman) ? backgroundColor "#f08080"
    toAttr (Class $ Group Trans) ? backgroundColor "#eee8aa"

    toAttr (Class Hide) ? display none
