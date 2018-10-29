{-# LANGUAGE OverloadedStrings #-}

module Frontend.Html where

import           Lucid
import           JMonkey                        ( interpretString )
import           Clay                           ( render )
import           Frontend.Css
import           Frontend.Js
import           Data.Gender
import           Data.Person
import           Data.Selector
import           Data.ToAttribute

lucidHtml :: [Person] -> Html ()
lucidHtml ps = do
    doctype_
    html_ $ do
        head_ $ do
            meta_ [charset_ "UTF-8"]
            meta_
                [ name_ "viewport"
                , content_ "width=device-width, initial-scale=1.0"
                ]
            style_ [type_ "text/css"] $ render clayCss
            title_ "Example"
        body_ $ do
            div_ [toAttr (Class AboveHeader)] $ do
                div_ [toAttr (Class First)] "yay!"
                h1_ "Various People"
            header_ [toAttr (Class Head)] $ do
                div_ [toAttr (Id ButtonAll), toClasses [Tab, Current]] "All"
                div_ [toAttr (Id $ Button Man), toAttr (Class Tab)]
                    $ toHtml (show Man)
                div_ [toAttr (Id $ Button Woman), toAttr (Class Tab)]
                    $ toHtml (show Woman)
                div_ [toAttr (Id $ Button Trans), toAttr (Class Tab)]
                    $ toHtml (show Trans)
            div_ . ul_ [toAttr (Class Content)] $ mapM_ box ps
            script_ [type_ "text/javascript"] $ interpretString jMonkeyJs

box :: Person -> Html ()
box p = li_ [toClasses [Box, Group (gender p)]] . dl_ $ do
    dt_ "Name"
    dd_ $ toHtml (name p)
    dt_ "Age"
    dd_ $ toHtml (show $ age p)
    dt_ "Gender"
    dd_ $ toHtml (show $ gender p)
