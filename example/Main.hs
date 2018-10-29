module Main where

import           Network.Wai.Handler.Warp       ( run )
import           Network.Wai                    ( Application
                                                , responseLBS
                                                )
import           Network.HTTP.Types             ( status200 )
import           Lucid                          ( renderBS )
import           Frontend.Html
import           Data.Person

main :: IO ()
main = run 3000 app

app :: Application
app _ respond = do
    let bs = renderBS $ lucidHtml people
    respond $ responseLBS status200 [] bs
