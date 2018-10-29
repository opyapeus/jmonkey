module Frontend.Js where

import           JMonkey                 hiding ( Id
                                                , Class
                                                )
import           Data.Gender
import           Data.Selector           hiding ( Selector )
import           Data.ToAttribute

jMonkeyJs :: JMonkey
jMonkeyJs = do
    -- Header to be fixed from the middle
    header <- select $ toAttr (Class Head)
    onScroll win $ ifel (YOffset Grater 200)
                        (add header $ toAttr (Class HeadFixed))
                        (remove header $ toAttr (Class HeadFixed))

    -- Loops automatically change classes
    d <- select $ toAttr (Class First)
    onTime Endless 3000
        $ ifel
              (Possess d $ toAttr (Class First))
              (do
                  remove d $ toAttr (Class First)
                  add d $ toAttr (Class Second)
              )
        $ ifel
              (Possess d $ toAttr (Class Second))
              (do
                  remove d $ toAttr (Class Second)
                  add d $ toAttr (Class Third)
              )
        $ ifThen
              (Possess d $ toAttr (Class Third))
              (do
                  remove d $ toAttr (Class Third)
                  add d $ toAttr (Class First)
              )

    -- People filter
    groupMan   <- select $ toAttr (Class $ Group Man)
    groupWoman <- select $ toAttr (Class $ Group Woman)
    groupTrans <- select $ toAttr (Class $ Group Trans)
    btnMan     <- select $ toAttr (Id $ Button Man)
    btnWoman   <- select $ toAttr (Id $ Button Woman)
    btnTrans   <- select $ toAttr (Id $ Button Trans)
    btnAll     <- select $ toAttr (Id ButtonAll)
    onClick btnMan $ do
        removeAdds groupMan [groupWoman, groupTrans] $ toAttr (Class Hide)
        addRemoves btnMan [btnWoman, btnTrans, btnAll] $ toAttr (Class Current)
    onClick btnWoman $ do
        removeAdds groupWoman [groupMan, groupTrans] $ toAttr (Class Hide)
        addRemoves btnWoman [btnMan, btnTrans, btnAll] $ toAttr (Class Current)
    onClick btnTrans $ do
        removeAdds groupTrans [groupMan, groupWoman] $ toAttr (Class Hide)
        addRemoves btnTrans [btnMan, btnWoman, btnAll] $ toAttr (Class Current)
    onClick btnAll $ do
        removes [groupMan, groupWoman, groupTrans] $ toAttr (Class Hide)
        addRemoves btnAll [btnMan, btnWoman, btnTrans] $ toAttr (Class Current)
  where
    removeAdds :: Target -> [Target] -> Selector -> JMonkey
    removeAdds t ts sel = do
        remove t sel
        adds ts sel
    addRemoves :: Target -> [Target] -> Selector -> JMonkey
    addRemoves t ts sel = do
        add t sel
        removes ts sel
