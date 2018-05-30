{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Text.Julius
import Text.Lucius

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/home.julius")
        toWidget $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/home.hamlet")
        $(whamletFile "templates/footer.hamlet")
