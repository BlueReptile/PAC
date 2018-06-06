{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Radar where

import Import
import Database.Persist.Postgresql
import Text.Lucius
import Text.Julius

getRadarR :: Handler Html
getRadarR = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
                (Just id) -> do
                    return id
                _ ->
                    return ""
    defaultLayout $ do
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        $(whamletFile "templates/radar.hamlet")
        toWidget $(juliusFile "templates/radar.julius")
        toWidget $(luciusFile "templates/radar.lucius")
        $(whamletFile "templates/header.hamlet")
        $(whamletFile "templates/footer.hamlet")
