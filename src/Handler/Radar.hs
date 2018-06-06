{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Radar where

import Import
import Database.Persist.Postgresql
import Text.Cassius
import Text.Lucius
import Text.Julius

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

getRadarR :: Handler Html
getRadarR = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
                (Just id) -> do
                    return id
                _ ->
                    return ""
    areaUm <- runDB $ selectList [AreaOrdem ==. 1] [Asc AreaOrdem]
    buraconegro <- case (safeHead(areaUm)) of
                    Just (Entity _ resto) -> do redirect (RadarIndiceR 1)
                    _ -> return Nothing
    --ack <- return $ show "a"
    defaultLayout $ do
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/home.julius")
        toWidget $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/header.hamlet")
        [whamlet|
          <main>
            <h1>
              Radar não conseguiu encontrar uma área para mostrar
        |]
        $(whamletFile "templates/footer.hamlet")

getRadarIndiceR :: Int -> Handler Html
getRadarIndiceR ordemcampo = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
                (Just id) -> do
                    return id
                _ ->
                    return ""
    defaultLayout $ do
        addStylesheet $ (StaticR css_materialize_css)
        --addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/home.julius")
        toWidget $(juliusFile "templates/radar.julius")
        toWidget $(cassiusFile "templates/radar.cassius")
        $(whamletFile "templates/header.hamlet")
        $(whamletFile "templates/radar.hamlet")
        $(whamletFile "templates/footerRadar.hamlet")
