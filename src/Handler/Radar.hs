{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Radar where

import Import
import Prelude
import Database.Persist.Postgresql
import Text.Cassius
import Text.Lucius
import Text.Julius

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

desemcapsula :: Maybe Text -> Text
desemcapsula (Just a) = a
desemcapsula Nothing = ""








getRadarR :: Handler Html
getRadarR = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
                (Just id) -> do
                    return id
                _ ->
                    return ""
    areaUm <- runDB $ selectList [AreaOrdem ==. 1] [Asc AreaOrdem]
    pegaareaUm <- case (safeHead(areaUm)) of
                    Just (Entity _ resto) -> do redirect (RadarIndiceR 1 ("false"))
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

getRadarIndiceR :: Int -> Text -> Handler Html
getRadarIndiceR ordemcampo automatico = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
                (Just id) -> do
                    return id
                _ ->
                    return ""
    areaMax <- runDB $ selectList [] [Desc AreaOrdem]
    pegaareaMax <- case (safeHead(areaMax)) of
                    Just (Entity _ resto) -> if (((areaOrdem resto) < ordemcampo) || (ordemcampo < 1)) then redirect (RadarIndiceR 1 automatico) else return Nothing
                    _ -> do redirect (RadarIndiceR 1 automatico)


    areanominho <- runDB $ selectList [AreaOrdem ==. ordemcampo] [Asc AreaOrdem]
    pegaareaNome <- case (safeHead(areanominho)) of
                    Just (Entity _ resto) -> do return $ Just (areaNome resto)
                    _ -> do return Nothing


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
