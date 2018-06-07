{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Json where

import Import
import Database.Persist.Postgresql
import Network.HTTP.Types.Status

getJsonNumSalasR :: Handler Value
getJsonNumSalasR = do
    sendStatusJSON noContent204 (object[])

getJsonAreaR :: Int -> Handler Value
getJsonAreaR ordemcampo = do
    sendStatusJSON noContent204 (object[])

getJsonSalaR :: SalaId -> Handler Value
getJsonSalaR sid = do
    sendStatusJSON noContent204 (object[])
