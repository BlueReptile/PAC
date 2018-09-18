{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Json where

import Import
import Prelude
import Database.Persist.Postgresql
import Network.HTTP.Types.Status
import Capsula
import Data.Time.Format
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import Database.Persist.Class




getJsonTrocaCartaoPorNomeR :: Text -> Handler Value
getJsonTrocaCartaoPorNomeR cartaorecebido = do
    --caraLista <- runDB $ selectList [PessoaCartao ==. (readMaybe $ show $ cartaorecebido)] [Asc PessoaNome]
    caraNome <- runDB $ getBy $ UniqueCartao cartaorecebido
    pegacaraNome <- case caraNome of
                    Just (Entity _ resto) -> do return $ Just (pessoaNome resto)
                    _ -> do return Nothing

    sendStatusJSON ok200 (object["nomedocara" .= pegacaraNome])
--sendStatusJSON noContent204 (object[])

getFindCardR :: Text -> Handler Value
getFindCardR cid = do
  maybeExiste <- runDB $ getBy $ UniqueCartao cid
  existe <- case maybeExiste of
            Just (Entity _ resto) -> do return $ Just (pessoaNivel resto)
            _ -> do return Nothing
  sendStatusJSON ok200 (object["acesso" .= existe])


getSalaNivelR  :: Text -> Handler Value
getSalaNivelR  ip = do
    maybeSalaNivel <- runDB
                $ E.select
                $ E.from $ \(sala `E.InnerJoin` arduino) -> do
                    E.on $ sala ^. SalaArid E.==. arduino ^. ArduinoId
                    E.where_ (arduino ^. ArduinoIp E.==. E.val ip)
                    return
                        ( sala ^. SalaNivel )
    salaNivel <- case maybeSalaNivel of
              [E.Value sala] -> do return $ Just sala
              _ -> do return Nothing
    sendStatusJSON ok200 (object["nivel" .= salaNivel])



getGetSalaIdR :: Text -> Handler Value
getGetSalaIdR ip = do
  maybeSalaId <- runDB
              $ E.select
              $ E.from $ \(sala `E.InnerJoin` arduino) -> do
                  E.on $ sala ^. SalaArid E.==. arduino ^. ArduinoId
                  E.where_ (arduino ^. ArduinoIp E.==. E.val ip)
                  return
                      ( sala ^. SalaId )
  salaId <- case maybeSalaId of
            [E.Value sala] -> do return $ Just sala
            _ -> do return Nothing
  sendStatusJSON ok200 (object["salaId" .= salaId])


getGetPessoaIdR :: Text -> Handler Value
getGetPessoaIdR cartaorecebido = do
  maybeExiste <- runDB $ getBy $ UniqueCartao cartaorecebido
  existe <- case maybeExiste of
            Just (Entity personId person) -> do return $ Just personId
            Nothing -> do return Nothing
  sendStatusJSON ok200 (object["id" .= existe])

postUpdateIpR :: Handler Value
postUpdateIpR = do
  --post <- requireJsonBody :: Handler Value
  --runDB $ insert post
  --sendResponseStatus status201 ("CREATED" :: Text)
  Prelude.undefined

putPutIpR :: ArduinoId -> Handler Value
putPutIpR aid = do
    post <- requireJsonBody :: Handler Arduino
    runDB $ replace aid post
    sendResponseStatus status200 ("UPDATED" :: Text)

postInsertRegistroR :: Handler Value
postInsertRegistroR = do
  post <- requireJsonBody :: Handler Registro
  _    <- runDB $ insert post
  sendResponseStatus status201 ("CREATED" :: Text)


postPosicaoR :: SalaId -> Text -> Text -> Handler Value
postPosicaoR sid posix posiy = do
    addHeader "Access-Control-Allow-Origin" "*"
    addHeader "Access-Control-Allow-Methods" "PUT, OPTIONS"
    runDB $ Database.Persist.Postgresql.update sid [SalaPosx Database.Persist.Postgresql.=. posix]
    runDB $ Database.Persist.Postgresql.update sid [SalaPosy Database.Persist.Postgresql.=. posiy]
    sendStatusJSON ok200 (object[])
