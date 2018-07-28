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
            Just (Entity _ resto) -> do return $ Just (pessoaCartao resto)
            _ -> do return Nothing
  sendStatusJSON ok200 (object["existe" .= existe])



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

postUpdateIpR ::  Text -> Handler Value
postUpdateIpR ip = do
  Import.undefined

postInsertRegistroR :: SalaId -> PessoaId -> UTCTime -> Bool -> Bool -> Handler Value
postInsertRegistroR salaid pessoaid timestamp direcao alert = do
  Import.undefined



postPosicaoR :: SalaId -> Int -> Int -> Handler Value
postPosicaoR sid posix posiy = do
  runDB $ Database.Persist.Postgresql.update sid [SalaPosx Database.Persist.Postgresql.=. Just posix]
  runDB $ Database.Persist.Postgresql.update sid [SalaPosy Database.Persist.Postgresql.=. Just posiy]
  sendStatusJSON ok200 (object[])
