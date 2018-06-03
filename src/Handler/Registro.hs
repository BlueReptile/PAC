{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Registro where

import Import
import Database.Persist.Postgresql
--import Network.HTTP.Types.Status
import Text.Lucius
import Text.Julius
import Prelude
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import Data.Time.Format

-- areq -> required
-- textField -> campo texto
-- Nothing -> propriedades a mais do campo
-- <$> :: Functor f => (a -> b) -> f a -> f b
-- <*> :: Applicative f => f (a -> b) -> f a -> f b

getRegistroR :: Handler Html
getRegistroR = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
            (Just id) -> do
                return id
            _ -> do
                redirect LoginPageR
    registros <- runDB
                $ E.select
                $ E.from $ \(registro `E.InnerJoin` sala) -> do
                    E.on $ registro ^. RegistroSala E.==. sala ^. SalaId
                    return
                        ( registro ^. RegistroId
                        , sala  ^. SalaNome
                        , registro ^. RegistroDatahora
                        )

        --selectList [] [Asc RegistroDatahora]
    defaultLayout $ do
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/admin.julius")
        toWidget $(luciusFile "templates/admin.lucius")
        $(whamletFile "templates/header.hamlet")
        toWidget $[whamlet|
                <main>
                    <table>
                        <thead>
                            <tr>
                                <th>
                                    Registros
                                <th>
                        <tbody>
                            $forall (E.Value registroid, E.Value nome, E.Value datahora) <- registros
                                <tr>
                                 <li class="divider"></li>
                                    <td>
                                        <a href=@{HomeR}>
                                            <h3>
                                                #{nome}
                                            #{formatePraMim $ datahora}
        |]
        $(whamletFile "templates/footer.hamlet")

formatePraMim :: UTCTime -> String
formatePraMim = show
--formatePraMim a = iso8601DateFormat $ Just (show a)
