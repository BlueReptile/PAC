{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Pessoa where

import Import
import Database.Persist.Postgresql
import Text.Lucius
import Text.Julius
import Prelude
import Database.Esqueleto
import Network.HTTP.Simple


-- areq -> required
-- textField -> campo texto
-- Nothing -> propriedades a mais do campo
-- <$> :: Functor f => (a -> b) -> f a -> f b
-- <*> :: Applicative f => f (a -> b) -> f a -> f b

getPessoaR :: Handler Html
getPessoaR = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
                (Just id) -> do
                    return id
                _ -> do
                    redirect LoginPageR
    arduinos <- runDB $ selectList [] [Asc ArduinoName]
    cardid <- httpLBS "http://187.21.121.25:8081/card"
    defaultLayout $ do
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/pessoa.julius")
        toWidget $(luciusFile "templates/admin.lucius")
        $(whamletFile "templates/header.hamlet")
        [whamlet|
        <br>
        <main>
         <div class="row">
          <div class="col s6 offset-s3 valign">
            <div class="card blue-grey darken-1">
              <div class="card-content white-text">
                <span class="card-title">Cadastro de Pessoa</span>
                  <form action=@{PessoaR} id="pessoaForm" name="pessoaForm" method=get>
                     <label class="active white-text" for="pessoa_nome">Nome da Pessoa</label>
                     <input value="" name="pessoa_nome" id="pessoa_nome" type="text" class="validate">
                     <label class="active white-text" for="pessoa_cpf">CPF</label>
                     <input value="" name="pessoa_cpf" id="pessoa_cpf" type="text" class="validate" onkeyup="">
                     <label>Arduino para Escanear o Cartão</label>
                     <select id="arduinoIp" name="arduinoIp">
                        <option value="" disabled selected>Qual Arduino?</option>
                        $forall (Entity arid arduino) <- arduinos
                          <option value="#{arduinoIp arduino}">#{arduinoName arduino}</option>
                      <label class="white-text" for="card">Cartão da pessoa</label>
                      <input disabled class="white-text" value="cartaoID" id="card" type="text" class="validate">
                      <input class="btn waves-effect waves-light" id="UpdateID" type="button" value="updateID" onclick="updateID();" />
                    <button class="btn waves-effect waves-light" type="submit" name="action">Cadastrar
                      <i class="material-icons right">send</i>
        |]
        $(whamletFile "templates/footer.hamlet")


postPessoaR :: Handler Html
postPessoaR = do
        maybeId <- lookupSession "ID"
        idText <- case maybeId of
            (Just id) -> do
                return id
            _ -> do
                redirect LoginPageR
        nome <- runInputPost $ ireq textField "pessoa_nome"
        ip <- runInputPost $ ireq intField "ip"
        -- sid <- runDB $ insert $ Pessoa nome (toSqlKey id)
        defaultLayout $ do
                addStylesheet $ (StaticR css_materialize_css)
                addScript $ (StaticR js_jquery_js)
                addScript $ (StaticR js_materialize_js)
                toWidget $(juliusFile "templates/admin.julius")
                toWidget $(luciusFile "templates/admin.lucius")
                $(whamletFile "templates/header.hamlet")
                [whamlet|
                 <main>
                    Pessoa #{nome} inserida com sucesso!
                |]
                $(whamletFile "templates/footer.hamlet")


getPessoaPerfilR :: PessoaId -> Handler Html
getPessoaPerfilR sid = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
            (Just id) -> do
                return id
            _ -> do
                redirect LoginPageR
    pessoa <- runDB $ get404 sid
    defaultLayout $ do
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/admin.julius")
        toWidget $(luciusFile "templates/admin.lucius")
        $(whamletFile "templates/header.hamlet")
        [whamlet|
          <main>
            <h1>
                Pessoas #{pessoaNome pessoa}
        |]
        $(whamletFile "templates/footer.hamlet")


postPessoaPerfilR :: PessoaId -> Handler Html
postPessoaPerfilR sid = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
            (Just id) -> do
                return id
            _ -> do
                redirect LoginPageR
    runDB $ Database.Persist.Postgresql.delete sid
    redirect ListaPessoaR

getListaPessoaR :: Handler Html
getListaPessoaR = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
            (Just id) -> do
                return id
            _ -> do
                redirect LoginPageR
    pessoas <- runDB $ selectList [] [Asc PessoaNome]
    defaultLayout $ do
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/admin.julius")
        toWidget $(luciusFile "templates/admin.lucius")
        $(whamletFile "templates/header.hamlet")
        [whamlet|
                <main>
                    <table>
                        <thead>
                            <tr>
                                <th>
                                    Pessoas
                                <th>
                        <tbody>
                            $forall (Entity pid pessoa) <- pessoas
                                <tr>
                                 <li class="divider"></li>
                                    <td>
                                        <a href=@{PessoaPerfilR pid}>
                                            #{pessoaNome pessoa}
                                    <td>
                                        <form action=@{PessoaPerfilR pid} method=post>
                                            <input class="btn waves-effect waves-light" type="submit" value="Apagar">
        |]
        $(whamletFile "templates/footer.hamlet")
