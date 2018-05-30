{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Admin where

import Import
import Database.Persist.Postgresql
import Text.Lucius
import Text.Julius

-- areq -> required
-- textField -> campo texto
-- Nothing -> propriedades a mais do campo
-- <$> :: Functor f => (a -> b) -> f a -> f b
-- <*> :: Applicative f => f (a -> b) -> f a -> f b
formAdmin :: Form Admin
formAdmin = renderDivs $ Admin
        <$> areq textField "Usuario: " Nothing
        <*> areq passwordField "Senha: " Nothing

getAdminR :: Handler Html
getAdminR = do
    (widget,enctype) <- generateFormPost formAdmin
    defaultLayout $ do
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/admin.julius")
        toWidget $(luciusFile "templates/admin.lucius")
        $(whamletFile "templates/header.hamlet")
        [whamlet|
         <main>
            <div class="row">
              <form class="col s4" form method=post action=@{AdminR} enctype=#{enctype}>
                    ^{widget}
                    <button class="btn waves-effect waves-light" type="submit" name="action">Cadastrar
                        <i class="material-icons right">send</i>
        |]
        $(whamletFile "templates/footer.hamlet")


postAdminR :: Handler Html
postAdminR = do
    -- LEIO OS PARAMETROS DO FORM
    ((res,_),_) <- runFormPost formAdmin
    case res of
        FormSuccess admin -> do
            aid <- runDB $ insert admin
            defaultLayout $ do
                addStylesheet $ (StaticR css_materialize_css)
                addScript $ (StaticR js_jquery_js)
                addScript $ (StaticR js_materialize_js)
                toWidget $(juliusFile "templates/admin.julius")
                toWidget $(luciusFile "templates/admin.lucius")
                $(whamletFile "templates/header.hamlet")
                [whamlet|
                 <main>
                    Admin #{fromSqlKey aid} inserido com sucesso!
                |]
                $(whamletFile "templates/footer.hamlet")
        _ -> redirect HomeR


getADMPerfilR :: AdminId -> Handler Html
getADMPerfilR aid = do
    admin <- runDB $ get404 aid
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
                ADMIN #{adminLogin admin}
        |]
        $(whamletFile "templates/footer.hamlet")


postADMPerfilR :: AdminId -> Handler Html
postADMPerfilR aid = do
                     runDB $ delete aid
                     redirect ListaAdminR

getListaAdminR :: Handler Html
getListaAdminR = do
    admins <- runDB $ selectList [] [Asc AdminLogin]
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
                                    Admins
                                <th>
                        <tbody>
                            $forall (Entity aid admin) <- admins
                                <tr>
                                 <li class="divider"></li>
                                    <td>
                                        <a href=@{ADMPerfilR aid}>
                                            #{adminLogin admin}
                                    <td>
                                        <form action=@{ADMPerfilR aid} method=post>
                                            <input class="btn waves-effect waves-light" type="submit" value="Apagar">
        |]
        $(whamletFile "templates/footer.hamlet")
