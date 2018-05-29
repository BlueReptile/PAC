{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Admin where

import Import
import Database.Persist.Postgresql

-- areq -> required
-- textField -> campo texto
-- Nothing -> propriedades a mais do campo
-- <$> :: Functor f => (a -> b) -> f a -> f b
-- <*> :: Applicative f => f (a -> b) -> f a -> f b
formAdmin :: Form Admin
formAdmin = renderDivs $ Admin
        <$> areq textField "login: " Nothing
        <*> areq passwordField "pass: " Nothing

getAdminR :: Handler Html
getAdminR = do
    (widget,enctype) <- generateFormPost formAdmin
    defaultLayout $ do
        addStylesheet $ (StaticR css_materialize_css)
        $(whamletFile "templates/admin.hamlet")
        [whamlet|
          <form class="col s18">
            <div class="row">
             <div class="input-field col s16">
              <form method=post action=@{AdminR} enctype=#{enctype}>
                ^{widget}
              <input class="btn waves-effect waves-light light-blue" type="submit" value="Cadastrar">
        |]
        $(whamletFile "templates/footer.hamlet")


postAdminR :: Handler Html
postAdminR = do
    -- LEIO OS PARAMETROS DO FORM
    ((res,_),_) <- runFormPost formAdmin
    case res of
        FormSuccess admin -> do
            aid <- runDB $ insert admin
            defaultLayout [whamlet|
                Admin #{fromSqlKey aid} inserido com sucesso!
            |]
        _ -> redirect HomeR


getADMPerfilR :: AdminId -> Handler Html
getADMPerfilR aid = do
    admin <- runDB $ get404 aid
    defaultLayout $ do
        [whamlet|
            <h1>
                ADMIN #{adminLogin admin}
        |]


postADMPerfilR :: AdminId -> Handler Html
postADMPerfilR aid = do
                     runDB $ delete aid
                     redirect ListaAdminR

getListaAdminR :: Handler Html
getListaAdminR = do
    admins <- runDB $ selectList [] [Asc AdminLogin]
    defaultLayout $ do
        addStylesheet $ (StaticR css_materialize_css)
        $(whamletFile "templates/admins.hamlet")
        [whamlet|
            <table>
                <thead>
                    <tr>
                        <th>
                            Admins
                        <th>
                <tbody>
                    $forall (Entity aid admin) <- admins
                        <tr>
                            <td>
                                <a href=@{ADMPerfilR aid}>
                                    #{adminLogin admin}
                            <td>
                                <form action=@{ADMPerfilR aid} method=post>
                                    <input type="submit" value="Apagar">
        |]
        $(whamletFile "templates/footer.hamlet")
