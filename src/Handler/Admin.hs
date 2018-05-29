{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Admin where

import Import
import Database.Persist.Postgresql
import Text.Julius
import Text.Lucius


-- areq -> required
-- textField -> campo texto
-- Nothing -> propriedades a mais do campo
-- <$> :: Functor f => (a -> b) -> f a -> f b
-- <*> :: Applicative f => f (a -> b) -> f a -> f b
formAdmin :: Form Admin
formAdmin = renderDivs $ Admin
        <$> areq textField "login: " Nothing
        <*> areq textField "pass: " Nothing

getAdminR :: Handler Html
getAdminR = do
    (widget,enctype) <- generateFormPost formAdmin
    defaultLayout $ do
        addStylesheet $ (StaticR css_materialize_css)
        $(whamletFile "templates/admin.hamlet")
        [whamlet|
         <div class="row">
          <form class="col s12">
            <div class="row">
             <div class="input-field col s6">
              <form action=@{AdminR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="OK">
        |]
        $(whamletFile "templates/footer.hamlet")



postAdminR :: Handler Html
postAdminR = do
    -- LEIO OS PARAMETROS DO FORM
    ((res,_),_) <- runFormPost formAdmin
    case res of
        FormSuccess admin -> do
            tid <- runDB $ insert admin
            defaultLayout [whamlet|
                admin #{fromSqlKey tid} inserido com sucesso!
            |]
        _ -> redirect HomeR

getADMPerfilR :: AdminId -> Handler Html
getADMPerfilR tid = do
    admin <- runDB $ get404 tid
    defaultLayout $ do
        [whamlet|
            <h1>
                ADMIN #{adminLogin admin}
        |]


postADMPerfilR :: AdminId -> Handler Html
postADMPerfilR tid = do
                     runDB $ delete tid
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
                            Login
                        <th>
                <tbody>
                    $forall (Entity tid admin) <- admins
                        <tr>
                            <td>
                                <a href=@{ADMPerfilR tid}>
                                    #{adminLogin admin}
                            <td>
                                <form action=@{ADMPerfilR tid} method=post>
                                    <input type="submit" value="Apagar">
        |]
        $(whamletFile "templates/footer.hamlet")
