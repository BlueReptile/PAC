{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Login where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Text.Lucius
import Text.Julius

formAdmin :: Form Admin
formAdmin = renderDivs $ Admin
        <$> areq textField "Usuario: " Nothing
        <*> areq passwordField "Senha: " Nothing

getLoginPageR :: Handler Html
getLoginPageR = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
        (Just id) -> do
            return id
        _ -> do
            return ""
    (widget,enctype) <- generateFormPost formAdmin
    defaultLayout $ do
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/admin.julius")
        toWidget $(luciusFile "templates/admin.lucius")
        $(whamletFile "templates/header.hamlet")
        [whamlet|
        <br>
        <main>
         <div class="row">
          <div class="col s6 offset-s3 valign">
            <div class="card light-blue darken-4">
              <div class="card-content white-text">
                <span class="card-title">Login de Admin</span>
                  <form action=@{AdminLoginR} method=post enctype=#{enctype}>
                    ^{widget}
                    <button class="btn waves-effect waves-light" type="submit" name="action">Logar
                      <i class="material-icons right">send</i>
        |]
        $(whamletFile "templates/footer.hamlet")





postAdminLoginR :: Handler Html
postAdminLoginR = do
                 maybeId <- lookupSession "ID"
                 idText <- case maybeId of
                         (Just id) -> do
                             return id
                         _ -> do
                             return ""
                 login <- runInputPost $ ireq textField "f1"
                 pass <- runInputPost $ ireq textField "f2"
                 maybeAdmin <- runDB $ getBy $ UniqueLogin login pass
                 case maybeAdmin of
                             Just user -> do
                                        setSession "ID" $ login
                                        redirect HomeR
                             _ -> defaultLayout $ do
                                 addStylesheet $ (StaticR css_materialize_css)
                                 addScript $ (StaticR js_jquery_js)
                                 addScript $ (StaticR js_materialize_js)
                                 toWidget $(juliusFile "templates/admin.julius")
                                 toWidget $(luciusFile "templates/admin.lucius")
                                 $(whamletFile "templates/header.hamlet")
                                 [whamlet|
                                   <main style="background-color:red;">
                                     <h1>
                                         USUARIO INVALIDO
                                 |]
                                 $(whamletFile "templates/footer.hamlet")

getAdminLogoutR :: Handler Html
getAdminLogoutR = do
             deleteSession "ID"
             redirect HomeR
