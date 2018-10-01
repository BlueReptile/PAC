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
                $ E.from $ \(registro `E.InnerJoin` sala `E.InnerJoin` pessoa) -> do
                    E.on $ registro ^. RegistroPessoa E.==. pessoa ^. PessoaId
                    E.on $ registro ^. RegistroSala E.==. sala ^. SalaId
                    return
                        ( registro ^. RegistroId
                        , sala  ^. SalaNome
                        , registro ^. RegistroDatahora
                        , registro ^. RegistroEntrada
                        , registro ^. RegistroAlert
                        , pessoa ^. PessoaNome
                        )


    --selectList [] [Asc RegistroDatahora]
    defaultLayout $ do
        setTitle "ⓅⒶⒸ - Registro"
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/admin.julius")
        toWidget $(luciusFile "templates/admin.lucius")
        $(whamletFile "templates/header.hamlet")
        toWidget $[whamlet|
        <div class="indigo z-depth-3" style="text-shadow: 1px 1px gray; padding: 10px">
         <div class="col s12">
            <a href="@{HomeR}" class="breadcrumb"><u>Home</u>
            <a class="breadcrumb">Registros
        <main>
                    <ul class="collection">
                             $forall (E.Value registroid, E.Value nomesala, E.Value datahora, E.Value direcao, E.Value alert, E.Value nomepessoa) <- registros
                              $if (alert /= True) && (direcao == True)
                               <li class="collection-item avatar indigo darken-3 valign-wrapper z-depth-3">
                                  <i class="material-icons teal circle fa fa-sign-in z-depth-3">
                                <span class="title white-text sombra">#{nomesala}
                                  <p class="white-text">
                                    #{nomepessoa}
                                    #{formatePraMim $ datahora}
                              $if (alert /= True) && (direcao == False)
                               <li class="collection-item avatar light-blue darken-3 valign-wrapper z-depth-3">
                                  <i class="material-icons red circle fa fa-sign-out z-depth-3">
                                <span class="title white-text sombra">#{nomesala}
                                  <p class="white-text">
                                    #{nomepessoa}
                                    #{formatePraMim $ datahora}
                              $if (alert == True)
                               <li class="collection-item avatar red darken-4 valign-wrapper z-depth-3">
                                  <i class="material-icons red circle z-depth-3">do_not_disturb
                                <span class="title white-text sombra">#{nomesala}
                                  <p class="white-text">
                                    #{nomepessoa}
                                    #{formatePraMim $ datahora}
        |]
        $(whamletFile "templates/footer.hamlet")

formatePraMim :: UTCTime -> String
formatePraMim a = Prelude.unwords $ Prelude.tail $ Prelude.reverse $ Prelude.words $ show a
