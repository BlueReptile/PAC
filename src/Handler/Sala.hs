{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Sala where

import Import
import Database.Persist.Postgresql
import Text.Lucius
import Text.Julius
import Prelude
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))

-- areq -> required
-- textField -> campo texto
-- Nothing -> propriedades a mais do campo
-- <$> :: Functor f => (a -> b) -> f a -> f b
-- <*> :: Applicative f => f (a -> b) -> f a -> f b

getSalaR :: Handler Html
getSalaR = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
                (Just id) -> do
                    return id
                _ -> do
                    redirect LoginPageR
    arduinos <- runDB $ selectList [] [Asc ArduinoName]
    areas <- runDB $ selectList [] [Asc AreaOrdem]
    defaultLayout $ do
        setTitle "ⓅⒶⒸ - Sala"
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/admin.julius")
        toWidget $(luciusFile "templates/admin.lucius")
        $(whamletFile "templates/header.hamlet")
        [whamlet|
        <div class="indigo z-depth-3" style="text-shadow: 1px 1px gray; padding: 10px">
          <div class="col s12">
            <a href="@{HomeR}" class="breadcrumb"><u>Home</u>
            <a href="@{ListaSalaR}" class="breadcrumb"><u>Salas</u>
            <a class="breadcrumb">Cadastro
        <main>
         <div class="row">
          <div class="col s6 offset-s3 valign">
            <div class="card blue-grey darken-1">
              <div class="card-content white-text">
                <span class="card-title">Cadastro de Sala</span>
                  <form action=@{SalaR} method=post>
                   <div class="input-field">
                     <label class="active white-text" for="sala_nome">Nome da Sala</label>
                     <input value="" name="sala_nome" id="sala_nome" type="text" class="validate">
                     <div class="input-field">
                       <label class="active white-text" for="nivel">Nivel da Sala</label>
                       <input value="" name="nivel" id="nivel" type="number" class="validate">

                    <br>
                     <div class="input-field">
                      <select name="id" >
                        <option value="" disabled selected>Qual Arduino?</option>
                        $forall (Entity arid arduino) <- arduinos
                          <option value="#{fromSqlKey $ arid}">#{arduinoName arduino}</option>
                      <label class="white-text">Arduino</label>


                     <div class="input-field">
                       <select id="areaDesignada" name="areaDesignada">
                         <option value="" disabled selected>Qual Area?</option>
                         $forall (Entity areaid restoarea) <- areas
                           <option value="#{fromSqlKey $ areaid}">#{areaNome restoarea}</option>
                       <label class="white-text">Area designada</label>

                    <button class="btn waves-effect waves-light" type="submit" name="action">Cadastrar
                      <i class="material-icons right">send</i>
        |]
        $(whamletFile "templates/footer.hamlet")


postSalaR :: Handler Html
postSalaR = do
        maybeId <- lookupSession "ID"
        idText <- case maybeId of
            (Just id) -> do
                return id
            _ -> do
                redirect LoginPageR
        nome <- runInputPost $ ireq textField "sala_nome"
        id <- runInputPost $ ireq intField "id"
        area <- runInputPost $ ireq intField "areaDesignada"
        nivel <- runInputPost $ ireq intField "nivel"
        let posx = "100px"
        let posy = "100px"
        sid <- runDB $ insert $ Sala nome (toSqlKey id) (toSqlKey area) posx posy nivel
        defaultLayout $ do
                setTitle "ⓅⒶⒸ - Sala"
                addStylesheet $ (StaticR css_materialize_css)
                addScript $ (StaticR js_jquery_js)
                addScript $ (StaticR js_materialize_js)
                toWidget $(juliusFile "templates/admin.julius")
                toWidget $(luciusFile "templates/admin.lucius")
                $(whamletFile "templates/header.hamlet")
                [whamlet|
                 <div class="indigo z-depth-3" style="text-shadow: 1px 1px gray; padding: 10px">
                  <div class="col s12">
                    <a href="@{HomeR}" class="breadcrumb"><u>Home</u>
                    <a href="@{ListaSalaR}" class="breadcrumb"><u>Salas</u>
                    <a class="breadcrumb">Cadastro
                    <main>
                       <div class="row">
                         <div class="col s3 m3">
                           <img class="center" src=@{StaticR haskellchan_render_png} alt="Haskell-chan" style="height:60vh;width:auto;">
                         <div class="col s9 m9 center">
                           <div class="card speech-bubble">
                             <div class="card-content white-text sombra1">
                               <span class="card-title sombra2">
                                 <h3>Operação Concluída!
                               <hr class="teal-text">
                               <p>
                                 Sala #{nome} inserida com sucesso no sistema!
                |]
                $(whamletFile "templates/footer.hamlet")


getSalaPerfilR :: SalaId -> Handler Html
getSalaPerfilR sid = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
            (Just id) -> do
                return id
            _ -> do
                redirect LoginPageR

    salainfo <- runDB
                $ E.select
                $ E.from $ \(sala `E.InnerJoin` arduino `E.InnerJoin` area) -> do
                    E.on $ sala ^.  SalaArea E.==. area ^. AreaId
                    E.on $ sala ^. SalaArid E.==. arduino ^. ArduinoId
                    return
                        ( sala ^. SalaId
                        , sala  ^. SalaNome
                        , arduino ^. ArduinoName
                        , area ^. AreaNome
                        )
    defaultLayout $ do
        setTitle "ⓅⒶⒸ - Sala"
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/admin.julius")
        toWidget $(luciusFile "templates/admin.lucius")
        $(whamletFile "templates/header.hamlet")
        [whamlet|
         <div class="indigo z-depth-3" style="text-shadow: 1px 1px gray; padding: 10px">
          <div class="col s12">
           <a href="@{HomeR}" class="breadcrumb"><u>Home</u>
           <a href="@{ListaSalaR}" class="breadcrumb"><u>Salas</u>
           $forall (E.Value idsala, E.Value sala, E.Value arduino, E.Value area) <- salainfo
             $if idsala == sid
               <a class="breadcrumb">#{sala}
         <main>
          <br>
           <br>
            <div class="row">
              <div class="col s6 offset-s3 valign">
               <div class="card blue-grey darken-1">
                <div class="card-content white-text">
                 <span class="card-title">SALA</span>
                 <br>
                 $forall (E.Value idsala, E.Value sala, E.Value arduino, E.Value area) <- salainfo
                   $if idsala == sid
                     <p> Nome da Sala : #{sala}
                     <p> Area da Sala : #{area}
                     <p> Arduino da Sala : #{arduino}
                <br>
                <div class="card-action">
                  <form action=@{EditSalaR}  method=post>
                   <input type="hidden" id="sid" name="sid" value=#{fromSqlKey sid}>
                   <button class="btn waves-effect waves-light" type="submit" name="action">Editar
                     <i class="material-icons right">send</i>
        |]
        $(whamletFile "templates/footer.hamlet")



postEditSalaR :: Handler Html
postEditSalaR = do
     maybeId <- lookupSession "ID"
     idText <- case maybeId of
            (Just id) -> do
                return id
            _ -> do
                redirect LoginPageR
     sid <- runInputPost $ ireq hiddenField "sid"
     sala <- runDB $ selectList [SalaId Database.Persist.Postgresql.==. sid] []
     arduinos <- runDB $ selectList [] [Asc ArduinoName]
     areas <- runDB $ selectList [] [Asc AreaOrdem]
     defaultLayout $ do
        setTitle "ⓅⒶⒸ - Sala"
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/admin.julius")
        toWidget $(luciusFile "templates/admin.lucius")
        $(whamletFile "templates/header.hamlet")
        [whamlet|
         <div class="indigo z-depth-3" style="text-shadow: 1px 1px gray; padding: 10px">
           <div class="col s12">
             <a href="@{HomeR}" class="breadcrumb"><u>Home</u>
             <a href="@{ListaSalaR}" class="breadcrumb"><u>Salas</u>
             <a class="breadcrumb">Editar
         <main>
          <br>
           <br>
            <div class="row">
              <div class="col s6 offset-s3 valign">
               <form action=@{AltSalaR}  method=post>
                <div class="card blue-grey darken-1">
                 <div class="card-content white-text">
                  <span class="card-title">SALA </span>
                  <br>
                  <p> Editar Sala
                  <br>
                  <div class="input-field">
                     <label class="active white-text" for="sala_nome">Nome da Sala</label>
                     <input value="" name="sala_nome" id="sala_nome" type="text" class="validate">
                  <div class="input-field">
                     <label class="active white-text" for="nivel">Nivel de Acesso</label>
                     <input value="" name="nivel" id="nivel" type="number" class="validate">
                  <label>Arduino</label>
                    <br>
                      <select name="id" >
                        <option value="" disabled selected>Qual Arduino?</option>
                        $forall (Entity arid arduino) <- arduinos
                          <option value="#{fromSqlKey $ arid}">#{arduinoName arduino}</option>
                  <label>Area designada</label>
                       <br>
                        <select id="areaDesignada" name="areaDesignada">
                          <option value="" disabled selected>Qual Area?</option>
                           $forall (Entity areaid restoarea) <- areas
                            <option value="#{fromSqlKey $ areaid}">#{areaNome restoarea}</option>
                 <br>
                 <div class="card-action">
                   <input type="hidden" id="sid" name="sid" value=#{fromSqlKey sid}>
                   <button class="btn waves-effect waves-light" type="submit" name="action">Editar
                     <i class="material-icons right">send</i>
        |]
        $(whamletFile "templates/footer.hamlet")

postAltSalaR :: Handler Html
postAltSalaR = do
   maybeId <- lookupSession "ID"
   idText <- case maybeId of
                (Just id) -> do
                    return id
                _ -> do
                    redirect LoginPageR
   nome <- runInputPost $ ireq textField "sala_nome"
   arid <- runInputPost $ ireq intField "id"
   area <- runInputPost $ ireq intField "areaDesignada"
   nivel <- runInputPost $ ireq intField "nivel"
   sid <- runInputPost $ ireq hiddenField "sid"
   --sala <- runDB $ selectList [SalaId ==. sid] []
   runDB $ Database.Persist.Postgresql.update sid [SalaNome Database.Persist.Postgresql.=. nome]
   runDB $ Database.Persist.Postgresql.update sid [SalaNivel Database.Persist.Postgresql.=. nivel]
   runDB $ Database.Persist.Postgresql.update sid [SalaArid Database.Persist.Postgresql.=. toSqlKey arid]
   runDB $ Database.Persist.Postgresql.update sid [SalaArea Database.Persist.Postgresql.=. toSqlKey area]
   defaultLayout $ do
           setTitle "ⓅⒶⒸ - Sala"
           addStylesheet $ (StaticR css_materialize_css)
           addScript $ (StaticR js_jquery_js)
           addScript $ (StaticR js_materialize_js)
           toWidget $(juliusFile "templates/admin.julius")
           toWidget $(luciusFile "templates/admin.lucius")
           $(whamletFile "templates/header.hamlet")
           [whamlet|
            <div class="indigo z-depth-3" style="text-shadow: 1px 1px gray; padding: 10px">
              <div class="col s12">
                <a href="@{HomeR}" class="breadcrumb"><u>Home</u>
                <a href="@{ListaSalaR}" class="breadcrumb"><u>Salas</u>
                <a class="breadcrumb">Editar
            <main>
                   <div class="row">
                     <div class="col s3 m3">
                       <img class="center" src=@{StaticR haskellchan_render_png} alt="Haskell-chan" style="height:60vh;width:auto;">
                     <div class="col s9 m9 center">
                       <div class="card speech-bubble">
                         <div class="card-content white-text sombra1">
                           <span class="card-title sombra2">
                             <h3>Operação Concluída!
                           <hr class="teal-text">
                           <p>
                             Sala alterada com sucesso!
           |]
           $(whamletFile "templates/footer.hamlet")

postSalaPerfilR :: SalaId -> Handler Html
postSalaPerfilR sid = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
            (Just id) -> do
                return id
            _ -> do
                redirect LoginPageR
    runDB $ Database.Persist.Postgresql.delete sid
    redirect ListaSalaR

getListaSalaR :: Handler Html
getListaSalaR = do
    maybeId <- lookupSession "ID"
    idText <- case maybeId of
            (Just id) -> do
                return id
            _ -> do
                redirect LoginPageR
    salas <- runDB $ selectList [] [Asc SalaNome]
    defaultLayout $ do
        setTitle "ⓅⒶⒸ - Sala"
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/admin.julius")
        toWidget $(luciusFile "templates/admin.lucius")
        $(whamletFile "templates/header.hamlet")
        [whamlet|
        <div class="indigo z-depth-3" style="text-shadow: 1px 1px gray; padding: 10px">
         <div class="col s12">
            <a href="@{HomeR}" class="breadcrumb"><u>Home</u>
            <a href="@{ListaSalaR}" class="breadcrumb"><u>Sala</u>
            <a class="breadcrumb">Lista de Salas
        <main>
         <ul class="collection">
            $forall (Entity sid sala) <- salas
             <li class="collection-item">
               <form action=@{SalaPerfilR sid} method=post class="right">
                 <input class="btn waves-effect waves-light" type="submit" value="Apagar">
              <a href=@{SalaPerfilR sid}><h5>#{salaNome sala}<h5>
        |]
        $(whamletFile "templates/footer.hamlet")
