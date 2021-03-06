{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Import.NoFoundation
import Data.Time.Format
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Yesod.Core.Types     (Logger)
import Text.Lucius
import Text.Julius
import Web.HttpApiData
import Data.Aeson (decodeStrict, encode)

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static
    , appConnPool    :: ConnectionPool
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

instance Yesod App where
    makeLogger = return . appLogger
    errorHandler NotFound = do
      maybeId <- lookupSession "ID"
      idText <- case maybeId of
          (Just id) -> do
              return id
          _ -> do
              return ""
      fmap toTypedContent $ defaultLayout $ do
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/admin.julius")
        toWidget $(luciusFile "templates/admin.lucius")
        $(widgetFile "header")
        $(widgetFile "error/404")
        $(widgetFile "footer")
    errorHandler (InvalidArgs makeLogger) = do
      maybeId <- lookupSession "ID"
      idText <- case maybeId of
          (Just id) -> do
              return id
          _ -> do
              return ""
      fmap toTypedContent $ defaultLayout $ do
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/admin.julius")
        toWidget $(luciusFile "templates/admin.lucius")
        $(widgetFile "header")
        $(widgetFile "error/sql")
        $(widgetFile "footer")
    errorHandler (InternalError makeLogger) = do
      maybeId <- lookupSession "ID"
      idText <- case maybeId of
          (Just id) -> do
              return id
          _ -> do
              return ""
      fmap toTypedContent $ defaultLayout $ do
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/admin.julius")
        toWidget $(luciusFile "templates/admin.lucius")
        $(widgetFile "header")
        $(widgetFile "error/internalerror")
        $(widgetFile "footer")
    errorHandler (PermissionDenied makeLogger)= do
      maybeId <- lookupSession "ID"
      idText <- case maybeId of
          (Just id) -> do
              return id
          _ -> do
              return ""
      fmap toTypedContent $ defaultLayout $ do
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/admin.julius")
        toWidget $(luciusFile "templates/admin.lucius")
        $(widgetFile "header")
        $(widgetFile "error/denied")
        $(widgetFile "footer")
    errorHandler (BadMethod makeLogger)= do
      maybeId <- lookupSession "ID"
      idText <- case maybeId of
          (Just id) -> do
              return id
          _ -> do
              return ""
      fmap toTypedContent $ defaultLayout $ do
        addStylesheet $ (StaticR css_materialize_css)
        addScript $ (StaticR js_jquery_js)
        addScript $ (StaticR js_materialize_js)
        toWidget $(juliusFile "templates/admin.julius")
        toWidget $(luciusFile "templates/admin.lucius")
        $(widgetFile "header")
        $(widgetFile "error/bad")
        $(widgetFile "footer")
    errorHandler other = defaultErrorHandler other


type Form a =
        Html -> MForm Handler (FormResult a, Widget)

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage



instance HasHttpManager App where
    getHttpManager = appHttpManager


instance PathPiece UTCTime where
  fromPathPiece = decodeStrict . encodeUtf8
  toPathPiece   = toStrict . decodeUtf8 . encode
