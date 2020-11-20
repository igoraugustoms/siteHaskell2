{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Logado where

import Import
import Database.Persist.Postgresql

getLogadoR :: Handler Html
getLogadoR = defaultLayout $ do 
    addStylesheet (StaticR css_bootstrap_css)
    sess <- lookupSession "_EMAIL"
    [whamlet|

        <h1>
            LIVRARIA FATECANA

        <img src=@{StaticR img_produto_jpg}>
        
        <ul>
            <li> 
                <a href=@{EntrarR}>
                    LOGIN
            <li> 
                <a href=@{UsuarioR}>
                    CADASTRO DE USUARIO


    |]
