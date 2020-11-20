{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import
import Database.Persist.Postgresql
import Text.Lucius

getHomeR :: Handler Html
getHomeR = defaultLayout $ do 
    addStylesheet (StaticR css_bootstrap_css)
    sess <- lookupSession "_EMAIL"
    toWidgetHead [lucius|
            h1 {
                color : red;
            }
            #menu ul {
                padding:0px;
                margin:0px;
                background-color:#EDEDED;
                list-style:none;
            }

            #menu ul li { display: inline; }

            #menu ul li a {
                padding: 2px 10px;
                display: inline-block;

                
                background-color:#EDEDED;
                color: #333;
                text-decoration: none;
                border-bottom:3px solid #EDEDED;
            }

            body{
                background-color: RGB(80,80,80);
            }
        |]
    [whamlet|
        <nav id="menu">
            <h1>
                LIVRARIA FATECANA
            
            <ul>
                <li> 
                    <a href=@{ProdutoR}>
                        CADASTRO DE LIVROS

                <li>
                    <a href=@{ListProdR}>
                        LISTAR LIVROS

                <li>
                    <a href=@{UsuarioR}>
                        CADASTRO DE USUARIOS

            
                $maybe email <- sess
                    <li>
                        <div>
                            #{email}
                            <form method=post action=@{SairR}>
                                <input type="submit" value="Sair">
                $nothing
                    <li>
                        <a href=@{EntrarR}>
                            LOGIN

        <img src=@{StaticR img_produto_jpg}>


    |]
