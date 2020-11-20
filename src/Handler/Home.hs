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
            header {
                background: #BBBBBB;
                padding: 20px 0;
            }

            h1 {
                color : red;
            }

            .caixa {
                position: relative;
                width: 940px;
                margin: 0 auto;
            }

            nav {
                position: absolute;
                top: 110px;
                right: 0;
            }

            nav li {
                display: inline;
                margin: 0 0 0 15px;
            }

            nav a {
                text-transform: uppercase;
                color: #000000;
                font-weight: bold;
                font-size: 22px;
                text-decoration: none;
            }

            nav a:hover {
                color: #C78C19;
                text-decoration: underline;
            }

            body{
                background-color: RGB(80,80,80);
            }

            .nome{
                position: absolute;
                left: 800px;
                top: 100px;
                color: red;
            }

            .imagem{
                position: absolute;
                left: 100px;
                top: 230px;
            }
        |]
    [whamlet|
        <body>
            <header>
                <div class="caixa">
                    <h1>
                        Livraria Fatecana

                    <nav>
                        <ul>
                            <li>
                                <a href=@{ProdutoR}>
                                    Cadastro de livros
                            <li>
                                <a href=@{ListProdR}>
                                    Listar livros
                            <li>
                                <a href=@{UsuarioR}>
                                    Cadastro de usuarios
                            $maybe email <- sess
                                <li>
                                    <div class="nome">
                                        #{email}
                                        <form method=post action=@{SairR}>
                                            <input type="submit" value="Sair da sessão">
                            $nothing
                                <li>
                                    <a href=@{EntrarR}>
                                        LOGIN

            <body>
                <div class="imagem">
                    <img src=@{StaticR img_produto_jpg}>


    |]
