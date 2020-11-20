{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Login where

import Import
import Tool
import Text.Lucius

formLogin :: Form (Text, Text)
formLogin = renderBootstrap $ (,)
    <$>  areq emailField "E-mail: " Nothing
    <*>  areq passwordField "Senha: " Nothing
    
getEntrarR :: Handler Html
getEntrarR = do 
    (widget,_) <- generateFormPost formLogin
    msg <- getMessage
    defaultLayout $ do 
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
                    width: 1200px;
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
                h1 {
                    color : red;
                }
                
                body{
                    background-color: RGB(169,169,169);
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
                                <li>
                                    <a href=@{ListCompraR}>
                                        Minhas compras

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


        |]
        geraForm EntrarR "ENTRAR" "Login" msg widget

postEntrarR :: Handler Html
postEntrarR = do 
    ((result,_),_) <- runFormPost formLogin
    case result of 
        FormSuccess ("admin@admin.com","root") -> do
            setSession "_EMAIL" "admin@admin.com"
            redirect AdminR
        FormSuccess (email,senha) -> do 
           -- select * from usuario where email=digitado.email
           usuario <- runDB $ getBy (UniqueEmail email)
           case usuario of 
                Nothing -> do 
                    setMessage [shamlet|
                        <div>
                            E-mail NAO ENCONTRADO!
                    |]
                    redirect EntrarR
                Just (Entity _ usu) -> do 
                    if (usuarioSenha usu == senha) then do
                        setSession "_EMAIL" (usuarioEmail usu)
                        redirect HomeR
                    else do 
                        setMessage [shamlet|
                            <div>
                                Senha INCORRETA!
                        |]
                        redirect EntrarR 
        _ -> redirect HomeR
        
postSairR :: Handler Html 
postSairR = do 
    deleteSession "_EMAIL"
    redirect HomeR

getAdminR :: Handler Html
getAdminR = defaultLayout $ do
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
            width: 800px;
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
        h1 {
            color : red;
        }
                
        body{
            background-color: RGB(169,169,169);
        }
    |]
    [whamlet|
        <body>
            <header>
                <div class="caixa">
                    <h1>
                        Bem-vindo, admin da Livraria Fatecana!

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
                                    <div>
                                        #{email}
                                        <form method=post action=@{SairR}>
                                            <input type="submit" value="Sair">
                            $nothing
                                <li>
                                    <a href=@{EntrarR}>
                                        LOGIN
    |]
