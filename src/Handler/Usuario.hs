{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Usuario where

import Import
import Tool
import Text.Lucius

formUsu :: Form (Usuario, Text)
formUsu = renderBootstrap $ (,)
    <$> (Usuario 
        <$> areq textField "Nome: " Nothing
        <*> areq emailField "E-mail: " Nothing
        <*> areq textField "Endereco: " Nothing
        <*> areq passwordField "Senha: " Nothing)
    <*> areq passwordField "Digite Novamente: " Nothing

getUsuarioR :: Handler Html
getUsuarioR = do 
    (widget,_) <- generateFormPost formUsu
    msg <- getMessage
    defaultLayout $ do 
        toWidgetHead $(luciusFile  "templates/form.lucius")
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
        geraForm UsuarioR "CADASTRO DE USUARIO" "Cadastrar" msg widget

postUsuarioR :: Handler Html
postUsuarioR = do 
    ((result,_),_) <- runFormPost formUsu
    case result of 
        FormSuccess (usuario,veri) -> do 
            if (usuarioSenha usuario == veri) then do 
                runDB $ insert400 usuario 
                setMessage [shamlet|
                    <div>
                        USUARIO INCLUIDO
                |]
                redirect UsuarioR
            else do 
                setMessage [shamlet|
                    <div>
                        SENHA E VERIFICACAO N CONFEREM
                |]
                redirect UsuarioR
        _ -> redirect HomeR
    
getListUsuR :: Handler Html
getListUsuR = do 
    -- produtos :: [Entity Produto]
    usuarios <- runDB $ selectList [] []
    sess <- lookupSession "_EMAIL"
    defaultLayout [whamlet|
        <body style="background-color: RGB(169,169,169);">
                <header style="background: #BBBBBB; padding: 20px 0;">
                    <div class="caixa" style="position: relative; width: 940px; margin: 0 auto;">
                        <h1 style="color:red;">
                            Livraria Fatecana

                        <nav style="position: absolute; top: 110px; right: 0;">
                            <ul>
                                <li style="display: inline; margin: 0 0 0 15px;">
                                    <a href=@{ListProdR} style="text-transform: uppercase; color: #000000; font-weight: bold; font-size: 22px; text-decoration: none;">
                                        Listar livros
                                <li style="display: inline; margin: 0 0 0 15px;">
                                    <a href=@{UsuarioR} style="text-transform: uppercase; color: #000000; font-weight: bold; font-size: 22px; text-decoration: none;">
                                        Cadastro de usuarios
                                <li style="display: inline; margin: 0 0 0 15px;">
                                    <a href=@{ListCompraR} style="text-transform: uppercase; color: #000000; font-weight: bold; font-size: 22px; text-decoration: none;">
                                        Minhas compras

                                $maybe email <- sess
                                    <li style="display: inline; margin: 0 0 0 15px;">
                                        <div>
                                            #{email}
                                            <form method=post action=@{SairR}>
                                                <input type="submit" value="Sair">
                                $nothing
                                    <li style="display: inline; margin: 0 0 0 15px;">
                                        <a href=@{EntrarR} style="text-transform: uppercase; color: #000000; font-weight: bold; font-size: 22px; text-decoration: none;">
                                            LOGIN
            <h2 style="color: red; font-size: 20px">
                Lista de usuarios 
            <table>
                <thead>
                    <tr>
                        <th> 
                            Usuario
                        
                        <th>
                            Email

                        <th>
                            Endereco
                <tbody>
                    $forall Entity uid usu <- usuarios
                        <tr>
                            <td>
                                #{usuarioNome usu}
                            
                            <td>
                                #{usuarioEmail usu}
                            
                            <td>
                                #{usuarioEnd usu}

                            <th>
                                <form action=@{DelUsuR uid} method=post>
                                    <input type="submit" value="X">
    |]

postDelUsuR :: UsuarioId -> Handler Html
postDelUsuR uid = do 
    _ <- runDB $ get404 uid 
    runDB $ delete uid 
    redirect ListUsuR
