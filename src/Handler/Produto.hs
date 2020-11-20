{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Produto where

import Import
import Tool
--import Database.Persist.Postgresql

-- (<$>) = fmap :: Functor f => (a -> b) -> f a -> f b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
formProduto :: Maybe Produto -> Form Produto
formProduto prod = renderDivs $ Produto  
    <$> areq textField (FieldSettings "Nome: " 
                                      Nothing
                                      (Just "hs12")
                                      Nothing
                                      [("class","myClass")]
                       ) (fmap produtoNome prod)
    <*> areq textField "Autor: " (fmap produtoAutor prod)
    <*> areq doubleField "Preco: " (fmap produtoPreco prod)

auxProdutoR :: Route App -> Maybe Produto -> Handler Html
auxProdutoR rt produto = do
    (widget,_) <- generateFormPost (formProduto produto)
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
            <h1>
                 CADASTRO DE LIVRO
            
            <form action=@{rt} method=post>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]
    
getProdutoR :: Handler Html
getProdutoR = auxProdutoR ProdutoR Nothing
    
postProdutoR :: Handler Html
postProdutoR = do
    ((resp,_),_) <- runFormPost (formProduto Nothing)
    case resp of 
         FormSuccess produto -> do 
             pid <- runDB $ insert produto
             redirect (DescR pid)
         _ -> redirect HomeR
    
-- SELECT * from produto where id = pid 
getDescR :: ProdutoId -> Handler Html
getDescR pid = do 
    produto <- runDB $ get404 pid
    (widget,_) <- generateFormPost formQt
    sess <- lookupSession "_EMAIL"
      
    defaultLayout [whamlet|
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
                                        <div>
                                            #{email}
                                            <form method=post action=@{SairR}>
                                                <input type="submit" value="Sair">
                                $nothing
                                    <li>
                                        <a href=@{EntrarR}>
                                            LOGIN
            <h2>
                Nome: #{produtoNome produto}
            
            <h2>
                Preco: #{produtoAutor produto}

            <h2>
                Preco: #{produtoPreco produto}
            
            <form action=@{ComprarR pid} method=post>
                ^{widget}
                <input type="submit" value="Comprar">
    |]

getListProdR :: Handler Html
getListProdR = do 
    -- produtos :: [Entity Produto]
    produtos <- runDB $ selectList [] [Desc ProdutoPreco]
    sess <- lookupSession "_EMAIL"
    defaultLayout [whamlet|
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
                                    <div>
                                        #{email}
                                        <form method=post action=@{SairR}>
                                            <input type="submit" value="Sair">
                            $nothing
                                <li>
                                    <a href=@{EntrarR}>
                                        LOGIN
            <table>
                <thead>
                    <tr>
                        <th> 
                            Livro
                        
                        <th>
                            Autor

                        <th>
                            Valor
                        
                        <th>
                        
                        <th>
                <tbody>
                    $forall Entity pid prod <- produtos
                        <tr>
                            <td>
                                <a href=@{DescR pid}>
                                    #{produtoNome prod}
                            
                            <td>
                                #{produtoAutor prod}
                            
                            <td>
                                #{produtoPreco prod}

                            <th>
                                <a href=@{UpdProdR pid}>
                                    Editar
                            <th>
                                <form action=@{DelProdR pid} method=post>
                                    <input type="submit" value="X">
    |]

getUpdProdR :: ProdutoId -> Handler Html
getUpdProdR pid = do 
    antigo <- runDB $ get404 pid
    auxProdutoR (UpdProdR pid) (Just antigo)    
    
-- UPDATE produto WHERE id = pid SET ...
postUpdProdR :: ProdutoId -> Handler Html
postUpdProdR pid = do
    ((resp,_),_) <- runFormPost (formProduto Nothing)
    case resp of 
         FormSuccess novo -> do
            runDB $ replace pid novo
            redirect (DescR pid) 
         _ -> redirect HomeR

postDelProdR :: ProdutoId -> Handler Html
postDelProdR pid = do 
    _ <- runDB $ get404 pid 
    runDB $ delete pid 
    redirect ListProdR



