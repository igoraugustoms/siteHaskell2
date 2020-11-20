{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Compra where

import Import
import Tool
import Database.Persist.Sql

getListCompraR :: Handler Html
getListCompraR = do
    sess <- lookupSession "_EMAIL"
    case sess of 
        Nothing -> redirect HomeR
        Just email -> do
            usu <- runDB $ getBy (UniqueEmail email)
            case usu of 
                 Nothing -> redirect HomeR 
                 Just (Entity uid usuario) -> do 
                     let sql = "SELECT ??,??,?? FROM usuario \
                        \ INNER JOIN compra ON compra.usuarioid = usuario.id \
                        \ INNER JOIN produto ON compra.produtoid = produto.id \
                        \ WHERE usuario.id = ?"
                     produtos <- runDB $ rawSql sql [toPersistValue uid] :: Handler [(Entity Usuario,Entity Compra,Entity Produto)]
                     defaultLayout $ do 

                        [whamlet|
                            <body style="background-color: RGB(169,169,169);">
                              <header style="background: #BBBBBB; padding: 20px 0;">
                                  <div class="caixa" style="position: relative; width: 940px; margin: 0 auto;">
                                      <h1 style="color: red">
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
                                                      <a href=@{EntrarR}>
                                                          LOGIN                        
                                <h2 style="color: red">
                                    Compras de #{usuarioNome usuario}
                                
                                <ul>
                                    $forall (Entity _ _, Entity _ compra, Entity _ produto) <- produtos
                                        <li style="font-size: 20px">
                                            #{produtoNome produto}: #{produtoPreco produto * (fromIntegral (compraQtunit compra))}
        |]

postComprarR :: ProdutoId -> Handler Html
postComprarR pid = do
    ((resp,_),_) <- runFormPost formQt
    case resp of 
         FormSuccess qt -> do 
             sess <- lookupSession "_EMAIL"
             case sess of 
                  Nothing -> redirect HomeR
                  Just email -> do 
                      usuario <- runDB $ getBy (UniqueEmail email)
                      case usuario of 
                           Nothing -> redirect HomeR 
                           Just (Entity uid _) -> do 
                               runDB $ insert (Compra pid uid qt)
                               redirect ListCompraR
         _ -> redirect HomeR
