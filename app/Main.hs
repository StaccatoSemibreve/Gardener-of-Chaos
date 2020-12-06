{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Main where

import IO
import Printer
import Data.Text (Text)
import qualified Data.Text as T
import CMarkGFM (optUnsafe, extStrikethrough, extTable, extTaskList, commonmarkToHtml)
import Lucid

pageMarkdown :: Html () -> T.Text -> T.Text -> Html ()
pageMarkdown navbar title markdown = doctypehtml_ $ do
    head_ $ do
        title_ (toHtml title)
        meta_ [charset_ "utf-8"]
        link_ [rel_ "icon", type_ "image/png", href_ "FSgardenOfChao-196x196.png", sizes_ "196x196"]
        link_ [rel_ "stylesheet", href_ "markdown-style.css"]
        link_ [rel_ "stylesheet", href_ "navbar-style.css"]
    body_ $ do
        navbar
        div_ [id_ "center-div"] $ do
            Lucid.toHtmlRaw $ commonmarkToHtml [optUnsafe] [extStrikethrough, extTable, extTaskList] markdown

pageNonStatic :: Html () -> T.Text -> T.Text -> T.Text -> T.Text -> Html ()
pageNonStatic navbar title style body script = doctypehtml_ $ do
    head_ $ do
        title_ (toHtml title)
        meta_ [charset_ "utf-8"]
        link_ [rel_ "icon", type_ "image/png", href_ "FSgardenOfChao-196x196.png", sizes_ "196x196"]
        link_ [rel_ "stylesheet", href_ "navbar-style.css"]
        style_ style
    body_ $ do
        navbar
        (toHtmlRaw body)
    script_ script

pageNavbar :: [[Text]] -> Html ()
pageNavbar items = do
    header_ [class_ "navbar-div"] $ do
        mapM_ navbarItem items
    where
        navbarItem :: [Text] -> Html ()
        navbarItem ("icon":name:desc:pos:url:iconId:iconSrc:iconAlt:[]) = div_ [class_ " navbar-item-container", class_ pos] $ do
            a_ [href_ url, class_ "navbar-item", title_ desc] $ do
                div_ [class_ " navbar-item-container", class_ pos] $ do
                    img_ [id_ iconId, class_ "navbar-icon", src_ iconSrc, alt_ iconAlt]
                    span_ [class_ "navbar-item"] $ toHtml name
        navbarItem ("text":name:desc:pos:url:[]) = div_ [class_ " navbar-item-container", class_ pos] $ do
            a_ [href_ url, class_ "navbar-item", title_ desc] $ do
                div_ [class_ " navbar-item-container", class_ pos] $ do
                    div_ [class_ "navbar-item"] $ toHtml name
        navbarItem _ = do
            error "invalid navbar item"

main :: IO ()
main = do
    csvs <- getCsvs
    let navbar = maybe (error "navbar .csv missing, can't make pages without being told about the navbar!") pageNavbar $ lookup "navbar" csvs
    let pages = maybe (error "pages.csv missing, can't make pages without being told how to!") id $ lookup "pages" csvs
    putStrLn $ "Pages: " ++ (show $ map head pages)
    
    markdowns <- getMarks csvs pages
    mapM_ (\(html:title:md:[]) -> pageToFile html title . pageMarkdown navbar title $ md) markdowns
    
    scripts <- getScripts csvs pages
    mapM_ (\(html:title:css:body:script:[]) -> pageToFile html title . pageNonStatic navbar title css body $ script) scripts
    
    putStrLn ""
    putStrLn "All done, now remember to do the announcements by editing the svg with a text editor (just ctrl-f the different bits of text, maybe shuffle some line spacing about if needed) and export at 1000px wide using inkscape, commit it to git, and push that commit!"
    putStrLn "Send Leontes the big announcement image, and send safety_man the small one!"
