{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module IO
    ( getCsvs
    , pageToFile
    , getMarks
    , getScripts
    , process
    ) where

import Printer
import Data.List
import System.Directory (listDirectory)
import System.FilePath (makeValid)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Lucid (renderToFile, Html)

type CSV = (Text, [[Text]]) -- a pair of the name of a csv and its contents, in a 2d list - each sublist is a single line of the csv

-- get all csv files from the editable/csv directory
getCsvs :: IO [CSV]
getCsvs = do
    -- all files in the csv directory, no filtering yet
    directory <- listDirectory $ makeValid "editable/csv"
    putStrLn "Found CSV Directory"
    -- those files, filtered to only csv files just in case
    let csvFiles = filter (".csv" `isSuffixOf`) directory
    putStrLn $ "Got CSVS: " ++ (show csvFiles)
    -- the text inside those files
    csvContents <- mapM IO.readFile . map ("editable/csv/" ++) $ csvFiles
    putStrLn "Read CSVS"
    -- [csv file, [[contents properly parsed]]
    return [ (maybe (error "impossible1") id . T.stripSuffix ".csv" . T.pack $ f, map (T.split (== '|') . T.strip) . T.lines . T.filter (/= '\65279') $ c) | (f, c) <- zip csvFiles csvContents ]

pageToFile :: T.Text -> T.Text -> Html () -> IO ()
pageToFile filename title page = do
    do putStrLn $ "Page: " ++ (show title) ++ " → " ++ (show filename)
    renderToFile (T.unpack filename) page

-- from pages.csv, return an io list of all the markdown pages from the csv, plus their md file contents (after processing)
-- [html file, title, markdown]
getMarks :: [(T.Text, [[T.Text]])] -> [[T.Text]] -> IO [[T.Text]]
getMarks csvs pages = do
    -- all "markdown" csv entries, minus the last entry
    let markdownsToGet = map init . filter (\entry -> last entry == "markdown") $ pages
    -- all associated files
    files <- mapM IO.readFile . map (makeValid . T.unpack . last) $ markdownsToGet
    -- the original csv entries, minus the last 2 entries, plus the md file contents
    return [ (init a) ++ [process csvs b] | (a, b) <- zip markdownsToGet files ]

-- [html file, title, css, body, script]
getScripts :: [(T.Text, [[T.Text]])] -> [[T.Text]] -> IO [[T.Text]]
getScripts csvs pages = do
    let scriptsToGet = map init . filter (\entry -> last entry == "script") $ pages
    filesCss <- mapM IO.readFile . map (makeValid . T.unpack . (!!2)) $ scriptsToGet
    filesBody <- mapM IO.readFile . map (makeValid . T.unpack . (!!3)) $ scriptsToGet
    filesScript <- mapM IO.readFile . map (makeValid . T.unpack . (!!4)) $ scriptsToGet
    return [ (init . init . init $ a) ++ [process csvs b] ++ [process csvs c] ++ [process csvs d] | (a, b, c, d) <- zip4 scriptsToGet filesCss filesBody filesScript ]

-- go through a file, replace all things of the form "!!!csv|parser" with that csv read line-by-line by that parser, and all things of the form "!!!csv||parser" with that csv read in one line by that parser
process :: [(Text, [[Text]])] -> Text -> Text
process csvs mark = T.unlines [ parse line | line <- T.lines mark ]
    where
        items :: Text -> [[Text]]
        items csv = maybe (error $ "missing csv " ++ (show csv)) id (lookup csv csvs)
        parse :: Text -> Text
        parse line
            | "!!!" `T.isPrefixOf` line = do
                let splitLine = T.split (== '|') . maybe (error "impossible") id . T.stripPrefix "!!!" $ line
                case (length splitLine) of
                    2 -> T.unlines . map (printer csvs . last $ splitLine) . items . head $ splitLine
                    3 -> printerOneliner csvs (last splitLine) (items . head $ splitLine)
            | otherwise = line
