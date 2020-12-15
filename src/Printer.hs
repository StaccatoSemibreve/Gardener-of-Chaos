{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Printer
    ( printer
    , printerOneliner
    ) where 

import Data.Char (isAlphaNum)
import qualified Data.Text as T
import Text.Printf (printf)
import Formatting (sformat, stext, shown, (%))

urlEncode :: T.Text -> T.Text
urlEncode = T.foldl (\text c -> text `T.append` (charEncode c)) T.empty
    where
        charEncode c
            | c == ' '                                                                              = "+"
            | isAlphaNum c || c == '-' || c == '.' || c == '_' || c == '~' || c == '\''   = T.singleton c
            | otherwise                                                                             = T.pack $ printf "%%%02X" c

printer :: [(T.Text, [[T.Text]])] -> T.Text -> [T.Text] -> T.Text
printer _ _ [] = ""
printer _ _ [""] = ""

printer _ "ruleset-list" (name:desc:"":[]) = sformat ("* <a name=\"" % stext % "\">**[" % stext % ":](#" % stext % ")** " % stext % "</a>") (urlEncode name) name (urlEncode name) desc
printer _ "ruleset-list" (name:desc:source:[]) = sformat ("* <a name=\"" % stext % "\">**[" % stext % ":](#" % stext % ")** " % stext % " [Source: " % stext % "]</a>") (urlEncode name) name (urlEncode name) desc source

printer _ "format-list" (name:desc:"":[]) = sformat ("* <a name=\"" % stext % "\">**[" % stext % ":](#" % stext % ")** " % stext % "</a>") (urlEncode name) name (urlEncode name) desc
printer _ "format-list" (name:desc:source:[]) = sformat ("* <a name=\"" % stext % "\">**[" % stext % ":](#" % stext % ")** " % stext % " [Source: " % stext % "]</a>") (urlEncode name) name (urlEncode name) desc source

printer csvs "index-info" (name:desc:datetime:[]) = sformat ("**" % stext % "** (" % stext % ")\n> " % stext % "\n\n" % stext) name desc formatDesc datetime
    where
        formatDesc = case formatDetails!!1 of
                          "" -> sformat stext (formatDetails!!0)
                          _ -> sformat (stext % " [Source: " % stext % "]") (formatDetails!!0) (formatDetails!!1)
                    where
                        formatDetails = maybe (error "format missing") id . lookup name . map (\item -> (head item, tail item)) . maybe (error "formats.csv missing") id . lookup "formats" $ csvs

printer _ "charicon-teambattle" (name:_) = sformat ("<image src=\"media/" % stext % ".png\" alt=\"" % stext % "\" id=\"" % stext % "\" class=\"icon\" />") name name name

printer _ "charicon-charselect1" (name:_) = sformat ("<image class=\"icon\" style=\"grid-area:" % stext % ";\" src=\"media/" % stext % ".png\" alt=\"" % stext % "\" /><a href=\"javascript:void(0);\" style=\"grid-area:" % stext % ";\" title=\"Add " % stext % " to Team 1.\" onclick='addChar(\"" % stext % "\",1)'><image class=\"overlay_char\" src=\"media/plus.svg\" /></a>") name name name name name name
printer _ "charicon-charselect2" (name:_) = sformat ("<image class=\"icon\" style=\"grid-area:" % stext % ";\" src=\"media/" % stext % ".png\" alt=\"" % stext % "\" /><a href=\"javascript:void(0);\" style=\"grid-area:" % stext % ";\" title=\"Add " % stext % " to Team 2.\" onclick='addChar(\"" % stext % "\",2)'><image class=\"overlay_char\" src=\"media/plus.svg\" /></a>") name name name name name name

printer _ "charicon-charselect1jp" (name:_) = sformat ("<image class=\"icon\" style=\"grid-area:" % stext % ";\" src=\"media/" % stext % ".png\" alt=\"" % stext % "\" /><a href=\"javascript:void(0);\" style=\"grid-area:" % stext % ";\" title=\"" % stext % "をチーム１\nに加える。\" onclick='addChar(\"" % stext % "\",1)'><image class=\"overlay_char\" src=\"media/plus.svg\" /></a>") name name name name name name
printer _ "charicon-charselect2jp" (name:_) = sformat ("<image class=\"icon\" style=\"grid-area:" % stext % ";\" src=\"media/" % stext % ".png\" alt=\"" % stext % "\" /><a href=\"javascript:void(0);\" style=\"grid-area:" % stext % ";\" title=\"" % stext % "をチーム２\nに加える。\" onclick='addChar(\"" % stext % "\",2)'><image class=\"overlay_char\" src=\"media/plus.svg\" /></a>") name name name name name name

printer _ "presets1team1" (name:svg:alt:flavour:code:"minus":[]) = sformat ("<image class=\"icon\" style=\"grid-area:" % stext % ";\" src=\"media/" % stext % "\" alt=\"" % stext % "\" /><a href=\"javascript:void(0);\" style=\"grid-area:" % stext % ";\" title=\"" % stext % "\" onclick='" % stext % "'><image class=\"overlay_char\" src=\"media/minus.svg\" style=\"background-color:red\" /></a>") name svg alt name (T.replace "\\team" "1" flavour) (T.replace "\\team" "1" code)
printer _ "presets1team1" (name:svg:alt:flavour:code:"plus":[]) = sformat ("<image class=\"icon\" style=\"grid-area:" % stext % ";\" src=\"media/" % stext % "\" alt=\"" % stext % "\" /><a href=\"javascript:void(0);\" style=\"grid-area:" % stext % ";\" title=\"" % stext % "\" onclick='" % stext % "'><image class=\"overlay_char\" src=\"media/plus.svg\"/></a>") name svg alt name (T.replace "\\team" "1" flavour) (T.replace "\\team" "1" code)
printer _ "presets1team2" (name:svg:alt:flavour:code:"minus":[]) = sformat ("<image class=\"icon\" style=\"grid-area:" % stext % ";\" src=\"media/" % stext % "\" alt=\"" % stext % "\" /><a href=\"javascript:void(0);\" style=\"grid-area:" % stext % ";\" title=\"" % stext % "\" onclick='" % stext % "'><image class=\"overlay_char\" src=\"media/minus.svg\" style=\"background-color:red\" /></a>") name svg alt name (T.replace "\\team" "2" flavour) (T.replace "\\team" "2" code)
printer _ "presets1team2" (name:svg:alt:flavour:code:"plus":[]) = sformat ("<image class=\"icon\" style=\"grid-area:" % stext % ";\" src=\"media/" % stext % "\" alt=\"" % stext % "\" /><a href=\"javascript:void(0);\" style=\"grid-area:" % stext % ";\" title=\"" % stext % "\" onclick='" % stext % "'><image class=\"overlay_char\" src=\"media/plus.svg\"/></a>") name svg alt name (T.replace "\\team" "2" flavour) (T.replace "\\team" "2" code)

printer _ "tournament-archive" (name:desc:ident:video:bracket:"both":[]) = sformat ("<li><strong><a href=\"https://www.youtube.com/watch?v=" % stext % "\">" % stext % ":</a></strong> " % stext % "<br>\n<button onclick=\'addArchive(this, \"" % stext % "\", \"https://www.youtube.com/embed/" % stext % "\", \"https://challonge.com/" % stext % "/module\"\'>Watch Archive (will load external content!)</button></li>") video name desc ident video bracket
printer _ "tournament-archive" (name:desc:ident:bracket:"bracket":[]) = sformat ("<li><strong>" % stext % ":</strong> " % stext % "<br>\n<button onclick=\'addBracket(this, \"" % stext % "\", \"https://challonge.com/" % stext % "\")\'>View Bracket (will load external content!)</button></li>") name desc ident bracket
printer _ "tournament-archive" (name:desc:ident:video:"video":[]) = sformat ("<li><strong><a href=\"" % stext % "\">" % stext % ":</a></strong> " % stext % "<br>\n<button onclick=\'addMatch(this, \"" % stext % "\", \"https://www.youtube.com/embed/" % stext % "\")\'>Watch Match (will load external content!)</button></li>") video name desc ident video
printer _ "tournament-archive" (name:desc:"skipped":[]) = sformat ("<li><strong>" % stext % ":</strong> " % stext % "</li>") name desc

printer csvs style contents = sformat ("ERROR: invalid print format: [" % stext % "] in format: " % stext) (T.pack . show $ contents) style

-- 				<button onclick='addBracket(this, "gocu", "https://challonge.com/FSGardenofChaosUpdate/module")'>View Bracket (will load external content!)</button></li>
-- 				<li><strong><a>The Big Update Tournament:</a></strong> A 9-player Battle of Flagstone style tournament held on the 23rd of July 2020, won by <span class="spoiler">Darkness, EmiSocks, Mozzarella, and ThyPirateKing</span>!<br>

printerOneliner :: [(T.Text, [[T.Text]])] -> T.Text -> [[T.Text]] -> T.Text
printerOneliner _ "JsArrayHead" formats = do
    collect . map (esc . head) $ formats
    where
        collect :: [T.Text] -> T.Text
        collect [] = ""
        collect items = "'" `T.append` (collectR items) where
            collectR :: [T.Text] -> T.Text
            collectR [] = ""
            collectR (t:[]) = t `T.append` "'"
            collectR (t:ts) = t `T.append` "', '" `T.append` (collectR ts)
        
        esc :: T.Text -> T.Text
        esc = T.foldl (\text c -> text `T.append` (charEncode c)) T.empty
            where
                charEncode c
                    | c == '\''                                                                              = "\\'"
                    | otherwise                                                                             = T.singleton c
printerOneliner _ "JsOptionsHead" formats = "\'" `T.append` (foldl T.append "" . map (uncurry . sformat $ ("<option value=\"" % stext % "\">" % stext % "</option>")) . map (\x -> (x,x)) . map head $ formats) `T.append` "\'"

printerOneliner _ _ _ = ""
