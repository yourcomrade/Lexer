module Lexer where

import System.IO
import Prelude 
import qualified Data.List as L
import qualified Data.Text as T
import Data.Char (isSpace, isDigit)
import Data.Maybe (fromMaybe, mapMaybe)
data InfoEntity = InfoEntity {
    name:: Maybe String,
    freq :: Maybe Int,
    pipedep ::Maybe Int,
    insig:: Maybe [ String],
    outsig::Maybe [String]
    } deriving(Show)

{-class AnyNothing a where
    hasAnyNothing :: a -> Bool
-- Make InfoEntity an instance of AnyNothing
instance AnyNothing InfoEntity where
    hasAnyNothing (InfoEntity name freq pipedep insig outsig) =
        any isNothing [name, freq, pipedep, insig, outsig]

-- Helper function to check if Maybe value is Nothing
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False
-}
keywords = ["Input", "Output", "frequency", "Pipeline"]
-- Define an empty or default InfoEntity with all fields as Nothing
emptyInfoEntity :: InfoEntity
emptyInfoEntity = InfoEntity {
    name     = Nothing,
    freq     = Nothing,
    pipedep  = Nothing,
    insig    = Nothing,
    outsig   = Nothing
}
readFileLineByLine :: FilePath -> IO ()
readFileLineByLine filePath = withFile filePath ReadMode readLines

readLines :: Handle -> IO ()
readLines handle = do
    eof <- hIsEOF handle
    if eof
        then return ()
    else do
        line <- hGetLine handle
        putStrLn line
        readLines handle

isVHDLcomment :: String -> Bool
isVHDLcomment str  | length str > 2 = if take 2 str == "--" then True else False
                   | otherwise = False 
getVHDLComment :: String -> Maybe String
getVHDLComment str | length str > 2 = if take 2 str == "--" then Just $ drop 2 str else Nothing
                   | otherwise = Nothing
                                                    
containsSpace :: String -> Bool
containsSpace str = any isSpace str

afterColon :: Maybe [String] -> Maybe [String]
afterColon Nothing = Nothing
afterColon (Just wordList) = 
    case dropWhile (notElem ':') wordList of
        []       -> Nothing            -- No colon found, return Nothing
        (_:rest) -> Just rest           -- Skip the word with the colon and return the rest

updateInfoEntity :: String -> Maybe InfoEntity -> Maybe InfoEntity
updateInfoEntity _ Nothing = Nothing
updateInfoEntity  comment (Just infoen) = 
    if ( containsSpace comment) ==  False then
        Just infoen 
    else
        let wordList = words comment
        in if (length wordList) ==  1  then
            let singleword = head wordList
            in if (elem ')'  singleword ) ==  False then
                Just infoen{name = Just singleword}
            else 
                Just infoen
        else
            case [word | word <- keywords, elem word wordList] of
                [] -> Just infoen
                ["Input"] -> Just infoen {insig =  afterColon (Just wordList)}
                ["Output"] -> Just infoen {outsig = afterColon (Just wordList)}
                ["Pipeline"] -> Just infoen { pipedep = fmap (read . filter isDigit . concat) (afterColon (Just wordList)) }
                ["frequency"] -> Just infoen { freq = fmap (read . filter isDigit . concat) (afterColon (Just wordList)) }

makeListVHDLcomments :: String -> Maybe [String]
makeListVHDLcomments vhdlcontent =
   Just $ mapMaybe getVHDLComment  [ vhdlcomment | vhdlcomment <- (lines vhdlcontent), isVHDLcomment vhdlcomment]

--makeListVHDLInfoEntities :: Maybe [String] -> Maybe [InfoEntity]
--makeListVHDLInfoEntities vhdlcomments = do

getLastInfoEntity :: Maybe [String] -> Maybe InfoEntity -> Maybe InfoEntity
getLastInfoEntity (Just vhdlcomments) (Just infoen) =
    if (length vhdlcomments) == 0 then
        Just infoen
    else
        getLastInfoEntity (Just (drop 1 vhdlcomments)) (updateInfoEntity (head vhdlcomments) (Just infoen))
   
        

processFile :: (Maybe [String] ->Maybe InfoEntity -> Maybe InfoEntity) -> FilePath -> IO ()
processFile process path = do
    handle <- openFile path ReadMode
    contents <- hGetContents handle
    let vhdlcomments = makeListVHDLcomments contents -- Maybe [String]
    putStrLn $ maybe "No comments found" unlines vhdlcomments
    case vhdlcomments of
        Just comments -> do
            let result = process (Just comments) (Just emptyInfoEntity) -- Process comments starting with an empty InfoEntity
            case result of
                Just info -> putStrLn (show info)
                Nothing   -> putStrLn "No information available."
        Nothing -> putStrLn "No VHDL comments found."
    hClose handle

main :: IO ()
main = do
    let filePath = "test.txt"
    --readFileLineByLine filePath
    processFile getLastInfoEntity filePath
