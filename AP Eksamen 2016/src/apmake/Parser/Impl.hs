module Parser.Impl where

import Control.Monad ( void )

import Ast
import SimpleParse

type ParseError = String -- Must be instance of (Eq, Show). (String derives Show and Eq)

parseString :: String -> Either ParseError Makefile
parseString s = 
    case fullParse (pRules <* eof) s of
        [] -> Left "Error"
        (e:_) -> Right e
        
 
pLiteral' :: Parser Char
pLiteral' = do
    c <- chars $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_', '.', '/', '-', '\\'] 
    if c == '\\'
    then do
        c2 <- chars ['%', ':', ' ', '\\']
        return c2
    else 
        return c
        

    
pLiteral :: Parser Frag
pLiteral = do
    s <- some pLiteral'
    return $ Lt s
    
pSt :: Parser Frag
pSt = do
    _ <- char '%'
    return St
    
pFile :: Parser Template
pFile = do
    _ <- mySpaces
    some $ choice [pLiteral, pSt]
        
    
pCommandT'' :: Parser Char
pCommandT'' = do
    c <- satisfy $ \x -> x /= '\n' && x /= '%'
    if c == '\\'
    then do
        c2 <- chars ['\n', '\\']
        if c2 == '\n'
        then pCommandT''
        else return c2
    else 
        return c    
    
pCommandT' :: Parser Frag
pCommandT' = do
    s <- some pCommandT''  
    return $ Lt s    
    
pCommandT :: Parser CommandT
pCommandT = do 
    mySToken "\n"
    _ <- char '\t'
    some $ choice [pCommandT', pSt]
    
    

pTargets :: Parser [FileT]
pTargets = do
    _ <- mySpaces
    some pFile 
    
pPrereqs :: Parser [FileT]
pPrereqs = do
    _ <- mySpaces
    many pFile
    

pRule :: Parser Rule
pRule = do
    _ <- mySpaces 
    targets <- pTargets
    _ <- mySToken ":"
    prereqs <- pPrereqs
    commands <- many pCommandT
    _ <- mySToken "\n"
    _ <- mySpaces 
    if anyContainsSt prereqs || anyContainsSt commands
    then 
        if allContainsSt targets
            then return $ Rule targets prereqs commands
            else reject
    else
        return $ Rule targets prereqs commands

pRules :: Parser Makefile
pRules = some pRule

-------------
-- Helpers --
-------------
      
myToken :: Parser a -> Parser a
myToken p = mySpaces >> p

mySToken :: String -> Parser ()
mySToken = void . myToken . string

mySpaces :: Parser String
mySpaces = munch myIsSpace

myIsSpace :: Char -> Bool
myIsSpace c = c == ' '

anyContainsSt :: [[Frag]] -> Bool
anyContainsSt fs = any (\x -> St `elem` x) fs

allContainsSt :: [[Frag]] -> Bool
allContainsSt fs = all (\x -> St `elem` x) fs


parseFile :: FilePath -> IO (Either ParseError Makefile)
parseFile path = fmap parseString $ readFile path

