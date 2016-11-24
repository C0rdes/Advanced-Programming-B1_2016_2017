module Parser.Impl where

import Ast

type ParseError = String -- Must be instance of (Eq, Show).

-- Your code here.

parseString :: String -> Either ParseError Makefile
parseString _ = Left "Not Implemented"

parseFile :: FilePath -> IO (Either ParseError Makefile)
parseFile path = fmap parseString $ readFile path
