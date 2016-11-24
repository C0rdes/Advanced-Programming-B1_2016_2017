module Parser
  ( ParseError
  , parseString
  , parseFile
  ) where

-- SimpleParse/ReadP
import Parser.Impl
  ( ParseError
  , parseString
  , parseFile
  )
  


{-
-- Parsec
import Parser.Impl
  ( parseString
  , parseFile
  )

import Text.ParserCombinators.Parsec ( ParseError )
-}
