module Ast where

data Frag
  = Lt String
  | St
  deriving (Eq, Show)

type Template = [Frag]

type File = String
type Command = String

type FileT = Template
type CommandT = Template

data Rule
  = Rule [FileT] [FileT] [CommandT]
  deriving (Eq, Show)

type Makefile = [Rule]
