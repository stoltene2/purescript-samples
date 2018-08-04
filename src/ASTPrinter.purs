module ASTPrinter where

import Prelude

data Expression = Expression

data Statement = ExpressionStatement Expression
               | BlockStatement
               | EmptyStatement
               | DebuggerStatement
               | WithStatement
               | ReturnStatement
               | LabeledStatement
               | BreakStatement
               | ContinueStatement
               | IfStatement
               | SwitchStatement
               | ThrowStatement
               | TryStatement
               | WhileStatement
               | DoWhileStatement
               | ForStatement
               | ForInStatement
               | DeclarationStatement Declaration

-- data Declaration = FunctionDeclaration
--                  | VariableDeclaration


newtype Identifier = Identifier String

-- TODO: I don't think Identifier is correct here
data VariableKind = LetIdentifier | VarIdentifier | ConstIdentifier

data Declaration = VariableDeclaration VariableKind Identifier


class Print a where
  print :: a -> String


instance printIdentifier :: Print Identifier where
  print (Identifier i) = i


instance printVariableKind :: Print VariableKind where
  print LetIdentifier   = "let"
  print VarIdentifier   = "var"
  print ConstIdentifier = "const"


instance printDeclaration :: Print Declaration where
  print (VariableDeclaration kind ident) = print kind <> " " <> print ident <> " = "
