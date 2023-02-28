module Types
    ( Term (..)
    , Variable
    , State
    ) where

data Term = Var Variable | App Term Term | Lambda Variable Term
  deriving (Eq, Show)

type Variable = String

type State = (Term, [Term])
