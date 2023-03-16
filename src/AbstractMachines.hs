{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}


module AbstractMachines
    ( Parser (..)
    , AbstractMachine (..)
    ) where

import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Set
import           Data.Tree
import qualified Text.ParserCombinators.Parsec  as P


class Parser p where
    {-# MINIMAL parse, unparse, convertTerm, newVarName, substitute, aName #-}

    type Variable p = r | r -> p

    newVarName :: Set (Variable p) -> Variable p

    parse :: String -> Either P.ParseError p

    unparse :: p -> String

    convertTerm :: p -> Tree String

    substitute :: p -> Variable p -> p -> p

    aName :: p -> p

class (Parser p, Show s) => AbstractMachine p s | s -> p where
    {-# MINIMAL   initialState
                , step
                , decodeStateToTerm
                #-}

    initialState :: p -> s

    step :: s -> Maybe s

    decodeStateToTerm :: s -> p

    compile :: String -> State s (Maybe s)
    compile input = case parse input of
        Right parsedTerm -> pure $ Just $ initialState parsedTerm
        Left         err -> pure Nothing

    reduceNSteps :: Integer -> State s ()
    reduceNSteps n | n == 0    = pure ()
                   | otherwise = do st <- get
                                    case step st of
                                        Just newSt -> do put newSt
                                                         reduceNSteps (n - 1)
                                        Nothing    -> put st

    isNormal :: State s Bool
    isNormal = do st <- get
                  case step st of
                      Just  _ -> pure False
                      Nothing -> pure True

    -- Note that normalize may be non-terminating! That is why we provide
    -- reduceNSteps as a terminating alternative.
    normalize :: State s ()
    normalize = do st <- get
                   case step st of
                        Just newSt -> do put newSt
                                         normalize
                        Nothing    -> pure ()

    decodeState :: State s String
    decodeState = do st <- get
                     pure $ unparse $ decodeStateToTerm st
