{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
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


class Parser (t :: * -> *) v  | v -> t where
    {-# MINIMAL parse, unparse, convertTerm, newVarName, substitute, aName #-}

    newVarName :: Set v -> v

    parse :: String -> Either P.ParseError (t v)

    unparse :: t v -> String

    convertTerm :: t v -> Tree String

    substitute :: t v -> v -> t v -> t v

    aName :: t v -> t v

class (Parser t v, Show s) => AbstractMachine t v s | s -> t v where
    {-# MINIMAL   initialState
                , step
                , decodeStateToTerm
                #-}

    initialState :: t v -> s

    step :: s -> Maybe s

    decodeStateToTerm :: s -> t v

    compile :: String -> State s (Maybe s)
    compile input = case (parse input) :: Either P.ParseError (t v) of
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
                  pure $ not $ isJust $ step st

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
                     pure $ (unparse :: t v -> String) $ decodeStateToTerm st
