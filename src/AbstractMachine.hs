{-# LANGUAGE TupleSections #-}

module AbstractMachine
    ( compile
    , compileTerm
    , step
    , reduceNSteps
    , isNormal
    , normalize
    , runAM
    , runAMNSteps
    , runAMOnTerm
    , runAMOnTermNSteps
    , decodeState
    , decodeStateToTerm
    ) where

import Data.Maybe

import Types
import Parser as P
import Substitutions as S


compile :: String -> Maybe State
compile input = case P.parse input of
    Left    _ -> Nothing
    Right res -> Just $ mkInitState res

compileTerm :: Term -> State
compileTerm = (,[])

step :: State -> Maybe State
step (Lambda v t, a : aa) = Just (S.substitute t v a, aa)
step (App t1 t2, a) = Just (t1, t2 : a)
step _ = Nothing

reduceNSteps :: Integer -> State -> Maybe State
reduceNSteps n s
    | n == 0 = Just s
    | otherwise = case step s of
        Nothing -> Just s
        Just next -> reduceNSteps (n - 1) next

isNormal :: State -> Bool
isNormal = isJust . step

normalize :: State -> Maybe Term
normalize s = case step s of
    Nothing -> Just $ decodeStateToTerm s
    Just s' -> normalize s'

runAM :: String -> Maybe String
runAM = (unparse <$>) . (>>= normalize) . compile

runAMNSteps :: Integer -> State -> Maybe String
runAMNSteps n s = decodeState <$> (reduceNSteps n s)

runAMOnTerm :: Term -> Maybe Term
runAMOnTerm = normalize . mkInitState

runAMOnTermNSteps :: Integer -> Term -> Maybe Term
runAMOnTermNSteps n t = fst <$> reduceNSteps n (mkInitState t)

decodeStateToTerm :: State -> Term
decodeStateToTerm (t, aa) = foldl App t aa

decodeState :: State -> String
decodeState = unparse . decodeStateToTerm


-- | Auxiliary values
mkInitState :: Term -> State
mkInitState = (, [])
