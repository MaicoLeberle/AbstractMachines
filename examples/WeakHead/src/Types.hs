{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Types
    ( WHTerm
    , WHState
    ) where

import           Control.Monad.Reader
import           Data.Set
import           Data.Tree
import qualified Text.ParserCombinators.Parsec as P

import           AbstractMachines as AM

data WH a =
      Var a
    | App (WH a) (WH a)
    | Lambda a (WH a)
  deriving (Eq, Show)

type Variable = String

type WHTerm = WH Variable

instance AM.Parser WH Variable where
    newVarName :: Set Variable -> Variable
    newVarName sv = breakOnSets $ Prelude.map (:[]) alphabet
      where
        breakOnSets :: [Variable] -> Variable
        breakOnSets currLayer = case breakOnSetsAux currLayer of
            Just res -> res
            Nothing  -> let nextLayer = do p <- currLayer
                                           s <- alphabet ++ digits
                                           pure (p ++ [s])
                        in breakOnSets nextLayer

        breakOnSetsAux :: [Variable] -> Maybe (Variable)
        breakOnSetsAux [] = Nothing
        breakOnSetsAux (y:ys) | not $ y `member` sv = Just y
                              | otherwise = breakOnSetsAux ys

    parse :: String -> Either P.ParseError WHTerm
    parse = P.parse parseAux "Failed to parse."
      where
        parseAux :: P.GenParser Char st WHTerm
        parseAux = do
            parseRes <- parseTerm
            P.eof
            pure parseRes

        parseTerm :: P.GenParser Char st WHTerm
        parseTerm =
                (parseVar >>= pure . Var)
            P.<|> (do
                    P.char '('
                    res <- parseApp P.<|> parseLambda
                    P.char ')'
                    pure res
                )

        parseVar :: P.GenParser Char st String
        parseVar = do
            i <- getLetter
            r <- parseVarAux ""
            pure $ i : r
          where
            parseVarAux :: String -> P.GenParser Char st String
            parseVarAux varNamePrefix =
                    (getLetter >>= parseVarAux . (: varNamePrefix))
                P.<|> (getDigit >>= parseVarAux . (: varNamePrefix))
                P.<|> pure (reverse varNamePrefix)

            getLetter :: P.GenParser Char st Char
            getLetter = P.oneOf alphabet

            getDigit :: P.GenParser Char st Char
            getDigit = P.oneOf digits

        parseLambda :: P.GenParser Char st WHTerm
        parseLambda = do
            P.char 'L'
            vName <- parseVar
            P.char '.'
            body <- parseTerm
            pure $ Lambda vName body

        parseApp :: P.GenParser Char st WHTerm
        parseApp = do
            firstTerm <- parseTerm
            P.char ' '
            secondTerm <- parseTerm
            pure $ App firstTerm secondTerm

    unparse :: WHTerm -> String
    unparse (Lambda v t) = "(L" ++ v ++ "." ++ unparse t ++ ")"
    unparse (App t1 t2) = "(" ++ unparse t1 ++ " " ++ unparse t2 ++ ")"
    unparse (Var v) = v

    convertTerm :: WHTerm -> Tree String
    convertTerm (Var v) =
        Node { rootLabel = "VAR"
             , subForest = [ Node {rootLabel = v, subForest = [] } ]
             }

    convertTerm (Lambda v t) =
        Node { rootLabel = "L" ++ v ++ "."
             , subForest = [convertTerm t]
             }

    convertTerm (App t s) =
        Node { rootLabel = "APP"
             , subForest = [convertTerm t, convertTerm s]
             }

    substitute
        :: WHTerm -> Variable -> WHTerm -> WHTerm
    substitute orig var subsFor = runReader (subs orig var subsFor) empty
      where
        subs :: WHTerm
             -> Variable
             -> WHTerm
             -> Reader (Set (Variable)) WHTerm
        subs t@(Var v') v u | v' == v   = pure u
                            | otherwise = pure t
        subs (App t1 t2) v u = do
            bound <- ask
            pure $ App (runReader (subs t1 v u) bound)
                         (runReader (subs t2 v u) bound)
        subs t@(Lambda v' s) v u
            | v == v' = pure t
            | v' `member` fv u =
                do nVar <- asks newVarName
                   nBody <- local (nVar `insert`) (subs s v' (Var nVar))
                   local (nVar `insert`)
                         $ asks $ Lambda nVar . (runReader (subs nBody v u))
            | otherwise = asks $ Lambda v' . (runReader (subs s v u))
          where
            fv :: WHTerm -> Set (Variable)
            fv t = fvAux t empty

            fvAux :: WHTerm
                  -> Set (Variable)
                  -> Set (Variable)
            fvAux (Var v) bound | v `member` bound = empty
                                | otherwise        = singleton v
            fvAux (App t1 t2) bound = fvAux t1 bound `union` fvAux t2 bound
            fvAux (Lambda v t) bound = fvAux t $ bound `union` singleton v

    aName :: WHTerm -> WHTerm
    aName t = runReader (aNameAux t) empty
      where
        aNameAux
            :: WHTerm -> Reader (Set (Variable)) WHTerm
        aNameAux v@(Var _) = pure v
        aNameAux (App t1 t2) =
            do bound <- ask
               pure $ App (runReader (aNameAux t1) bound)
                          (runReader (aNameAux t2) bound)
        aNameAux (Lambda v t) =
            do bound <- ask
               if v `member` bound
                   then do nVar <- asks newVarName
                           nBody <- local (nVar `insert`) (subs t v (Var nVar))
                           local (nVar `insert`) $ asks $
                                Lambda nVar . runReader (aNameAux nBody)
                   else asks $ Lambda v . runReader (aNameAux t) . (v `insert`)

        subs :: WHTerm
             -> Variable
             -> WHTerm
             -> Reader (Set (Variable)) WHTerm
        subs t@(Var v') v u | v' == v   = pure u
                            | otherwise = pure t
        subs (App t1 t2) v u = do
            bound <- ask
            pure $ App (runReader (subs t1 v u) bound)
                         (runReader (subs t2 v u) bound)
        subs t@(Lambda v' s) v u
            | v == v' = pure t
            | v' `member` fv u =
                do nVar <- asks newVarName
                   nBody <- local (nVar `insert`) (subs s v' (Var nVar))
                   local (nVar `insert`)
                         $ asks $ Lambda nVar . (runReader (subs nBody v u))
            | otherwise = asks $ Lambda v' . (runReader (subs s v u))
          where
            fv :: WHTerm -> Set (Variable)
            fv t = fvAux t empty

            fvAux :: WHTerm
                  -> Set (Variable)
                  -> Set (Variable)
            fvAux (Var v) bound | v `member` bound = empty
                                | otherwise        = singleton v
            fvAux (App t1 t2) bound = fvAux t1 bound `union` fvAux t2 bound
            fvAux (Lambda v t) bound = fvAux t $ bound `union` singleton v


instance AM.AbstractMachine WH Variable WHState where
    initialState :: WHTerm -> WHState
    initialState = WHState . (, [])

    step :: WHState -> Maybe WHState
    step (WHState (Lambda v t, a : aa)) = Just $ WHState (substitute t v a, aa)
    step (WHState (App t1 t2, a)) = Just $ WHState (t1, t2 : a)
    step _ = Nothing

    decodeStateToTerm :: WHState -> WHTerm
    decodeStateToTerm (WHState (t, aa)) = Prelude.foldl App t aa


-- | Auxiliary values and types.
alphabet :: [Char]
alphabet = ['a'..'z']

digits :: [Char]
digits = ['0'..'9']

data WHState = WHState (WHTerm, [WHTerm])
  deriving Eq

instance Show WHState where
  show (WHState (t, s)) = concat [ "( "
                                 , unparse t
                                 , "\n, "
                                 , unparseStack s
                                 , "\n)"
                                 ]

unparseStack :: [WHTerm] -> String
unparseStack stack = concat [ "["
                            , unparseStackAux
                            , "]"
                            ]
  where
    unparseStackAux :: String
    unparseStackAux = case stack of
        []       -> ""
        [t]      -> unparse t
        (t : tt) -> concat [ unparse t
                           , ", "
                           , unparseStack tt
                           ]
