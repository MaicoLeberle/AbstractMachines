{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Types
    ( Syntax
    , CbVState (..)
    ) where

import           Control.Monad.Reader
import           Data.List (find)
import           Data.Set
import           Data.Tree
import qualified Text.ParserCombinators.Parsec as P

import           AbstractMachines as AM


instance AM.Parser Syntax where
    data Term Syntax =
          Var (Variable Syntax)
        | App (Term Syntax) (Term Syntax)
        | Lambda (Variable Syntax) (Term Syntax)
      deriving (Eq, Show)

    type Variable Syntax = String

    newVarName :: Set (Variable Syntax) -> Variable Syntax
    newVarName sv = breakOnSets $ Prelude.map (:[]) alphabet
      where
        breakOnSets :: [Variable Syntax] -> Variable Syntax
        breakOnSets currLayer = case breakOnSetsAux currLayer of
            Just res -> res
            Nothing  -> let nextLayer = do p <- currLayer
                                           s <- alphabet ++ digits
                                           pure (p ++ [s])
                        in breakOnSets nextLayer

        breakOnSetsAux :: [Variable Syntax] -> Maybe (Variable Syntax)
        breakOnSetsAux [] = Nothing
        breakOnSetsAux (y:ys) | not $ y `member` sv = Just y
                              | otherwise = breakOnSetsAux ys

    parse :: String -> Either P.ParseError (Term Syntax)
    parse = P.parse parseAux "Failed to parse."
      where
        parseAux :: P.GenParser Char st (Term Syntax)
        parseAux = do
            parseRes <- parseTerm
            P.eof
            pure parseRes

        parseTerm :: P.GenParser Char st (Term Syntax)
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

        parseLambda :: P.GenParser Char st (Term Syntax)
        parseLambda = do
            P.char 'L'
            vName <- parseVar
            P.char '.'
            body <- parseTerm
            pure $ Lambda vName body

        parseApp :: P.GenParser Char st (Term Syntax)
        parseApp = do
            firstTerm <- parseTerm
            P.char ' '
            secondTerm <- parseTerm
            pure $ App firstTerm secondTerm

    unparse :: Term Syntax -> String
    unparse (Lambda v t) = "(L" ++ v ++ "." ++ unparse t ++ ")"
    unparse (App t1 t2) = "(" ++ unparse t1 ++ " " ++ unparse t2 ++ ")"
    unparse (Var v) = v

    convertTerm :: Term Syntax -> Tree String
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
        :: Term Syntax -> Variable Syntax -> Term Syntax -> Term Syntax
    substitute orig var subsFor = runReader (subs orig var subsFor) empty
      where
        subs :: Term Syntax
             -> Variable Syntax
             -> Term Syntax
             -> Reader (Set (Variable Syntax)) (Term Syntax)
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
            fv :: Term Syntax -> Set (Variable Syntax)
            fv t = fvAux t empty

            fvAux :: Term Syntax
                  -> Set (Variable Syntax)
                  -> Set (Variable Syntax)
            fvAux (Var v) bound | v `member` bound = empty
                                | otherwise        = singleton v
            fvAux (App t1 t2) bound = fvAux t1 bound `union` fvAux t2 bound
            fvAux (Lambda v t) bound = fvAux t $ bound `union` singleton v

    aName :: Term Syntax -> Term Syntax
    aName t = runReader (aNameAux t) empty
      where
        aNameAux
            :: Term Syntax -> Reader (Set (Variable Syntax)) (Term Syntax)
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

        subs :: Term Syntax
             -> Variable Syntax
             -> Term Syntax
             -> Reader (Set (Variable Syntax)) (Term Syntax)
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
            fv :: Term Syntax -> Set (Variable Syntax)
            fv t = fvAux t empty

            fvAux :: Term Syntax
                  -> Set (Variable Syntax)
                  -> Set (Variable Syntax)
            fvAux (Var v) bound | v `member` bound = empty
                                | otherwise        = singleton v
            fvAux (App t1 t2) bound = fvAux t1 bound `union` fvAux t2 bound
            fvAux (Lambda v t) bound = fvAux t $ bound `union` singleton v

{-  Leroy abstract machine for the weak call-by-value lambda calculus.
    Refer to publication "Distilling Abstract Machines", 2014, Accattoli,
    Barenbaum and Mazza, for the underlying theory.
-}
instance AM.AbstractMachine Syntax CbVState where
    initialState :: Term Syntax -> CbVState
    initialState t = CbVState {term = t, stack = [], env = CbVEnv []}

    step :: CbVState -> Maybe CbVState
    {-       (t u,               s, e)
        ->c1 (  u, Func (t, e) : s, e)
    -}
    step st@CbVState{ term = App t u
                    , stack   = s
                    , env     = e
                    }
        = Just st{ term    = u
                 , stack      = Func (t, e) : s
                 , env        = e
                 }
    {-       (Lv.t',   Func (u, e') : s,  e)
        ->c2 (    u, Arg (Lv.t', e) : s, e')
    -}
    step st@CbVState{ term = t@(Lambda v t')
                    , stack   = Func (u, e') : s
                    , env     = e
                    }
        = Just st{ term    = u
                 , stack      = Arg (t, e) : s
                 , env        = e'
                 }
    {-       (Lv.t', Arg (u, e') : s,                  e)
         ->m (   t',               s, [v <- (u, e')] : e)
    -}
    step st@CbVState{ term = t@(Lambda v t')
                    , stack   = Arg (u, e') : s
                    , env     = CbVEnv e
                    }
        = Just st{ term    = t'
                 , stack      = s
                 , env        = CbVEnv $ (v, (u, e')) : e
                 }
    {-      (x, s, e1 ++ [ x <- (t, e')] ++ e2)
        ->e (t, s,                          e')
    -}
    step st@CbVState{ term = Var v
                    , stack   = s
                    , env     = CbVEnv e
                    }
        = do (t, e') <- findSubs v e
             Just st{ term = t
                    , stack   = s
                    , env     = e'
                    }
      where
        findSubs :: String
                 -> [(String, (Term Syntax, CbVEnv))]
                 -> Maybe (Term Syntax, CbVEnv)
        findSubs name e = snd <$> find ((==) name . fst) e

    step                              _ = Nothing

    -- _(t, s, e)_ := _s_ < _e_ < t > >
    decodeStateToTerm :: CbVState -> Term Syntax
    decodeStateToTerm CbVState{..} = decodeStack stack (decodeEnv env term)
      where
        {-  _[]_              <t> := t
            _([x <- c] : ee)_ <t> := _e_ < t {x <- _c_} >
        -}
        decodeEnv ::  CbVEnv -> Term Syntax -> Term Syntax
        decodeEnv (CbVEnv [])           t = t
        decodeEnv (CbVEnv ((v, c) : e)) t =
            decodeEnv (CbVEnv e) (substitute t v (decodeClosure c))

        {-  _[]_            <t> := t
            _(Func c : ss)_ <t> := _ss_ < _c_ t>
            _(Arg  c : ss)_ <t> := _ss_ < t _c_>
        -}
        decodeStack :: Stack -> Term Syntax -> Term Syntax
        decodeStack            [] t = t
        decodeStack (Func c : ss) t = decodeStack ss (App (decodeClosure c) t)
        decodeStack  (Arg c : ss) t = decodeStack ss (App t (decodeClosure c))

        --  _(t, e)_ = _e_ <t>
        decodeClosure :: (Term Syntax, CbVEnv) -> Term Syntax
        decodeClosure (t, e) = decodeEnv e t


-- | Auxiliary values and types.
alphabet :: [Char]
alphabet = ['a'..'z']

digits :: [Char]
digits = ['0'..'9']

data Syntax

data CbVState = CbVState
    { term    :: Term Syntax
    , stack   :: Stack
    , env     :: CbVEnv
    }

instance Show CbVState where
    show CbVState{..} = concat [ "( " ++ unparse term ++ "\n"
                               , ", " ++   show stack ++ "\n"
                               , ", " ++     show env ++ "\n"
                               , ")"
                               ]

type Stack = [StackElemType]

data StackElemType = Arg Closure | Func Closure
  deriving (Eq)

instance Show StackElemType where
    show (Arg  (t, e)) = "(" ++ unparse t ++ ", " ++ show e ++  ")"
    show (Func (t, e)) = "(" ++ unparse t ++ ", " ++ show e ++  ")"

type Closure = (Term Syntax, CbVEnv)

data CbVEnv = CbVEnv [(String, (Term Syntax, CbVEnv))]
  deriving (Eq)

instance Show CbVEnv where
    show (CbVEnv e) = show $ Prelude.map unparseEntry e
      where
        unparseEntry :: (String, (Term Syntax, CbVEnv))
                     -> (String, (String, CbVEnv))
        unparseEntry (s, (t, e)) = (s, (unparse t, e))
