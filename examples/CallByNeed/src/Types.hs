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
    , CbNeedState (..)
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
        | ES (Term Syntax) (Variable Syntax) (Term Syntax)
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
    unparse (ES t x s) = unparse t ++ " [" ++ x ++ " <- " ++ unparse s ++ "]"

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
        aNameAux (App t1 t2) = do bound <- ask
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

{-  The MAD (Milner Abstract machine by-neeD). Refer to publication "Distilling
    Abstract Machines", 2014, Accattoli, Barenbaum and Mazza, for the underlying
    theory.
-}
instance AM.AbstractMachine Syntax CbNeedState where
    initialState :: Term Syntax -> CbNeedState
    initialState t = CbNeedState { term  = t
                                 , stack = mempty
                                 , dump  = mempty
                                 , env   = mempty
                                 }

    step :: CbNeedState -> Maybe CbNeedState
    {-       (t u,     s, D, E)
        ->c1 (  t, u : s, D, E)
    -}
    step st@CbNeedState{ term  = App t u
                       , stack = s
                       , dump  = d
                       , env   = e
                       }
        = Just st{ term        = t
                 , stack       = u : s
                 , dump        = d
                 , env         = e
                 }

    {-       (x,  s,              D, E' ++ [x <- t] ++ E'')
        ->c2 (t, [], (E', x, s) : D,                   E'')
    -}
    step st@CbNeedState{ term  = Var x
                       , stack = s
                       , dump  = d
                       , env   = e
                       }
        = do (e', t, e'') <- findSubs [] e
             Just st{ term     = t
                    , stack    = mempty
                    , dump     = mkDumpElem e' x s : d
                    , env      = e''
                    }
      where
        findSubs :: Env -> Env -> Maybe (Env, Term Syntax, Env)
        findSubs _              [] = Nothing
        findSubs e' ((y, u) : e'') | x == y = Just (e', u, e'')
                                   | otherwise = findSubs (e' ++ [(y, u)]) e''

    {-       (Lx.t, u : s, D,            E)
         ->m (   t,     s, D, [x <- u] : E)
    -}
    step st@CbNeedState{ term  = Lambda x t
                       , stack = u : s
                       , dump  = d
                       , env   = e
                       }
        = Just st{ term        = t
                 , stack       = s
                 , dump        = d
                 , env         = (x, u) : e
                 }

    {-      (    Ly.t, [], (E', x, s) : D, E'')
        ->e ((Ly.t)^α,  s,              D, E' ++ [x <- Ly.t] ++ E'')

        Note that, according to the underlying theory and the definition of
        aName given above, α-renaming t' for aName t' is not correct. A proper
        definition for aName requires α-renaming in such a way that the
        resulting term is well-named--c.f. the "Distilling Abstract Machines"
        publication.
    -}
    step st@CbNeedState{ term  = t'@(Lambda y t)
                       , stack = mempty
                       , dump  = DumpElem {dEnv = e', dVar = x, dStack = s} : d
                       , env   = e''
                    }
        = Just st{ term        = aName t'
                 , stack       = s
                 , dump        = d
                 , env         = e' ++ [(x, t')] ++ e''
                 }

    step                              _ = Nothing

    -- _(t, s, D, E)_ := _E_ < _D_ < _s_ < t > > >
    decodeStateToTerm :: CbNeedState -> Term Syntax
    decodeStateToTerm CbNeedState{..} =
        decodeEnv env $ decodeDump dump $ decodeStack stack term
      where
        {-  _[]_              <t> := t
            _([x <- c] : ee)_ <t> := _e_ < t {x <- _c_} >
        -}
        decodeEnv :: Env -> Term Syntax -> Term Syntax
        decodeEnv                   [] t = t
        decodeEnv ((name, sub) : subs) t = decodeEnv subs $ ES t name sub

        --  _(t, e)_ = _e_ <t>
        decodeDump :: Dump -> Term Syntax -> Term Syntax
        decodeDump [] t = t
        decodeDump (DumpElem{..} : ee) t =
          ES (decodeEnv dEnv $ decodeDump ee $ decodeStack dStack $ Var dVar)
             dVar
             t

        {-  _[]_            <t> := t
            _(Func c : ss)_ <t> := _ss_ < _c_ t>
            _(Arg  c : ss)_ <t> := _ss_ < t _c_>
        -}
        decodeStack :: Stack -> Term Syntax -> Term Syntax
        decodeStack       [] t = t
        decodeStack (a : aa) t = decodeStack aa $ App t a


-- | Auxiliary values and types.
alphabet :: [Char]
alphabet = ['a'..'z']

digits :: [Char]
digits = ['0'..'9']

data Syntax

data CbNeedState = CbNeedState
    { term    :: Term Syntax
    , stack   :: Stack
    , dump    :: Dump
    , env     :: Env
    }

instance Show CbNeedState where
    show CbNeedState{..} = concat [ "( "
                                  , unparse term
                                  , "\n, Stack: "
                                  , unparseStack stack
                                  , "\n, Dump: "
                                  , show dump
                                  , "\n, Environment: "
                                  , unparseEnv env
                                  , "\n)"
                                  ]

unparseStack :: Stack -> String
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

unparseEnv :: Env -> String
unparseEnv env = concat [ "["
                        , unparseEnvAux
                        , "]"
                        ]
  where
    unparseEnvAux = case env of
      []            -> ""
      [(x, t)]      -> concat [ "["
                              , x
                              , " <- "
                              , unparse t
                              , "]"
                              ]
      ((x, t) : ss) -> concat [ "["
                              , x
                              , " <- "
                              , unparse t
                              , "]"
                              , ", "
                              , unparseEnv ss
                              ]


type Stack = [Term Syntax]

type Dump = [DumpElem]

data DumpElem = DumpElem
    { dEnv   :: Env
    , dVar   :: String
    , dStack :: Stack
    }
  deriving (Eq)

instance Show DumpElem where
  show DumpElem{..} = concat [ "("
                             , unparseEnv dEnv
                             , ", "
                             , dVar
                             , ", "
                             , unparseStack dStack
                             , ")"
                             ]

mkDumpElem :: Env -> String -> Stack -> DumpElem
mkDumpElem = DumpElem

type Env = [(String, Term Syntax)]
