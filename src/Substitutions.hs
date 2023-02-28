module Substitutions
    ( substitute
    , aName
    ) where


import Control.Monad.Reader
import Data.Set

import Parser
import Types


{-| `substitute t x u` implements the substitution corresponding to the
    theoretical notation  "t {x <- u}".
-}
substitute :: Term -> Variable -> Term -> Term
substitute orig var subsFor = runReader (subs orig var subsFor) empty

aName :: Term -> Term
aName t = runReader (aNameAux t) empty
  where
    aNameAux :: Term -> Reader (Set Variable) Term
    aNameAux v@(Var _) = return v
    aNameAux (App t1 t2) =
        do bound <- ask
           return $ App (runReader (aNameAux t1) bound)
                        (runReader (aNameAux t2) bound)
    aNameAux (Lambda v t) =
        do bound <- ask
           if v `member` bound
           then do nVar <- asks newVarName
                   nBody <- local (nVar `insert`) (subs t v (Var nVar))
                   local (nVar `insert`)
                         $ asks $ Lambda nVar . runReader (aNameAux nBody)
           else asks $ Lambda v . runReader (aNameAux t) . (v `insert`)


-- | Auxiliary values.
subs :: Term -> Variable -> Term -> Reader (Set Variable) Term
subs t@(Var v') v u | v' == v   = return u
                    | otherwise = return t
subs (App t1 t2) v u = do
    bound <- ask
    return $ App (runReader (subs t1 v u) bound) (runReader (subs t2 v u) bound)
subs t@(Lambda v' s) v u
    | v == v' = return t
    | v' `member` fv u =
        do nVar <- asks newVarName
           nBody <- local (nVar `insert`) (subs s v' (Var nVar))
           local (nVar `insert`)
                 $ asks $ Lambda nVar . (runReader (subs nBody v u))
    | otherwise = asks $ Lambda v' . (runReader (subs s v u))

fv :: Term -> Set Variable
fv t = fvAux t empty
  where
    fvAux :: Term -> Set Variable -> Set Variable
    fvAux (Var v) bound | v `member` bound = empty
                        | otherwise        = singleton v
    fvAux (App t1 t2) bound = fvAux t1 bound `union` fvAux t2 bound
    fvAux (Lambda v t) bound = fvAux t $ bound `union` singleton v
