module Hslog.Solver where

import Hslog.Types
import Hslog.Error
import Control.Monad (foldM)
import Control.Applicative
import Data.Bifunctor
import Data.List
import Data.Maybe (fromMaybe, isJust, fromJust)
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (Map)

import Debug.Trace (trace)

type Substitutions = Map Var Term

unify :: Substitutions -> Term -> Term -> Maybe Substitutions
unify s t1 t2 | trace
  ("--unify '" ++ show s ++ "' " ++ gen t1 ++ " " ++ gen t2) False = undefined
unify substs term1 term2 =
  case (term1, term2) of
    (TAtom a1, TAtom a2)
      | a1 == a2 -> Just substs
      | otherwise -> Nothing
    (TNum n1, TNum n2)
      | n1 == n2 -> Just substs
      | otherwise -> Nothing
    (TVar v1, t2) ->
      unifyVar v1 t2
    (t1, TVar v2) ->
      unifyVar v2 t1
    (TComp (Compound f1 args1), TComp (Compound f2 args2))
      | f1 /= f2 -> Nothing
      | length args1 /= length args2 -> Nothing
      | otherwise ->
        foldM (\s (t1, t2) -> unify s t1 t2) substs (zip args1 args2)
    _ -> Nothing
  where
    unifyVar :: Var -> Term -> Maybe Substitutions
    unifyVar v1 (TVar v2) | v1 == v2 = Just substs
    unifyVar var term =
      case Map.lookup var substs of
        -- Just (TVar v) -> Just $ Map.insert v term substs
        Just (TVar v) -> unifyVar v term
        Just t -> unify substs t term
        Nothing -> Just $ Map.insert var term substs

-- Returns 'EHslogError x' instead of 'Maybe x'
unifyE :: Substitutions -> Term -> Term -> EHslogError Substitutions
unifyE s a b = case unify s a b of
  Just s' -> Right s'
  Nothing -> Left $ UnificationError a b

unifyHeads :: Substitutions -> Head -> Head -> Maybe Substitutions
unifyHeads substs (Head f1 args) (Head f2 args2) =
  unify substs (TComp (Compound f1 args)) (TComp (Compound f2 args2))

newtype KnowledgeBase = KB (Map Funct [Clause]) deriving Show

addClauseToKb :: Clause -> KnowledgeBase -> KnowledgeBase
addClauseToKb cl (KB kbm) =
  let
    (Head fname args) = getClauseHead cl
    f = Funct fname (length args)
    cls = fromMaybe [] (Map.lookup f kbm)
  in
  KB $ Map.insert f (cl : cls) kbm

kbFromList :: [Clause] -> KnowledgeBase
kbFromList = foldr addClauseToKb emptyKb

emptyKb :: KnowledgeBase
emptyKb = KB Map.empty

-- Find via unification
findClause
  :: KnowledgeBase -> Substitutions -> Head -> EHslogError (Clause, Substitutions)
findClause (KB kbm) substs hd@(Head fname args) =
  let f = Funct fname (length args) in
  case Map.lookup f kbm of
    Just cls ->
      let unify' cl = unifyHeads substs (getClauseHead cl) hd in
      case find (isJust . unify') cls of
        Just cl -> Right (cl, fromJust $ unify' cl) -- XXX
        Nothing -> Left UnificationErrorEmpty
    Nothing -> Left $ UnknownPredicate f

solve' :: KnowledgeBase -> Substitutions -> Term -> EHslogError Substitutions
solve' kb substs term = do
  (fname@(Atom fstr), args) <- case term of
    TAtom a -> Right (a, [])
    TComp (Compound a terms) -> Right (a, terms)
    t -> Left $ CallableExpected t
  let
    len = length args
    f = Funct fname len
    hd = Head fname args
  case (fstr, len) of
    ("=", 2) -> unifyE substs (args !! 0) (args !! 1)
    (",", 2) -> do
      s' <- solve' kb substs (args !! 0)
      solve' kb s' (args !! 1)
    (";", 2) -> Left $ NotSupported f
    _ -> do
      (cl, s') <- findClause kb substs hd
      case cl of
        Fact _ -> Right s'
        Rule _ (Body t) ->
          solve' kb s' t

solve :: KnowledgeBase -> Query -> EHslogError Substitutions
solve kb (Query term) =
  let substs = Map.empty in
  solve' kb substs term

-- Returns substitutions as (Var, Term) list instead of Map.
solveL :: KnowledgeBase -> Query -> EHslogError [(Var, Term)]
solveL kb = second Map.assocs . solve kb
