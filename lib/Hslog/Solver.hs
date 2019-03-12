module Hslog.Solver where

import Hslog.Types
import Hslog.Error
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Functor ((<&>))
import Data.Functor.Identity
import Data.Bifunctor
import Data.List
import Data.Maybe (fromMaybe)
import Data.Either (partitionEithers)
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (Map)

import Debug.Trace (trace)

type Substitutions = Map Var Term
type Replacements = Map Var Var

unify :: Substitutions -> Term -> Term -> Maybe Substitutions
unify s t1 t2 | trace
  ("--unify '" ++ show s ++ "' " ++ gen t1 ++ " " ++ gen t2) False = undefined
unify substs term1 term2 =
  case (apply substs term1, apply substs term2) of
    (t1, t2)
      | t1 == t2 -> Just substs
    (TVar v, t) ->
      Just $ Map.insert v t substs
    (t, TVar v) ->
      Just $ Map.insert v t substs
    (TComp (Compound f1 args1), TComp (Compound f2 args2))
      | f1 /= f2 -> Nothing
      | length args1 /= length args2 -> Nothing
      | otherwise ->
        foldM (\s (t1, t2) -> unify s t1 t2) substs (zip args1 args2)
    _ -> Nothing

-- Returns 'EHslogError x' instead of 'Maybe x'
unifyE :: Substitutions -> Term -> Term -> EHslogError Substitutions
unifyE s a b = case unify s a b of
  Just s' -> Right s'
  Nothing -> Left $ UnificationError a b

unifyHeads :: Substitutions -> Head -> Head -> EHslogError Substitutions
unifyHeads substs (Head f1 args1) (Head f2 args2) =
  unifyE substs (TComp (Compound f1 args1)) (TComp (Compound f2 args2))

apply :: Substitutions -> Term -> Term
apply substs t =
  case t of
    TAtom {} -> t
    TNum {} -> t
    (TVar var) ->
      case Map.lookup var substs of
        Just t' -> apply substs t'
        Nothing -> t
    (TComp (Compound fname args)) ->
      TComp (Compound fname (map (apply substs) args))

applyArgs :: Substitutions -> [Term] -> Substitutions
applyArgs s a | trace
  ("(-applyArgs " ++ show s ++ " " ++ show a) False = undefined
applyArgs substs args = foldl f Map.empty args
  where
    f :: Substitutions -> Term -> Substitutions
    f acc arg =
      case arg of
        TAtom {} -> acc
        TNum {} -> acc
        TVar v -> Map.insert v (apply substs arg) acc
        TComp (Compound _ terms) -> foldl f acc terms

applyHead :: Substitutions -> Head -> Head
applyHead substs (Head f args) =
  Head f (map (apply substs) args)

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

getClauses :: KnowledgeBase -> Head -> EHslogError [Clause]
getClauses (KB kbm) hd@(Head fname args) =
  let f = Funct fname (length args) in
  case Map.lookup f kbm of
    Just cls -> Right cls
    Nothing -> Left $ UnknownPredicate f

combineSubsts :: Substitutions -> Substitutions -> EHslogError Substitutions
combineSubsts s1 s2 | trace
  ("&-combine " ++ show s1 ++ " | " ++ show s2) False = undefined
combineSubsts s1 s2 =
  foldM f s1 (Map.toList s2)
  where
    f :: Substitutions -> (Var, Term) -> EHslogError Substitutions
    f substs (var, term) =
      case Map.lookup var substs of
        Just t -> unifyE substs t term
        Nothing -> Right $ Map.insert var term substs

type Seq = Int

createTempVar :: Seq -> Var
createTempVar seq = Var $ "#temp" ++ show seq

renameVariables :: Monad m => Clause -> StateT Seq m Clause
renameVariables cl = getVarReplacements cl <&> replaceVars cl

replaceVars :: Clause -> Replacements -> Clause
replaceVars cl repls =
  case cl of
    Fact h -> Fact (inHead h)
    Rule h b -> Rule (inHead h) (inBody b)
  where
    inHead (Head a ts) = Head a (map inTerm ts)
    inBody (Body t) = Body (inTerm t)
    inTerm t =
      case t of
        (TVar v1) ->
          case Map.lookup v1 repls of
            Just v2 -> TVar v2
            Nothing -> t
        (TComp (Compound fname args)) ->
          TComp (Compound fname (map inTerm args))
        _ -> t

getVarReplacements :: Monad m => Clause -> StateT Seq m Replacements
getVarReplacements cl =
  case cl of
    Fact h -> inHead h
    Rule h b -> Map.union <$> inHead h <*> inBody b
  where
    inHead (Head a ts) = mapM inTerm ts <&> Map.unions
    inBody (Body t) = inTerm t

    inTerm :: Monad m => Term -> StateT Seq m Replacements
    inTerm (TVar v) = do
      seq <- get
      put (seq + 1)
      return $ Map.singleton v (createTempVar seq)
    inTerm (TComp (Compound fname args)) =
      mapM inTerm args <&> Map.unions
    inTerm _ = return Map.empty

convertState :: State s [EHslogError a]
             -> StateT s EHslogError a
             -> State s [EHslogError a]
convertState acc stT = state $ \s ->
  either
    (\err -> first (Left err:) (runState acc s))
    (\(a,s') -> first (Right a:) (runState acc s'))
    (runStateT stT s)

-- TODO: '?- eq(X,Y).' ('eq' defined as 'eq(X,X).')
--       produces 'X = <temp0>, Y = <temp0>', and not 'X = Y'.

solve' :: KnowledgeBase -> Substitutions -> Term
       -> StateT Seq EHslogError Substitutions
solve' _ s t | trace ("|-solve' " ++ show s ++ " " ++ gen t) False = undefined
solve' kb substs term = do
  (fname@(Atom fstr), args) <- lift $ case term of
    TAtom a -> Right (a, [])
    TComp (Compound a terms) -> Right (a, terms)
    t -> Left $ CallableExpected t
  let
    solve'' = solve' kb
    len = length args
    f = Funct fname len
  case (fstr, len) of
    ("=", 2) -> lift $ unifyE substs (args !! 0) (args !! 1)
    (",", 2) -> do
      s' <- solve'' substs (args !! 0)
      solve'' s' (args !! 1)
    (";", 2) -> lift $ Left $ NotSupported f
    _ -> do
      let hd = Head fname (map (apply substs) args)
      cls <- lift $ getClauses kb hd
      let
        eachClause :: Clause -> StateT Seq (Either HslogError) Substitutions
        eachClause cl | trace ("Clause: " ++ show cl) False = undefined
        eachClause cl = do
          newcl <- renameVariables cl
          trace ("New clause: " ++ show newcl) (return ())
          s' <- lift $ unifyHeads Map.empty hd (getClauseHead newcl)
          trace ("unifyHeads success: " ++ show s') (return ())
          case newcl of
            Fact _ ->
              lift $ combineSubsts substs (applyArgs s' args)
            Rule _ (Body t) -> do
              s'' <- solve'' s' t
              trace ("Rule substs: " ++ show s'') (return ())
              lift $ combineSubsts substs (applyArgs s'' args)
        st = foldr (\cl acc -> convertState acc (eachClause cl)) (return []) cls
      StateT $ \s ->
        let (ll, s') = runState st s in
        case partitionEithers ll of
          (_, l@(_:_)) -> Right (Map.unions l, s') -- TODO: return list of substitutions
          ([x], _) -> Left x
          (errs@(_:_:_), _) -> Left (MultipleErrors errs)
          ([], []) -> error "No errors and no solutions"

solve :: KnowledgeBase -> Query -> EHslogError Substitutions
solve kb (Query term) =
  let substs = Map.empty in
  evalStateT (solve' kb substs term) 0

-- Returns substitutions as (Var, Term) list instead of Map.
solveL :: KnowledgeBase -> Query -> EHslogError [(Var, Term)]
solveL kb q = Map.assocs <$> solve kb q
