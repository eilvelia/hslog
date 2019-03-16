module Test.Hslog.SolverSpec (spec) where

import Data.Bifunctor
import Data.Functor ((<&>))
import Text.Megaparsec
import Test.Hspec
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (Map)
import Hslog.Types
import Hslog.Error
import qualified Hslog.Parser as P
import Hslog.Solver

(|>) :: a -> (a -> r) -> r
x |> f = f x

parse' :: String -> String -> Either String (KnowledgeBase, Query)
parse' kbstr querystr = do
  kblist <- parse P.file "test_kb" kbstr
    |> first (\err -> "KB ParseError:" ++ parseErrorPretty err)
  ast <- parse P.query "test_query" querystr
    |> first (\err -> "Query ParseError:" ++ parseErrorPretty err)
  return (kbFromList kblist, ast)

shouldSolveWith :: HasCallStack
                => (String, String) -> [[(Var, Term)]] -> Expectation
(kbstr, querystr) `shouldSolveWith` substsL =
  let substs = map Map.fromList substsL in
  either expectationFailure (uncurry shouldBe) $ do
    (kb, ast) <- parse' kbstr querystr
    substs' <- solve kb ast |> first (\err -> "SolveError: " ++ show err)
    return (substs, substs')

shouldEquallySolve :: HasCallStack
                   => (String, String) -> (String, String) -> Expectation
shouldEquallySolve (kbstr1, querystr1) (kbstr2, querystr2) =
  either expectationFailure (uncurry shouldBe) $ do
    (kb1, ast1) <- parse' kbstr1 querystr1
    (kb2, ast2) <- parse' kbstr2 querystr2
    return (solve kb1 ast1, solve kb2 ast2)

shouldFail :: HasCallStack => (String, String) -> Expectation
shouldFail (kbstr, querystr) =
  either expectationFailure return $ do
    (kb, ast) <- parse' kbstr querystr
    either
      (\_ -> Right ())
      (\s -> Left $
        "Expected a SolveError, but received the subsitutions: " ++ show s)
      (solve kb ast)

shouldFailWith :: HasCallStack => (String, String) -> HslogError -> Expectation
(kbstr, querystr) `shouldFailWith` err =
  either expectationFailure (shouldBe err) $ do
    (kb, ast) <- parse' kbstr querystr
    either
      Right
      (\s -> Left $
        "Expected a SolveError, but received the subsitutions: " ++ show s)
      (solve kb ast)

shouldFailWithUniErr :: HasCallStack => (String, String) -> Expectation
shouldFailWithUniErr (kbstr, querystr) =
  either expectationFailure return $ do
    (kb, ast) <- parse' kbstr querystr
    either
      (\_ -> Right ())
      (\s -> Left $
        "Expected a UnificationError, but received the subsitutions: "
        ++ show s)
      (solve kb ast)

tvar :: String -> Term
tvar name = TVar (Var name)

tcomp :: String -> [Term] -> Term
tcomp name args = TComp (Compound (Atom name) args)

tatom :: String -> Term
tatom name = TAtom (Atom name)

unierr = UnificationError

spec :: Spec
spec = do
  let
    kb1 = unlines
      [ "woman(mia)."
      , "woman(jody)."
      , "woman(yolanda)."
      , "playsAirGuitar(jody)."
      , "party." ]
    listKb = unlines
      [ "append([], A, A)."
      , "append([H|T], L, [H|R1]) :- append(T, L, R1)."
      , "member(A, [A|_])."
      , "member(A, [_|Rest]) :- member(A, Rest)." ]
    burgers1Kb = unlines
      [ "burger2(f(X)) :- big_mac(X)."
      , "burger(X) :- big_mac(X)."
      , "big_mac(a)." ]
    burgers2Kb = unlines
      [ "burger2(f(X)) :- big_mac(X)."
      , "big_mac(a)."
      , "big_mac(c)." ]
    burgersKb = unlines
      [ "burger(X) :- big_mac(X)."
      , "burger(X) :- big_kahuna_burger(X)."
      , "burger(X) :- whopper(X)."
      , ""
      , "big_mac(a)."
      , "big_mac(c)."
      , "big_kahuna_burger(b)."
      , "whopper(d)." ]
    kKb = unlines
      [ "f(a)."
      , "f(b)."
      , ""
      , "g(a)."
      , "g(b)."
      , ""
      , "h(b)."
      , ""
      , "k(X) :- f(X), g(X), h(X)." ]
  it "basic unification: 2=2" $
    ("", "2=2.") `shouldSolveWith` [[]]
  it "basic unification: 2=3" $
    ("", "2=3.") `shouldFailWith` unierr (TNum 2) (TNum 3)
  it "basic unification: X=2" $
    ("", "X=2.") `shouldSolveWith` [[(Var "X", TNum 2)]]
  it "'?- X=Y,Y=X.' should result in 'X = Y'" $
    ("", "X=Y,Y=X.") `shouldSolveWith` [[(Var "X", tvar "Y")]]
  it "'?- X=Y,X=Y.' should result in 'X = Y'" $
    ("", "X=Y,X=Y.") `shouldSolveWith` [[(Var "X", tvar "Y")]]
  it "'?- X=X.' -> 'true' (empty substs)" $
    ("", "X=X.") `shouldSolveWith` [[]]
  it "'?- X=Y,Y=Z,Z=4,X=3.' should fail" $
    ("", "X=Y, Y=Z, Z=4, X=3.") `shouldFailWith` unierr (tvar "X") (TNum 3)
  it "'?- X=2,X=Y,Y=3' should fail" $
    ("", "X=2, X=Y, Y=3.") `shouldFailWith` unierr (tvar "Y") (TNum 3)
  it "variable renaming" $
    (burgers1Kb, "burger2(X).") `shouldEquallySolve` ("", "X = f(a).")
  it "'destructuring' in a clause head" $
    (burgers1Kb, "burger2(f(X)).") `shouldEquallySolve` ("", "X = a.")
  it "recursive calls should not cause infinite loop" $
    ("p(c(A)) :- X=f(A),p(X).", "p(c(3)).") |> shouldFailWithUniErr
  it "non-determinism" $
    (kb1, "woman(X).") `shouldSolveWith`
      [ [(Var "X", tatom "mia")]
      , [(Var "X", tatom "jody")]
      , [(Var "X", tatom "yolanda")] ]
  it "non-determinism with destructuring" $
    (burgers2Kb, "burger2(Y).") `shouldSolveWith`
      [ [(Var "Y", tcomp "f" [tatom "a"])]
      , [(Var "Y", tcomp "f" [tatom "c"])] ]
  describe "burgers example" $ do
    it "burger(X) -> X=a;X=c;X=b;X=d." $
      (burgersKb, "burger(X).") `shouldSolveWith`
        [ [(Var "X", tatom "a")]
        , [(Var "X", tatom "c")]
        , [(Var "X", tatom "b")]
        , [(Var "X", tatom "d")] ]
    it "burger(X),big_kahuna_burger(X) -> X=b." $
      (burgersKb, "burger(X),big_kahuna_burger(X).") `shouldSolveWith`
       [ [(Var "X", tatom "b")] ]
  describe "lists: append & member" $ do
    it "append([1,2], [3,4], X) -> X = [1,2,3,4]" $
      (listKb, "append([1,2], [3,4], X).") `shouldEquallySolve`
        ("", "X = [1,2,3,4].")
    it "append(X, [3,4], [1,2,3,4]) -> X = [1,2]" $
      (listKb, "append(X, [3,4], [1,2,3,4]).") `shouldEquallySolve`
        ("", "X = [1,2].")
    it "member(2, [1,2,3]) -> true" $
      (listKb, "member(2, [1,2,3]).") `shouldSolveWith` [[]]
    it "member(5, [1,3,2]) -> false" $
      (listKb, "member(5, [1,3,2]).") |> shouldFailWithUniErr
    it "member(X, [1,2,3]) -> X=1;X=2;X=3" $
      (listKb, "member(X, [1,2,3]).") `shouldSolveWith`
        [ [(Var "X", TNum 1)]
        , [(Var "X", TNum 2)]
        , [(Var "X", TNum 3)] ]
  it "backtracking" $
    (kKb, "k(X).") `shouldSolveWith` [ [(Var "X", tatom "b")] ]
