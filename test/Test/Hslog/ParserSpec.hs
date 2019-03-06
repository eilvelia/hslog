module Test.Hslog.ParserSpec (spec) where

import Text.Megaparsec
import Text.Megaparsec.Error
import Test.Hspec
import Test.Hspec.Megaparsec

import Hslog.Types
import qualified Hslog.Parser as P

parse' x = parse x "test" -- "test" - filename

spec :: Spec
spec = do
  describe "term" $ do
    it "parse simple atom" $
      parse' P.term "aTm" `shouldParse` TAtom (Atom "aTm")
    it "parse quoted atom" $
      parse' P.term "' at%m'" `shouldParse` TAtom (Atom " at%m")
    it "parse number" $
      parse' P.term "234" `shouldParse` TNum 234
    it "parse variable" $
      parse' P.term "XX" `shouldParse` TVar (Var "XX")
    it "parse variable starting with '_'" $
      parse' P.term "_YY" `shouldParse` TVar (Var "_YY")
    it "parse infix operators" $
      parse' P.term "2= 3 , 4 =5" `shouldParse` TComp (
        Compound (Atom ",")
          [ TComp (Compound (Atom "=")
              [TNum 2, TNum 3])
          , TComp (Compound (Atom "=")
              [TNum 4, TNum 5])])
    it "parse term with parens" $
      parse' P.term "(2=((3,4)))" `shouldParse` TComp (
        Compound (Atom "=")
          [ TNum 2
          , TComp (Compound (Atom ",")
              [TNum 3, TNum 4])])
    it "parse compound term with comments" $
      parse' P.term "a1(b1(c1,/*comment*/ 123) , a2)" `shouldParse` TComp (
        Compound (Atom "a1")
          [ TComp (Compound (Atom "b1") [
              TAtom (Atom "c1")
              , TNum 123])
          , TAtom (Atom "a2") ])
    describe "list notation" $ do
      it "parse empty list" $
        parse' P.term "[]" `shouldParse` TAtom (Atom "[]")
      it "parse list with one element" $
        parse' P.term "[5]" `shouldParse` TComp (
          Compound (Atom ".") [TNum 5,TAtom (Atom "[]")])
      it "parse list with 5 elements" $
        parse' P.term "[a,'b',3,X,5]" `shouldParse` TComp (
          Compound (Atom ".")
            [ TAtom (Atom "a")
            , TComp (Compound (Atom ".")
                [ TAtom (Atom "b")
                , TComp (Compound (Atom ".")
                    [ TNum 3
                    , TComp (Compound (Atom ".")
                        [ TVar (Var "X")
                        , TComp (Compound (Atom ".")
                            [ TNum 5
                            , TAtom (Atom "[]")])])])])])
      it "parse nested lists" $
        parse' P.term "[1,[2]]" `shouldParse` TComp (
          Compound (Atom ".")
            [ TNum 1
            , TComp (Compound (Atom ".")
                [ TComp (Compound (Atom ".")
                    [ TNum 2
                    , TAtom (Atom "[]")])
                , TAtom (Atom "[]")])])
      it "parse tails" $
        parse' P.term "[1|a]" `shouldParse` TComp (
          Compound (Atom ".")
            [ TNum 1
            , TAtom (Atom "a")])
      it "parse list with operators" $
        parse' P.term "[(1=2)]" `shouldParse` TComp (
          Compound (Atom ".")
            [ TComp (Compound (Atom "=")
                [TNum 1,TNum 2])
            , TAtom (Atom "[]")])
  describe "rule" $ do
    it "parse simple rule" $
      parse' P.rule "eq(A, B) :- A = B" `shouldParse`
        Rule
          (Head (Atom "eq") [TVar (Var "A"), TVar (Var "B")])
          (Body (TComp (Compound (Atom "=")
            [ TVar (Var "A")
            , TVar (Var "B")])))
    it "parse rule with empty compound as head" $
      parse' P.rule "r() :- rr()" `shouldParse`
        Rule
          (Head (Atom "r") [])
          (Body (TComp (Compound (Atom "rr") [])))
    it "parse rule with atom as head" $
      parse' P.rule "r :- rr()" `shouldParse`
        Rule
          (Head (Atom "r") [])
          (Body (TComp (Compound (Atom "rr") [])))
  describe "clause" $ do
    it "parse fact with '.' at the end" $
      parse' P.clause "fact." `shouldParse`
        Fact (Head (Atom "fact") [])
    it "should not parse clause without '.' at the end" $
      parse' P.clause `shouldFailOn` "fact"
  describe "query" $
    it "parse simple query" $
      parse' P.query "X = Y." `shouldParse` Query (
        TComp (Compound (Atom "=") [TVar (Var "X"), TVar (Var "Y")]))
  describe "file" $ do
    it "parse knowledge base with simple clauses" $
      let
        kb = unlines [
          "happy(vincent).",
          "listens2Music(butch). % Comment",
          "playsAirGuitar(vincent):-",
          "   listens2Music(vincent),",
          "   happy(vincent).",
          "playsAirGuitar(butch) :-",
          "   happy(butch).",
          "playsAirGuitar(butch):- ",
          "   listens2Music(butch).",
          ""
          ]
      in
      parse' P.file `shouldSucceedOn` kb
    it "should not parse invalid KB" $
      let kb = "happy(vincent). abcde" in
      parse' P.file `shouldFailOn` kb
