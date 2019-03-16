module Hslog.Types where

import Data.List (intercalate)
import Hslog.Misc (validChars)

newtype Atom = Atom String deriving (Eq, Show, Ord)
newtype Var = Var String deriving (Eq, Show, Ord)

-- (Compound { functor :: Atom, arguments :: [Term] })
data Compound = Compound Atom [Term] deriving (Eq, Show)

data Term
  = TAtom Atom
  | TNum Int
  | TVar Var
  | TComp Compound
  deriving (Eq, Show)

data Head = Head Atom [Term] deriving (Eq, Show)
newtype Body = Body Term deriving (Eq, Show)

data Clause
  = Fact Head
  | Rule Head Body
  deriving (Eq, Show)

-- (Functor { fname :: Atom, arity :: Int })
data Funct = Funct Atom Int deriving (Eq, Ord)

instance Show Funct where
  show (Funct nameAtom len) = gen nameAtom ++ "/" ++ show len

newtype Query = Query Term deriving (Eq, Show)

class Generator a where
  gen :: a -> String

instance Generator Atom where
  gen (Atom name) =
    let (hd : rest) = name in
    if hd `elem` ['a'..'z'] && all (`elem` validChars) rest
    then name
    else "'" ++ name ++ "'"

instance Generator Var where
  gen (Var name) = name

instance Generator Compound where
  gen (Compound f args) =
    gen f ++ "(" ++ (intercalate ", " . map gen) args ++ ")"

instance Generator Term where
  gen (TAtom a) = gen a
  gen (TNum n) = show n
  gen (TVar v) = gen v
  gen (TComp c) = gen c

instance Generator Head where
  gen (Head f args) =
    case args of
      [] -> gen f
      _ -> gen (Compound f args)

instance Generator Body where
  gen (Body t) = gen t

instance Generator Clause where
  gen (Fact hd) = gen hd ++ "."
  gen (Rule hd bd) = gen hd ++ " :- " ++ gen bd ++ "."

getClauseHead :: Clause -> Head
getClauseHead (Fact hd) = hd
getClauseHead (Rule hd _) = hd

showTypeOfTerm :: Term -> String
showTypeOfTerm TAtom{} = "(an atom)"
showTypeOfTerm TNum{} = "(a number)"
showTypeOfTerm TVar{} = "(a variable)"
showTypeOfTerm TComp{} = "(a compound term)"
