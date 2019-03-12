module Hslog.Error where

import Hslog.Types
import Data.List (intercalate)

data HslogError
  = UnificationError Term Term
  | CallableExpected Term
  | NotSupported Funct
  | UnknownPredicate Funct
  | MultipleErrors [HslogError]
  deriving Eq

type EHslogError = Either HslogError

instance Show HslogError where
  show x = case x of
    UnificationError a b ->
      "UnificationError (" ++ gen a ++ " = " ++ gen b ++ ")"
    CallableExpected t ->
      "Callable expected, but found " ++ gen t ++ " " ++ showTypeOfTerm t
    NotSupported f ->
      show f ++ " is not supported"
    UnknownPredicate f ->
      "Unknown predicate " ++ show f
    MultipleErrors errs ->
      intercalate " | " (map show errs)
