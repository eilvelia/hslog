module Hslog.Error where

import Hslog.Types

data HslogError
  = UnificationError Term Term
  | UnificationErrorEmpty
  | CallableExpected Term
  | NotSupported Funct
  | UnknownPredicate Funct

type EHslogError = Either HslogError

instance Show HslogError where
  show x = case x of
    UnificationError a b ->
      "UnificationError (" ++ gen a ++ " = " ++ gen b ++ ")"
    UnificationErrorEmpty ->
      "UnificationError"
    CallableExpected t ->
      "Callable expected, but found " ++ gen t
    NotSupported f ->
      show f ++ " is not supported"
    UnknownPredicate f ->
      "Unknown predicate " ++ show f
