module Test.Hslog.ParserSpec (spec) where

import Text.Megaparsec
import Test.Hspec
import Test.Hspec.Megaparsec

import Hslog.Types
import qualified Hslog.Parser as P

parse' x = parse x "test" -- "test" - filename

spec :: Spec
spec =
  it "term" $
    parse' P.term "a1(b1(c1,/*comment*/ 123), a2)" `shouldParse` TComp (
      Compound
        (Atom "a1")
        [ TComp (Compound (Atom "b1") [TAtom (Atom "c1"), TNum 123])
        , TAtom (Atom "a2") ]
    )
