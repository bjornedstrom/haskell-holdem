% This work is licensed under a Creative Commons
% Attribution-ShareAlike 3.0 Unported License.
% Copyright (c) Björn Edström <be@bjrn.se> 2011

\begin{code}
module TestCard (tests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Test.QuickCheck
import Test.HUnit

import HaskellHoldEm.Card as Card
\end{code}

\begin{code}
test_classify1 = Card.classify [Card Ace Hearts, Card King Clubs, Card Queen Diamonds, Card Jack Spades, Card (Rank 10) Hearts] @?= Straight Ace
-- TODO: Add more test cases.

tests = [
          testGroup "Card" [
              testCase "test_classify1" test_classify1
            ]

        ]
\end{code}

% ----- Configure Emacs -----
%
% Local Variables: ***
% mode: latex ***
% mmm-classes: literate-haskell-latex ***
% End: ***
