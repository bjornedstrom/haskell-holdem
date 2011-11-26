% This work is licensed under a Creative Commons
% Attribution-ShareAlike 3.0 Unported License.
% Copyright (c) Björn Edström <be@bjrn.se> 2011

\begin{code}
import System.Random
import System.Random.Shuffle
import HaskellHoldEm.Card
\end{code}

\begin{code}
unshuffled :: [Card]
unshuffled = [Card rank suit |
              suit <- [Spades,Hearts,Diamonds,Clubs],
              rank <- [Ace,King,Queen,Jack,
                       Rank 10,Rank 9,Rank 8,Rank 7,Rank 6,
                       Rank 5,Rank 4,Rank 3,Rank 2]]

-- |Return a shuffled list of cards
deck :: (RandomGen gen) => gen -> [Card]
deck gen = shuffle' unshuffled 52 gen

main :: IO ()
main = putStrLn . show $ (deck (mkStdGen 12321))
\end{code}

% ----- Configure Emacs -----
%
% Local Variables: ***
% mode: latex ***
% mmm-classes: literate-haskell-latex ***
% End: ***
