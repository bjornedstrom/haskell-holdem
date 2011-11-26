% This work is licensed under a Creative Commons
% Attribution-ShareAlike 3.0 Unported License.
% Copyright (c) Björn Edström <be@bjrn.se> 2011

\begin{code}
import HaskellHoldEm.Card
\end{code}

\begin{code}
hand = [Card Ace Hearts,
        Card King Clubs,
        Card Queen Diamonds,
        Card Jack Spades,
        Card (Rank 10) Hearts]

main :: IO ()
main = putStrLn . show . scoreHand . classify $ hand
\end{code}

% ----- Configure Emacs -----
%
% Local Variables: ***
% mode: latex ***
% mmm-classes: literate-haskell-latex ***
% End: ***
