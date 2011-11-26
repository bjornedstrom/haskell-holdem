% This work is licensed under a Creative Commons
% Attribution-ShareAlike 3.0 Unported License.
% Copyright (c) Björn Edström <be@bjrn.se> 2011

\section{Cards, Poker Hands, Classifications and Rankings}

Texas Hold 'em is a Poker game played with a regular ``french'' deck
of 52 playing cards. This chapter will explain how individual poker
hands (of five cards) are valued relative to other poker hands. In
Texas Hold 'em as all other similar Poker games, the best hand wins
(or ties with a similar hand).

This chapter defines a module Card that expose an abstraction of a
playing card and functions that classifies hands (such as deciding
that a hand is a ``straight flush'' and creating metrics so a
classified hand can be compared with another classified hand).

\begin{code}
module HaskellHoldEm.Card
    (
      Card (..)
    , Rank (..)
    , Suit (..)
    , HandClass (..)
    , classify
    , scoreHand
    ) where

import Data.Maybe
import List
\end{code}

First of all we create data structures for a single playing card.

\begin{code}
data Suit = Spades
          | Hearts
          | Diamonds
          | Clubs
          deriving (Eq)

data Rank = Ace
          | King
          | Queen
          | Jack
          | Rank Int -- 2,3,4,5,6,7,8,9

data Card = Card {
      getRank :: Rank
    , getSuit :: Suit
    }
\end{code}

In Texas Hold 'em, a cards suit does not have a value. Equivilance is
important (for deciding a ``flush'') but ordering is not - we cannot
say that, for instance, a Heart is worth more than Diamonds.

Rank however have both an ordering relation and equivalence. We will
define those explicitly.

\begin{code}
rankToInt :: Rank -> Int
rankToInt Jack = 11
rankToInt Queen = 12
rankToInt King = 13
rankToInt Ace = 14
rankToInt (Rank n) = n

intToRank :: Int -> Rank
intToRank 14 = Ace
intToRank 13 = King
intToRank 12 = Queen
intToRank 11 = Jack
intToRank n = Rank n

instance Eq Rank where
    a == b = (rankToInt a) == (rankToInt b)

instance Ord Rank where
    compare a b = compare (rankToInt a) (rankToInt b)
\end{code}

It is convenient to have the two functions rankToInt and intToRank
when working with classification and scoring.

\subsection{Hand Classification}

All poker hands can be classified as one of 9 different n-tuples. A
simple example is the Straight Flush, which can be defined as the
2-tuple (StraightFlush, h) where h is the highest card Rank. This is
all the information required to create a scoring for this particular
hand, throwing away information that is not necessary, such as the
suit of the cards.

The different n-tuples, ordered by ranking, are:

(straight flush, highest card)
(four of a kind, card in four of a kind, kicker)
(full house, card in three of a kind, card in two of a kind)
(flush, highest card 1, highest card 2, ... highest card 5)
(straight, highest card)
(three of a kind, card in three of a kind, kicker 1, kicker 2)
(two pair, card in best pair, card in second best pair, kicker)
(one pair, card in pair, kicker 1, kicker 2, kicker 3)
(highest card 1, highest card 2, ..., highest card 5)

``Kicker'' is Poker terminology for the stray cards used to handle
ties. For example, if two players have four-of-a-kind, the fifth card
that is not part f the four is known as the kicker. Highest kicker
wins.

Using this information, we define the data type HandClass:
\begin{code}
data HandClass = StraightFlush Rank
               | FourOfAKind Rank Rank
               | FullHouse Rank Rank
               | Flush Rank Rank Rank Rank Rank
               | Straight Rank
               | ThreeOfAKind Rank Rank Rank
               | TwoPair Rank Rank Rank
               | OnePair Rank Rank Rank Rank
               | HighestCard Rank Rank Rank Rank Rank
               deriving (Eq, Show)
\end{code}

It is a bit messy at the moment, the field names have the "reasonable"
meaning: for example in the TwoPair case the three Ranks are,
respectively: The rank of the best pair, the rank of the second best
pair, and the kicker.

Before we implement the classify function, the mapping from a list of
Card to a HandClass, we create a helper function to classify
straights. Straights are tricky because an Ace can be treated as a
low, if the other cards are [2,3,4,5], or high, if the other cards are
[10,Jack,Queen,King].

\begin{code}
hasStraight :: [Rank] -> Maybe Rank
hasStraight rank
    | aceHigh == [minimum aceHigh..maximum aceHigh]
        = Just . intToRank . maximum $ aceHigh
    | aceLow == [minimum aceLow..maximum aceLow]
        = Just . intToRank . maximum $ aceLow
    | otherwise  = Nothing
    where
      aceHigh = map rankToInt rank
      rankToInt2 r = if r == Ace then 1 else rankToInt r
      aceLow = sort $ map rankToInt2 rank
\end{code}

We let the hasStraight function be implemented with a Maybe return
value, since the hand may not be a straight at all. If it is, it
returns the highest ranking card in the straight, needed for a full
classification according to the 2-tuple defined above.

Finally, using the hasStraight function we can define the classify
function, as follows:

\begin{code}
classify :: [Card] -> HandClass
classify hand
    | isFlush && isJust straight = StraightFlush $ fromJust straight
    | any (len 4) groups = FourOfAKind (head $ pairs 4)
                                       (head kickers)
    | any (len 3) groups && any (len 2) groups
        = FullHouse (head $ pairs 3)
                    (head $ pairs 2)
    | isFlush = Flush r1 r2 r3 r4 r5
    | isJust straight = Straight $ fromJust straight
    | any (len 3) groups = ThreeOfAKind (head $ pairs 3)
                                        (maximum kickers)
                                        (minimum kickers)
    | 2 == length (pairs 2) = TwoPair (maximum $ pairs 2)
                                      (minimum $ pairs 2)
                                      (head kickers)
    | any (len 2) groups = OnePair (head $ pairs 2)
                                   (maximum kickers)
                                   (head . tail $ kickers)
                                   (minimum kickers)
    | otherwise = HighestCard r1 r2 r3 r4 r5
    where
      ranks = sort $ map getRank hand
      groups = group ranks
      kickers = pairs 1
      pairs n = nub . sort . flatten . filter (len n) $ groups
      (r5:r4:r3:r2:r1:_) = ranks
      suits = map getSuit hand
      isFlush = all (== (head suits)) suits
      straight = hasStraight ranks
      len n list = length list == n
\end{code}

This concludes the definition of classify.

\subsection{Scoring}

Once we have classified a hand to a HandClass, we can score it. First
we set up some preliminaries:

\begin{code}
data Score = Score Int deriving (Eq, Ord, Show)
\end{code}

We define the data type Score to indicate a scoring: scores can be
compared and ordered, but nothing else.

\begin{code}
-- |Flatten a list of lists
flatten = foldr (++) []

listToScore :: [Int] -> Score
listToScore list = Score $ sum $ zipWith (*) [15^5, 15^4, 15^3, 15^2, 15^1, 15^0] list
\end{code}

The idea with the scoring is to treat the n-tuples as a number in
base-15, which is sufficient since the highest ranking card is 14. The
type of hand, such as straight flush or full house, is the most
significant digit in this base-15 number. The least significant is
usually the last kicker.

The implementation of scoreHand is thus:

\begin{code}
-- |Score a hand of 5 cards. Four of a Kind will have a higher score
-- than Flush, for example.
scoreHand :: HandClass -> Score
scoreHand (StraightFlush highest)        = listToScore $ [8] ++ map rankToInt [highest]
scoreHand (FourOfAKind fourRank k)       = listToScore $ [7] ++ map rankToInt [fourRank,k]
scoreHand (FullHouse threeRank twoRank)  = listToScore $ [6] ++ map rankToInt [threeRank,twoRank]
scoreHand (Flush h1 h2 h3 h4 h5)         = listToScore $ [5] ++ map rankToInt [h1,h2,h3,h4,h5]
scoreHand (Straight highest)             = listToScore $ [4] ++ map rankToInt [highest]
scoreHand (ThreeOfAKind threeRank k1 k2) = listToScore $ [3] ++ map rankToInt [threeRank,k1,k2]
scoreHand (TwoPair p1Rank p2Rank k)      = listToScore $ [2] ++ map rankToInt [p1Rank,p2Rank,k]
scoreHand (OnePair pairRank k1 k2 k3)    = listToScore $ [1] ++ map rankToInt [pairRank,k1,k2,k3]
scoreHand (HighestCard h1 h2 h3 h4 h5)   = listToScore $ [0] ++ map rankToInt [h1,h2,h3,h4,h5]
\end{code}

\subsection{Pretty Printing}

We conclude this module by implementing some helper functions to
pretty print hands.

\begin{code}
instance Show Suit where
    show Spades = "s"
    show Hearts = "h"
    show Diamonds = "d"
    show Clubs = "c"

instance Show Rank where
    show (Rank 10) = "T"
    show (Rank n) = show n
    show Ace = "A"
    show King = "K"
    show Queen = "Q"
    show Jack = "J"

instance Show Card where
    show (Card rank suit) = show rank ++ show suit
\end{code}

% ----- Configure Emacs -----
% Local Variables: ***
% mode: latex ***
% mmm-classes: literate-haskell-latex ***
% End: ***
