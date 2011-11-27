% This work is licensed under a Creative Commons
% Attribution-ShareAlike 3.0 Unported License.
% Copyright (c) Björn Edström <be@bjrn.se> 2011

%% TODO: Perhaps move this to Main.lhs to get a nice polyglot. In that
%% case it will be done when all code is written and in place.

\documentclass{article}

\usepackage[utf8]{inputenc}
\usepackage[english]{babel}
\usepackage{verbatim}
\usepackage{amsmath}

%% TODO: Preferably use lhs2TeX or similar. The problem with lhs2Tex
%% is that it does not handle Haddock style comments very well. For
%% now we will just include code blocks verbatim.
\newenvironment{code}{\footnotesize\verbatim}{\endverbatim\normalsize}

\author{Björn Edström}
\title{Haskell Hold 'em \\ Texas Hold 'em made simple and executable}
\begin{document}

\maketitle

\begin{abstract}
This article will explain the rules of the Texas Hold 'em game with
the Haskell language. A simple computer game is implemented that shows
game state, how betting works and how winners (and losers) are
determined.
\end{abstract}

\tableofcontents

\section{Introduction}

The rules of Texas Hold 'em will be explained with code. Some
assumptions are made, such that the reader has some familiarity with a
poker deck.

\input src/HaskellHoldEm/Card.lhs

\end{document}
