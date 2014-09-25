\name{causaleffect-package}
\alias{causaleffect-package}
\alias{causaleffect}
\docType{package}
\title{Deriving Expressions of Joint Interventional Distributions in Causal Models}

\description{
Causal calculus is concerned with estimating the interventional distribution of some action from the observed
joint probability distribution of the variables in a given causal structure.
All identifiable causal effects can be derived using the rules of do-calculus, but
the rules themselves do not give any direct indication whether the effect in question is identifiable or not.
Ilya Shpitser and Judea Pearl (2006) constructed an algorithm for identifying joint interventional distributions in causal models, which contain unobserved variables and induce directed acyclic graphs.
This algorithm can be seen as a repeated application of the rules of do-calculus and known properties of probabilities,
that ultimately either derives an expression for the causal distribution, or fails to identify the effect, in which case
the effect is non-identifiable. causaleffect provides an implementation of this algorithm.
}

\details{
\tabular{ll}{
Package: \tab causaleffect\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-09-17\cr
License: \tab GPL-2\cr
}
The function \code{causal.effect} receives two character vectors describing the variables of interest and an \code{\link{igraph}} (package \code{igraph}) object as arguments and
returns a string describing the interventional distribution in LaTeX syntax if the effect is identifiable.
}
\author{Santtu Tikka <santtuth@gmail.com>}

\references{
Pearl J. 2009 \emph{Causality: Models, Reasoning and Inference}, New York: Cambridge University Press. 

Shpitser I., Pearl J. 2006 Identification of Joint Interventional Distributions in Recursive semi-Markovian Causal Models. \emph{Proceedings of the 21st National Conference on Artificial Intelligence}, \bold{2}, 1219--1226. 
}