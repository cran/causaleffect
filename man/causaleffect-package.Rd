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
Ilya Shpitser and Judea Pearl (2006a) constructed an algorithm for identifying joint interventional distributions in causal models, which contain unobserved variables and induce directed acyclic graphs.
A highly similar algorithm was constructed earlier by Tian and Pearl (2003).
The algorithm of Shpitser and Pearl (2006a) can be seen as a repeated application of the rules of do-calculus and known properties of probabilities,
and it ultimately either derives an expression for the causal distribution or fails to identify the effect, in which case
the effect is unidentifiable. Shpitser and Pearl (2006b) also presented a generalized algorithm for conditional causal effects.
causaleffect provides an implementation of this algorithm.
}

\details{
\tabular{ll}{
Package: \tab causaleffect\cr
Type: \tab Package\cr
Version: \tab 1.1.1\cr
Date: \tab 2015-04-28\cr
License: \tab GPL-2\cr
}
The function \code{causal.effect} receives three character vectors describing the variables of interest and an \code{\link{igraph}} (package \code{igraph}) object as arguments and
returns a string or a list structure describing the interventional distribution if the effect is identifiable.
}
\author{Santtu Tikka <santtuth@gmail.com>}

\references{
Pearl J. 2009 \emph{Causality: Models, Reasoning and Inference}, New York: Cambridge University Press. 

Shpitser I., Pearl J. 2006a Identification of Joint Interventional Distributions in Recursive semi-Markovian Causal Models. \emph{Proceedings of the 21st National Conference on Artificial Intelligence}, \bold{2}, 1219--1226. 

Shpitser I., Pearl J. 2006b Identification of Conditional Interventional Distributions. \emph{Proceedings of the 22nd Conference on Uncertainty in Artificial Intelligence}, 427--444.

Tian J., Pearl J. 2003 On the Identification of Causal Effects. Department of Computer Science, University of California, Los Angeles.
}