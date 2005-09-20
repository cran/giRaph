
\name{wrappers}

\alias{v}
\alias{u}
\alias{d}
\alias{r}

\title{Short-hands for vertex set and edge construction}

\description{Provide short-hands for vertex set and edge construction.}

\usage{
v(...)
u(...)
d(...)
r(...)
}

\arguments{
  \item{\dots}{unique vertex identifiers (should be characters for \code{v}
  and strictly positive integers for \code{u},\code{d} and \code{r})}
}

\details{Function \code{d} builds a directed edge from tail to head,
function \code{r} builds a directed edge from head to tail.}

\value{
Function \code{v} returns an object of  class \code{"vertexSet"}.
Function \code{u} returns an object of  class \code{"undirectedEdge"}.
Both functions \code{d} and \code{r} return an object of  class \code{"directedEdge"}.
}

% \references{ ... }

\author{Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca}

% \note{ ~~further notes~~ }

% \section{Warning }{....} ~

\seealso{
\code{\link{vertexSet-class}},
\code{\link{undirectedEdge-class}} and
\code{\link{directedEdge-class}}.
}

% \examples{}

\keyword{methods}
