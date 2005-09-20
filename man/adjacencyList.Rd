
\name{adjacencyList}

\alias{adjacencyList}
\alias{adjacencyList<-}

\title{Adjacency list representation of a graph}

\description{Retrieve or set the adjacency list representation of a graph.}

\usage{adjacencyList(object, ...)
adjacencyList(x, force = TRUE) <- value}

\arguments{
  \item{object}{a graph object from which the representation should be retrieved}
  \item{\dots}{additional parameters to be used when retrieving the representation}
  \item{x}{a graph object in which the representation should be set}
  \item{force}{a logical value telling whether the representation should be set even if this amounts to changing the graph}
  \item{value}{an object of class \code{"adjacencyList"} containing the representation to be set}
}

\details{The functions \code{adjacencyList} and \code{adjacencyList<-} are generic.}

\value{The function \code{adjacencyList} returns an object of  class \code{"adjacencyList"}
containing the adjacency list representation to be retrieved. The function \code{adjacencyList<-}
returns a graph object in which the adjacency list representation has been set.}

% \references{ ... }

\author{Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca}

% \note{ ~~further notes~~ }

% \section{Warning }{....} ~

\seealso{\code{\link{adjacencyList-class}} and \code{\link{multiGraph-class}}}

% \examples{}

\keyword{ methods }
