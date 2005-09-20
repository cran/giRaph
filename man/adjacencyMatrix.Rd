
\name{adjacencyMatrix}

\alias{adjacencyMatrix}
\alias{adjacencyMatrix<-}

\title{Adjacency matrix representation of a graph}

\description{Retrieve or set the adjacency matrix representation of a graph.}

\usage{adjacencyMatrix(object, ...)
adjacencyMatrix(x, force = TRUE) <- value}

\arguments{
  \item{object}{a graph object from which the representation should be retrieved}
  \item{\dots}{additional parameters to be used when retrieving the representation}
  \item{x}{a graph object in which the representation should be set}
  \item{force}{a logical value telling whether the representation should be set even if this amounts to changing the graph}
  \item{value}{an object of class \code{"adjacencyMatrix"} containing the representation to be set}
}

\details{The functions \code{adjacencyMatrix} and \code{adjacencyMatrix<-} are generic.}

\value{The function \code{adjacencyMatrix} returns an object of  class \code{"adjacencyMatrix"}
containing the adjacency matrix representation to be retrieved. The function \code{adjacencyMatrix<-}
returns a graph object in which the adjacency matrix representation has been set.}

%\references{ ... }

\author{Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca}

% \note{ ~~further notes~~ }

% \section{Warning }{....} ~

\seealso{\code{\link{adjacencyMatrix-class}} and \code{\link{simpleGraph-class}}}

% \examples{}

\keyword{ methods }
