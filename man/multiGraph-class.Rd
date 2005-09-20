
\name{multiGraph-class}

\docType{class}

\alias{multiGraph-class}

\alias{coerce,simpleGraph,multiGraph-method}
\alias{coerce,generalGraph,multiGraph-method}
\alias{coerce,anyGraph,multiGraph-method}
\alias{adjacencyList,multiGraph-method}
\alias{adjacencyList<-,multiGraph-method}

\title{Class "multiGraph"}

\description{A class for multi-graphs.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("multiGraph", ...)}.
%    ~~ describe objects here ~~ 
}

\section{Slots}{
  \describe{
    \item{\code{adjacencyList}:}{Object of class \code{"adjacencyList"}}
    \item{\code{incidenceMatrix}:}{Object of class \code{"incidenceMatrix"}}
    \item{\code{incidenceList}:}{Object of class \code{"incidenceList"}}
  }
}

\section{Extends}{
Class \code{"generalGraph"}, directly, with explicit coerce.
Class \code{"anyGraph"}, directly, with explicit coerce.
}

\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "anyGraph", to = "multiGraph")}:
	  all but ordinary directed and undirected edges are lost in the conversion}
    \item{coerce}{\code{signature(from = "generalGraph", to = "multiGraph")}:
	  hyper-edges are lost in the conversion}
    \item{coerce}{\code{signature(from = "simpleGraph", to = "multiGraph")}:
	  no edges are lost in the conversion as every simple-graph is a multi-graph}
    \item{adjacencyList}{\code{signature(object = "multiGraph")}: gets the adjacency list representation}
    \item{adjacencyList<-}{\code{signature(x = "multiGraph")}: sets the adjacency list representation}
  }
}

%\references{ ~put references to the literature/web site here ~ }

\author{Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca}

%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{adjacencyList-class}} and \code{\link{adjacencyList}}}

%\examples{
%##---- Should be DIRECTLY executable !! ----
%}

\keyword{classes}
