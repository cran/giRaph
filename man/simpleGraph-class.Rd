
\name{simpleGraph-class}

\docType{class}

\alias{simpleGraph-class}

\alias{coerce,multiGraph,simpleGraph-method}
\alias{coerce,generalGraph,simpleGraph-method}
\alias{coerce,anyGraph,simpleGraph-method}
\alias{coerce,mathgraph,simpleGraph-method}
\alias{coerce,simpleGraph,mathgraph-method}
\alias{adjacencyMatrix,simpleGraph-method}
\alias{adjacencyMatrix<-,simpleGraph-method}
\alias{display,simpleGraph-method}
\alias{dynamic.Graph,simpleGraph-method}

\title{Class "simpleGraph"}

\description{A class for simple-graphs}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("simpleGraph", ...)}.
%    ~~ describe objects here ~~ 
}

\section{Slots}{
  \describe{
    \item{\code{adjacencyMatrix}:}{Object of class \code{"adjacencyMatrix"}}
    \item{\code{adjacencyList}:}{Object of class \code{"adjacencyList"}}
    \item{\code{incidenceMatrix}:}{Object of class \code{"incidenceMatrix"}}
    \item{\code{incidenceList}:}{Object of class \code{"incidenceList"}}
  }
}

\section{Extends}{
Class \code{"multiGraph"}, directly, with explicit coerce.
Class \code{"generalGraph"}, directly, with explicit coerce.
Class \code{"anyGraph"}, directly, with explicit coerce.
}

\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "anyGraph", to = "simpleGraph")}:
	  only ordinary directed and undirected edges, but no loops nor parallel edges, are kept in the conversion}
    \item{coerce}{\code{signature(from = "generalGraph", to = "simpleGraph")}:
	  hyper-edges, loops and parallel edges are lost in the conversion}
    \item{coerce}{\code{signature(from = "multiGraph", to = "simpleGraph")}:
	  loops and parallel edges are lost in the conversion}
    \item{coerce}{\code{signature(from = "mathgraph", to = "simpleGraph")}: ... }
    \item{coerce}{\code{signature(from = "simpleGraph", to = "mathgraph")}: ... }
    \item{adjacencyMatrix}{\code{signature(object = "simpleGraph")}: gets the adjacency matrix representation}
    \item{adjacencyMatrix<-}{\code{signature(x = "simpleGraph")}: sets the adjacency matrix representation}
    \item{display}{\code{signature(x = "simpleGraph")}: ... }
    \item{dynamic.Graph}{\code{signature(object = "simpleGraph")}: ... }
  }
}

%\references{ ~put references to the literature/web site here ~ }

\author{Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca}

%\note{ ~~further notes~~ }

 %~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{adjacencyMatrix-class}} and \code{\link{adjacencyMatrix}}}

%\examples{
%##---- Should be DIRECTLY executable !! ----
%}

\keyword{classes}
