
\name{incidenceMatrix-class}

\docType{class}

\alias{incidenceMatrix-class}

\alias{initialize,incidenceMatrix-method}
\alias{show,incidenceMatrix-method}
\alias{names,incidenceMatrix-method}
\alias{names<-,incidenceMatrix-method}
\alias{card,incidenceMatrix-method}
\alias{isEmpty,incidenceMatrix-method}
\alias{isPresent,undirectedEdge,incidenceMatrix-method}
\alias{isPresent,directedEdge,incidenceMatrix-method}
\alias{areTheSame,incidenceMatrix,incidenceMatrix-method}
\alias{[,incidenceMatrix-method}
\alias{[[,incidenceMatrix-method}
\alias{coerce,incidenceList,incidenceMatrix-method}
\alias{coerce,adjacencyList,incidenceMatrix-method}
\alias{coerce,adjacencyMatrix,incidenceMatrix-method}
\alias{+,incidenceMatrix,vertexSet-method}
\alias{-,incidenceMatrix,vertexSet-method}
\alias{+,incidenceMatrix,undirectedEdge-method}
\alias{+,incidenceMatrix,directedEdge-method}
\alias{-,incidenceMatrix,undirectedEdge-method}
\alias{-,incidenceMatrix,directedEdge-method}
\alias{*,incidenceMatrix,vertexSet-method}

\title{Class "incidenceMatrix"}

\description{A class for incidence matrix representation of general graphs}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("incidenceMatrix", I)}.
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"matrix"}; a column for each vertex and a row for each edge}
  }
}

\section{Extends}{
Class \code{"matrix"}, from data part.
Class \code{"structure"}, by class \code{"matrix"}.
Class \code{"array"}, by class \code{"matrix"}.
Class \code{"vector"}, by class "matrix", with explicit coerce.
}

\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "incidenceMatrix")}:
	  constructs an incidence matrix representation of a general graph from a matrix of positive integers}
	\item{show}{\code{signature(object = "incidenceMatrix")}: displays an incidence matrix representation}
    \item{names}{\code{signature(x = "incidenceMatrix")}: gets the character vertex identifiers of an incidence matrix}
	\item{names<-}{\code{signature(x = "incidenceMatrix")}: sets the character vertex identifiers of an incidence matrix}
    \item{card}{\code{signature(object = "incidenceMatrix")}:
	  returns the number of vertices and the total number of edge occurrences in an incidence matrix}
    \item{isEmpty}{\code{signature(object = "incidenceMatrix")}: an incidence matrix is empty if it has no columns}
    \item{areTheSame}{\code{signature(x = "incidenceMatrix", y = "incidenceMatrix")}:
	  x and y are the same incidence matrix if they represent the same general graph}
    \item{isPresent}{\code{signature(el = "undirectedEdge", ou = "incidenceMatrix")}:
	  tells whether an undirected edge occurs in the graph represented by an incidence matrix}
    \item{isPresent}{\code{signature(el = "directedEdge", ou = "incidenceMatrix")}:
	  tells whether a directed edge occurs in the graph represented by an incidence matrix}
    \item{[}{\code{signature(x = "incidenceList")}: extracts the incidence matrix of an induced subgraph}
    \item{[[}{\code{signature(x = "incidenceList")}: extracts the character identifier of a vertex} 
    \item{coerce}{\code{signature(from = "incidenceList", to = "incidenceMatrix")}:
	  converts an incidence list to an incidence matrix by dropping all but undirected and directed edges}
    \item{coerce}{\code{signature(from = "adjacencyList", to = "incidenceMatrix")}:
	  converts an adjacency list to an incidence matrix}
    \item{coerce}{\code{signature(from = "adjacencyMatrix", to = "incidenceMatrix")}:
	  converts and adjacency matrix to an incidence matrix}
    \item{+}{\code{signature(e1 = "incidenceMatrix", e2 = "vertexSet")}:
	  adds a vertex set to an incidence matrix by making the new vertices isolated}
    \item{-}{\code{signature(e1 = "incidenceMatrix", e2 = "vertexSet")}:
	  removes a vertex set from an incidence matrix by dropping all edges involving the vertex set}
    \item{+}{\code{signature(e1 = "incidenceMatrix", e2 = "undirectedEdge")}: adds an undirected edge to an incidence matrix}
    \item{+}{\code{signature(e1 = "incidenceMatrix", e2 = "directedEdge")}: adds a directed edge to an incidence matrix}
    \item{-}{\code{signature(e1 = "incidenceMatrix", e2 = "undirectedEdge")}:
	  removes an undirected edge from an incidence matrix}
    \item{-}{\code{signature(e1 = "incidenceMatrix", e2 = "directedEdge")}:
	  removes a directed edge from an incidence matrix}
    \item{*}{\code{signature(e1 = "incidenceMatrix", e2 = "vertexSet")}:
	  restricts an incidence matrix to a vertex set by dropping all edges involving vertices outside the vertex set}
  }
}

% \references{ ... }

\author{Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca}

\note{The \code{names<-} replacement method works only if the names to be assigned
can be used to construct a \code{vertexSet} object having the right cardinality,
otherwise the names are left unchanged and a warning message is given.}
\section{Warning }{All zero input rows are discarded by the constructor.}

\seealso{\code{\link{incidenceMatrix}} and \code{\link{generalGraph-class}}}

% \examples{}

\keyword{classes}
