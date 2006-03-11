
\name{adjacencyMatrix-class}

\docType{class}

\alias{adjacencyMatrix-class}

\alias{initialize,adjacencyMatrix-method}
\alias{show,adjacencyMatrix-method}
\alias{names,adjacencyMatrix-method}
\alias{names<-,adjacencyMatrix-method}
\alias{card,adjacencyMatrix-method}
\alias{isEmpty,adjacencyMatrix-method}
\alias{isPresent,undirectedEdge,adjacencyMatrix-method}
\alias{isPresent,directedEdge,adjacencyMatrix-method}
\alias{areTheSame,adjacencyMatrix,adjacencyMatrix-method}
\alias{[,adjacencyMatrix-method}
\alias{[[,adjacencyMatrix-method}
\alias{coerce,incidenceList,adjacencyMatrix-method}
\alias{coerce,incidenceMatrix,adjacencyMatrix-method}
\alias{coerce,adjacencyList,adjacencyMatrix-method}
\alias{+,adjacencyMatrix,vertexSet-method}
\alias{-,adjacencyMatrix,vertexSet-method}
\alias{+,adjacencyMatrix,directedEdge-method}
\alias{+,adjacencyMatrix,undirectedEdge-method}
\alias{-,adjacencyMatrix,directedEdge-method}
\alias{-,adjacencyMatrix,undirectedEdge-method}
\alias{*,adjacencyMatrix,vertexSet-method}

\title{Class "adjacencyMatrix"}

\description{A class for adjacency matrix representation of simple-graphs}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("adjacencyMatrix",X)}.
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"matrix"}; standard 0-1 coding
	for ordinary directed and undirected edges}
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
    \item{initialize}{\code{signature(.Object = "adjacencyMatrix")}:
      constructs an adjacency matrix representation of a simple-graph from a 0-1 matrix}
    \item{show}{\code{signature(object = "adjacencyMatrix")}: displays an adjacency matrix representation }
    \item{names}{\code{signature(x = "adjacencyMatrix")}: gets the character vertex identifiers of an adjacency matrix}
	\item{names<-}{\code{signature(x = "adjacencyMatrix")}: sets the character vertex identifiers of an adjacency matrix}
    \item{card}{\code{signature(object = "adjacencyMatrix")}:
	  returns the number of vertices and the total number of edges (directed and undirected) in an incidence matrix}
    \item{isEmpty}{\code{signature(object = "adjacencyMatrix")}:
	  an adjacency matrix is empty if it has no entries}
    \item{isPresent}{\code{signature(el = "undirectedEdge", ou = "adjacencyMatrix")}:
	  tells whether an undirected edge occurs in the graph represented by an adjacency matrix}
    \item{isPresent}{\code{signature(el = "directedEdge", ou = "adjacencyMatrix")}:
	  tells whether a directed edge occurs in the graph represented by an adjacency matrix}
    \item{areTheSame}{\code{signature(x = "adjacencyMatrix", y = "adjacencyMatrix")}:
	  x and y are the same adjacency matrix if they represent the same simple-graph}
    \item{[}{\code{signature(x = "adjacencyMatrix")}: extracts the adjacency matrix of an induced subgraph}
    \item{[[}{\code{signature(x = "adjacencyMatrix")}: extracts the character identifier of a vertex}
    \item{coerce}{\code{signature(from = "incidenceList", to = "adjacencyMatrix")}:
	  converts an incidence list to an adjacency matrix by keeping ordinary directed and undirected edges
	  and dropping loops and parallel edges}
    \item{coerce}{\code{signature(from = "incidenceMatrix", to = "adjacencyMatrix")}:
	  converts an incidence matrix to an adjacency matrix by dropping hyper-edges, loops and parallel edges}
    \item{coerce}{\code{signature(from = "adjacencyList", to = "adjacencyMatrix")}:
	  converts an adjacency list to an adjacency matrix by dropping loops and parallel edges}
    \item{+}{\code{signature(e1 = "adjacencyMatrix", e2 = "vertexSet")}:
	  adds a vertex set to an adjacency matrix by making the new vertices isolated}
    \item{-}{\code{signature(e1 = "adjacencyMatrix", e2 = "vertexSet")}:
	  removes a vertex set from an adjacency matrix by dropping all edges involving the vertex set}
    \item{+}{\code{signature(e1 = "adjacencyMatrix", e2 = "undirectedEdge")}:
	  adds an ordinary undirected edge (not a loop) to an adjacency matrix}
    \item{+}{\code{signature(e1 = "adjacencyMatrix", e2 = "directedEdge")}:
	  adds an ordinary directed edge to an adjacency matrix}
    \item{-}{\code{signature(e1 = "adjacencyMatrix", e2 = "undirectedEdge")}:
	  removes an undirected edge from an adjacency matrix}
    \item{-}{\code{signature(e1 = "adjacencyMatrix", e2 = "directedEdge")}:
	  removes a directed edge from an adjacency matrix}
    \item{*}{\code{signature(e1 = "adjacencyMatrix", e2 = "vertexSet")}:
	  restricts an adjacency matrix to a vertex set by dropping all edges involving vertices outside the vertex set}
  }
}

% \references{ ... }

\author{Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca}

\note{The \code{names<-} replacement method works only if the names to be assigned
can be used to construct a \code{vertexSet} object having the right cardinality,
otherwise the names are left unchanged and a warning message is given.}

\section{Warning }{All input non-zero diagonal entries are silently changed to zero by the constructor.}

\seealso{\code{\link{adjacencyMatrix}} and \code{\link{simpleGraph-class}}}

% \examples{}

\keyword{classes}
