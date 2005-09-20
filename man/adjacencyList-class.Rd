
\name{adjacencyList-class}

\docType{class}

\alias{adjacencyList-class}

\alias{initialize,adjacencyList-method}
\alias{show,adjacencyList-method}
\alias{areTheSame,adjacencyList,adjacencyList-method}
\alias{[,adjacencyList-method}
\alias{coerce,incidenceList,adjacencyList-method}
\alias{coerce,incidenceMatrix,adjacencyList-method}
\alias{coerce,adjacencyMatrix,adjacencyList-method}
\alias{+,adjacencyList,vertexSet-method}
\alias{-,adjacencyList,vertexSet-method}
\alias{+,adjacencyList,undirectedEdge-method}
\alias{+,adjacencyList,directedEdge-method}
\alias{-,adjacencyList,undirectedEdge-method}
\alias{-,adjacencyList,directedEdge-method}

\title{Class "adjacencyList"}

\description{A class for adjacency list representation of multi-graphs}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("adjacencyList",id,pa,ne)}.
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"}; each element represents a vertex and is in turn a \code{list}
	of (at most) three elements, namely \code{pa}, \code{ne} and \code{ch} storing, respectively, the numeric identifiers
	of parents, neighbours and children}
  }
}

\section{Extends}{
Class \code{"list"}, from data part.
Class \code{"vector"}, by class \code{"list"}.
}

\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "adjacencyList")}:
      constructs an adjacency list representation of a multi-graph from a vertex set \code{id},
	  a list \code{pa} of parent numeric identifiers and a list \code{ne} of neighbour numeric identifiers}
    \item{show}{\code{signature(object = "adjacencyList")}: displays an adjacency list representation }
    \item{areTheSame}{\code{signature(x = "adjacencyList", y = "adjacencyList")}:
	  x and y are the same adjacency list if they represent the same multi-graph}
    \item{[}{\code{signature(x = "adjacencyList")}: extracts the subgraph induced by the selected vertices}
    \item{coerce}{\code{signature(from = "incidenceList", to = "adjacencyList")}:
	  converts an incidence list to an adjacency list by dropping all but ordinary directed and undirected edges}
    \item{coerce}{\code{signature(from = "incidenceMatrix", to = "adjacencyList")}:
	  converts an incidence matrix to an adjacency matrix by dropping hyper-edges}
    \item{coerce}{\code{signature(from = "adjacencyMatrix", to = "adjacencyList")}:
	  converts an adjacency matrix to an adjacency list}
    \item{+}{\code{signature(e1 = "adjacencyList", e2 = "vertexSet")}:
	  adds a vertex set to an adjacency list by making the new vertices isolated}
    \item{-}{\code{signature(e1 = "adjacencyList", e2 = "vertexSet")}:
	  removes a vertex set from an adjacency list by dropping all edges involving the vertex set}
    \item{+}{\code{signature(e1 = "adjacencyList", e2 = "undirectedEdge")}:
	  adds an ordinary undirected edge to an adjacency list}
    \item{-}{\code{signature(e1 = "adjacencyList", e2 = "undirectedEdge")}:
	  removes an undirected edge from an adjacency list}
    \item{+}{\code{signature(e1 = "adjacencyList", e2 = "directedEdge")}:
	  adds an ordinary directed edge to an adjacency list}
    \item{-}{\code{signature(e1 = "adjacencyList", e2 = "directedEdge")}:
	  removes a directed edge from an adjacency list}
  }
}

% \references{ ... }

\author{Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca}

% \note{ ~~further notes~~ }

\section{Warning }{All input numeric identifiers that are greater than the actual number of vertices
are silently discarded by the constructor.}

\seealso{\code{\link{adjacencyList}} and \code{\link{multiGraph-class}}}

% \examples{}

\keyword{classes}
