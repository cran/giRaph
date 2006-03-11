
\name{adjacencyList-class}

\docType{class}

\alias{adjacencyList-class}

\alias{initialize,adjacencyList-method}
\alias{show,adjacencyList-method}
\alias{names,adjacencyList-method}
\alias{names<-,adjacencyList-method}
\alias{card,adjacencyList-method}
\alias{isEmpty,adjacencyList-method}
\alias{isPresent,undirectedEdge,adjacencyList-method}
\alias{isPresent,directedEdge,adjacencyList-method}
\alias{areTheSame,adjacencyList,adjacencyList-method}
\alias{[,adjacencyList-method}
\alias{[[,adjacencyList-method}
\alias{coerce,incidenceList,adjacencyList-method}
\alias{coerce,incidenceMatrix,adjacencyList-method}
\alias{coerce,adjacencyMatrix,adjacencyList-method}
\alias{+,adjacencyList,vertexSet-method}
\alias{-,adjacencyList,vertexSet-method}
\alias{+,adjacencyList,undirectedEdge-method}
\alias{+,adjacencyList,directedEdge-method}
\alias{-,adjacencyList,undirectedEdge-method}
\alias{-,adjacencyList,directedEdge-method}
\alias{*,adjacencyList,vertexSet-method}

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
	  a list \code{pa} of parent numeric identifiers, a list \code{ch} of children numeric identifiers,
	  and a list \code{ne} of neighbour numeric identifiers}
    \item{show}{\code{signature(object = "adjacencyList")}: displays an adjacency list representation }
	\item{names}{\code{signature(x = "adjacencyList")}: gets the character vertex identifiers of an adjacency list}
	\item{names<-}{\code{signature(x = "adjacencyList")}: sets the character vertex identifiers of an adjacency list}
    \item{card}{\code{signature(object = "adjacencyList")}:
	  returns the number of vertices and the total number of edge occurrences in an adjacency list}
    \item{isEmpty}{\code{signature(object = "adjacencyList")}: an adjacency list is empty if it has no entries}
    \item{isPresent}{\code{signature(el = "undirectedEdge", ou = "adjacencyList")}:
	  tells whether an undirected edge occurs in the multi-graph represented by an adjacency list}
    \item{isPresent}{\code{signature(el = "directedEdge", ou = "adjacencyList")}:
	  tells whether a directed edge occurs in the multi-graph represented by an adjacency list}
    \item{areTheSame}{\code{signature(x = "adjacencyList", y = "adjacencyList")}:
	  x and y are the same adjacency list if they represent the same multi-graph}
    \item{[}{\code{signature(x = "adjacencyList")}: extracts the adjacency list of an induced subgraph}
    \item{[[}{\code{signature(x = "adjacencyList")}: extracts the character identifier of a vertex}
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
    \item{+}{\code{signature(e1 = "adjacencyList", e2 = "directedEdge")}:
	  adds an ordinary directed edge to an adjacency list}
    \item{-}{\code{signature(e1 = "adjacencyList", e2 = "undirectedEdge")}:
	  removes an undirected edge from an adjacency list}
    \item{-}{\code{signature(e1 = "adjacencyList", e2 = "directedEdge")}:
	  removes a directed edge from an adjacency list}
    \item{*}{\code{signature(e1 = "adjacencyMatrix", e2 = "vertexSet")}:
	  restricts an adjacency matrix to a vertex set by dropping all edges involving vertices outside the vertex set}
  }
}

% \references{ ... }

\author{Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca}

\note{The \code{names<-} replacement method works only if the names to be assigned
can be used to construct a \code{vertexSet} object having the right cardinality,
otherwise the names are left unchanged and a warning message is given.}

\section{Warning}{The \code{pa}, \code{ch} and \code{ne} constructor input lists are silently discarded
if their length differs from the actual number of vertices (determined by \code{id}). All input numeric identifiers
greater than this number are also silently discarded by the constructor. The \code{id} input to constructor is mandatory,
since the constructor needs to identify vertices. In addition, at least one between \code{pa} and \code{ch} should be present,
if directed edges have to be specified, and \code{ne} should be present, if undirected edges have to be specified.
If both \code{pa} and \code{ch} are present, the union of the two corresponding multi-sets of edges is specified.
If the neighbourhood relationship specified by \code{ne} is not symmetric, it is made symmetric by the constructor.}

\seealso{\code{\link{adjacencyList}} and \code{\link{multiGraph-class}}}

% \examples{}

\keyword{classes}
