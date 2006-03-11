
\name{incidenceList-class}

\docType{class}

\alias{incidenceList-class}

\alias{initialize,incidenceList-method}
\alias{show,incidenceList-method}
\alias{names,incidenceList-method}
\alias{names<-,incidenceList-method}
\alias{card,incidenceList-method}
\alias{isEmpty,incidenceList-method}
\alias{isPresent,edge,incidenceList-method}
\alias{areTheSame,incidenceList,incidenceList-method}
\alias{[,incidenceList-method}
\alias{[[,incidenceList-method}
\alias{coerce,incidenceMatrix,incidenceList-method}
\alias{coerce,adjacencyList,incidenceList-method}
\alias{coerce,adjacencyMatrix,incidenceList-method}
\alias{+,incidenceList,vertexSet-method}
\alias{-,incidenceList,vertexSet-method}
\alias{+,incidenceList,edge-method}
\alias{-,incidenceList,edge-method}
\alias{*,incidenceList,vertexSet-method}

\title{Class "incidenceList"}

\description{A class for incidence list representation of any graph}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("incidenceList", V, E)}.
}

\section{Slots}{
  \describe{
    \item{\code{V}:}{Object of class \code{"vertexSet"}}
    \item{\code{E}:}{Object of class \code{"edgeList"}}
  }
}

\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "incidenceList")}: constructs an incidence list representation
	  of a graph from a vertex set and a mult-set of edges}
	\item{show}{\code{signature(object = "incidenceList")}: displays an incidence list representation}
	\item{names}{\code{signature(x = "incidenceList")}: gets the character vertex identifiers of an incidence list}
	\item{names<-}{\code{signature(x = "incidenceList")}: sets the character vertex identifiers of an incidence list}
    \item{card}{\code{signature(object = "incidenceList")}:
	  returns the number of vertices and the total number of edge occurrences in an incidence list}
    \item{isEmpty}{\code{signature(object = "incidenceList")}:
	  an incidence list is empty if such is its vertex set}
    \item{isPresent}{\code{signature(el = "edge", ou = "incidenceList")}:
	  tells whether an edge occurs in the graph represented by an incidence list}
    \item{areTheSame}{\code{signature(x = "incidenceList", y = "incidenceList")}:
	  x and y are the same incidence list if they represent the same graph}
    \item{[}{\code{signature(x = "incidenceList")}: extracts the incidence list of an induced subgraph}
    \item{[[}{\code{signature(x = "incidenceList")}: extracts the character identifier of a vertex}
    \item{coerce}{\code{signature(from = "incidenceMatrix", to = "incidenceList")}:
	  converts an incidence matrix to an incidence list}
    \item{coerce}{\code{signature(from = "adjacencyList", to = "incidenceList")}:
	  converts an adjacency list to an incidence list}
    \item{coerce}{\code{signature(from = "adjacencyMatrix", to = "incidenceList")}:
	  converts an adjacency matrix to an incidence list}
    \item{+}{\code{signature(e1 = "incidenceList", e2 = "vertexSet")}:
	  adds a vertex set to an incidence list by making the new vertices isolated}
    \item{-}{\code{signature(e1 = "incidenceList", e2 = "vertexSet")}:
	  removes a vertex set from an incidence list by dropping all edges involving the vertex set}
	\item{+}{\code{signature(e1 = "incidenceList", e2 = "edge")}: adds an edge to an incidence list}
    \item{-}{\code{signature(e1 = "incidenceList", e2 = "edge")}: removes an edge from an incidence list}
    \item{*}{\code{signature(e1 = "incidenceList", e2 = "vertexSet")}:
	  restricts an incidence list to a vertex set by dropping all edges involving vertices outside the vertex set}
  }
}

% \references{ ... }

\author{Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca}

\note{The \code{names<-} replacement method works only if the names to be assigned
can be used to construct a \code{vertexSet} object having the right cardinality,
otherwise the names are left unchanged and a warning message is given.}

\section{Warning }{All input edges whose maximum numeric identifier is greater than
the actual number of vertices are silently discarded by the constructor.}

\seealso{\code{\link{incidenceList}} and \code{\link{anyGraph-class}}}

% \examples{}

\keyword{classes}
