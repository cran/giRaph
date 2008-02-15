
\name{multiGraph-class}

\docType{class}

\alias{multiGraph-class}

\alias{initialize,multiGraph-method}
\alias{show,multiGraph-method}
\alias{display,multiGraph-method}
\alias{dynamic.Graph,multiGraph-method}
\alias{incidenceList<-,multiGraph-method}
\alias{incidenceMatrix<-,multiGraph-method}
\alias{adjacencyList<-,multiGraph-method}
\alias{incidenceList,multiGraph-method}
\alias{incidenceMatrix,multiGraph-method}
\alias{adjacencyList,multiGraph-method}
\alias{names,multiGraph-method}
\alias{names<-,multiGraph-method}
\alias{card,multiGraph-method}
\alias{isEmpty,multiGraph-method}
\alias{isPresent,edge,multiGraph-method}
\alias{areTheSame,multiGraph,multiGraph-method}
\alias{[,multiGraph-method}
\alias{[[,multiGraph-method}
\alias{coerce,simpleGraph,multiGraph-method}
\alias{coerce,generalGraph,multiGraph-method}
\alias{coerce,anyGraph,multiGraph-method}
\alias{coerce,multiGraph,dg.simple.graph-method}
\alias{coerce,multiGraph,dg.graph-method}
\alias{+,multiGraph,vertexSet-method}
\alias{-,multiGraph,vertexSet-method}
\alias{+,multiGraph,edge-method}
\alias{-,multiGraph,edge-method}
\alias{*,multiGraph,vertexSet-method}

\title{Class "multiGraph"}

\description{A class for multi-graphs.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("multiGraph", ...)}.
A \code{multiGraph} object consists of three slots, one for each possible representation:
\code{adjacencyList}, \code{incidenceMatrix} and \code{incidenceList}.
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
    \item{initialize}{\code{signature(.Object="multiGraph")}:
	  constructs a multi-graph from one of the three possible representations}
    \item{show}{\code{signature(object = "multiGraph")}: displays the available representations of a multi-graph}
    \item{display}{\code{signature(x = "multiGraph")}: static graphical representation via package 'mathgraph'}
    \item{dynamic.Graph}{\code{signature(object = "multiGraph")}:
	  dynamic graphical representation via package 'dynamicGraph'}
    \item{incidenceList<-}{\code{signature(x = "multiGraph")}: sets the incidence list representation}
    \item{incidenceMatrix<-}{\code{signature(x = "multiGraph")}: sets the incidence matrix representation}
    \item{adjacencyList<-}{\code{signature(x = "multiGraph")}: sets the adjacency list representation}
    \item{incidenceList}{\code{signature(object = "multiGraph")}: gets the incidence list representation}
    \item{incidenceMatrix}{\code{signature(object = "multiGraph")}: gets the incidence matrix representation}
    \item{adjacencyList}{\code{signature(object = "multiGraph")}: gets the adjacency list representation}
	\item{names}{\code{signature(x = "multiGraph")}: gets the character vertex identifiers of a multi-graph}
	\item{names<-}{\code{signature(x = "multiGraph")}: sets the character vertex identifiers of a multi-graph}
    \item{card}{\code{signature(object = "multiGraph")}:
	  returns the number of vertices and the total number of edge occurrences in a multi-graph}
    \item{isEmpty}{\code{signature(object = "multiGraph")}:
	  a graph object is empty if all its possible representations are empty}
    \item{isPresent}{\code{signature(el = "edge", ou = "multiGraph")}:
	  an edge occurs in a graph object if it occurs in its non-empty slots}
    \item{areTheSame}{\code{signature(x = "multiGraph", y = "multiGraph")}:
	  x and y are the same if their non-empty slots represent the same graph}
    \item{[}{\code{signature(x = "multiGraph")}: extracts an induced subgraph}
    \item{[[}{\code{signature(x = "multiGraph")}: extracts the character identifier of a vertex} 
    \item{coerce}{\code{signature(from = "anyGraph", to = "multiGraph")}:
	  all but ordinary directed and undirected edges are lost in the conversion}
    \item{coerce}{\code{signature(from = "generalGraph", to = "multiGraph")}:
	  hyper-edges are lost in the conversion}
    \item{coerce}{\code{signature(from = "simpleGraph", to = "multiGraph")}:
	  no edges are lost in the conversion as every simple-graph is a multi-graph}
    \item{coerce}{\code{signature(from = "multiGraph", to = "dg.simple.graph")}:
	  conversion to class 'dg.simple.graph' of package 'dynamicGraph'}
    \item{coerce}{\code{signature(from = "multiGraph", to = "dg.graph")}:
	  conversion to class 'dg.graph' of package 'dynamicGraph'}
    \item{+}{\code{signature(e1 = "multiGraph", e2 = "vertexSet")}:
	  adds a vertex set to a multi-graph by making the new vertices isolated}
    \item{-}{\code{signature(e1 = "multiGraph", e2 = "vertexSet")}:
	  removes a vertex set from a multi-graph by dropping all edges involving the vertex set}
    \item{+}{\code{signature(e1 = "multiGraph", e2 = "edge")}:
	  adds an edge to a multi-graph}
    \item{-}{\code{signature(e1 = "multiGraph", e2 = "edge")}:
	  removes an edge from a multi-graph}
    \item{*}{\code{signature(e1 = "multiGraph", e2 = "vertexSet")}:
	  restricts a multi-graph to a vertex set by dropping all edges involving vertices outside the vertex set}
  }
}

%\references{ ~put references to the literature/web site here ~ }

\author{Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca}

\note{Graphical representation via package 'dynamicGraph' is based on coercion to class \code{dg.graph},
implemented via coercion to class \code{dg.simple.graph}. Coercion to class \code{dg.simple.graph} is
implemented via coercion to class \code{simpleGraph}, thus dropping loops and parallel edges. Graphical
representation via package 'mathgraph' is obtained by means of coercion to class \code{simpleGraph}.}

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{adjacencyList-class}} and \code{\link{adjacencyList}}}

%\examples{
%##---- Should be DIRECTLY executable !! ----
%}

\keyword{classes}
