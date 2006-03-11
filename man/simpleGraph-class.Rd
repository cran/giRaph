
\name{simpleGraph-class}

\docType{class}

\alias{simpleGraph-class}

\alias{initialize,simpleGraph-method}
\alias{show,simpleGraph-method}
\alias{display,simpleGraph-method}
\alias{dynamic.Graph,simpleGraph-method}
\alias{incidenceList<-,simpleGraph-method}
\alias{incidenceMatrix<-,simpleGraph-method}
\alias{adjacencyList<-,simpleGraph-method}
\alias{adjacencyMatrix<-,simpleGraph-method}
\alias{adjacencyMatrix,simpleGraph-method}
\alias{names,simpleGraph-method}
\alias{names<-,simpleGraph-method}
\alias{card,simpleGraph-method}
\alias{isEmpty,simpleGraph-method}
\alias{isPresent,edge,simpleGraph-method}
\alias{areTheSame,simpleGraph,simpleGraph-method}
\alias{[,simpleGraph-method}
\alias{[[,simpleGraph-method}
\alias{coerce,multiGraph,simpleGraph-method}
\alias{coerce,generalGraph,simpleGraph-method}
\alias{coerce,anyGraph,simpleGraph-method}
\alias{coerce,mathgraph,simpleGraph-method}
\alias{coerce,simpleGraph,mathgraph-method}
\alias{coerce,simpleGraph,dg.simple.graph-method}
\alias{coerce,simpleGraph,dg.graph-method}
\alias{+,simpleGraph,vertexSet-method}
\alias{-,simpleGraph,vertexSet-method}
\alias{+,simpleGraph,directedEdge-method}
\alias{+,simpleGraph,undirectedEdge-method}
\alias{-,simpleGraph,edge-method}
\alias{*,simpleGraph,vertexSet-method}

\title{Class "simpleGraph"}

\description{A class for simple-graphs}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("simpleGraph", ...)}.
A \code{simpleGraph} object consists of four slots, one for each possible representation:
\code{adjacencyMatrix}, \code{adjacencyList}, \code{incidenceMatrix} and \code{incidenceList}.
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
    \item{initialize}{\code{signature(.Object="simpleGraph")}:
	  constructs a simple-graph from one of the four possible representations}
    \item{show}{\code{signature(object = "simpleGraph")}: displays the available representations of a simple graph}
    \item{display}{\code{signature(x = "simpleGraph")}: static graphical representation via package 'mathgraph'}
    \item{dynamic.Graph}{\code{signature(object = "simpleGraph")}:
	  dynamic graphical representation via package 'dynamicGraph'}
    \item{incidenceList<-}{\code{signature(x = "simpleGraph")}: sets the incidence list representation}
    \item{incidenceMatrix<-}{\code{signature(x = "simpleGraph")}: sets the incidence matrix representation}
    \item{adjacencyList<-}{\code{signature(x = "simpleGraph")}: sets the adjacency list representation}
    \item{adjacencyMatrix<-}{\code{signature(x = "simpleGraph")}: sets the adjacency matrix representation}
    \item{adjacencyMatrix}{\code{signature(object = "simpleGraph")}: gets the adjacency matrix representation}
	\item{names}{\code{signature(x = "simpleGraph")}: gets the character vertex identifiers of a simple graph}
	\item{names<-}{\code{signature(x = "simpleGraph")}: sets the character vertex identifiers of a simple graph}
    \item{card}{\code{signature(object = "simpleGraph")}:
	  returns the number of vertices and the total number of edges (directed and undirected) in a simple graph}
    \item{isEmpty}{\code{signature(object = "simpleGraph")}:
	  a graph object is empty if all its possible representations are empty}
    \item{isPresent}{\code{signature(el = "edge", ou = "simpleGraph")}:
	  an edge occurs in a graph object if it occurs in its non-empty slots}
    \item{areTheSame}{\code{signature(x = "simpleGraph", y = "simpleGraph")}:
	  x and y are the same if their non-empty slots represent the same graph}
    \item{[}{\code{signature(x = "simpleGraph")}: extracts an induced subgraph}
    \item{[[}{\code{signature(x = "simpleGraph")}: extracts the character identifier of a vertex} 
    \item{coerce}{\code{signature(from = "anyGraph", to = "simpleGraph")}:
	  only ordinary directed and undirected edges, but no loops nor parallel edges, are kept in the conversion}
    \item{coerce}{\code{signature(from = "generalGraph", to = "simpleGraph")}:
	  hyper-edges, loops and parallel edges are lost in the conversion}
    \item{coerce}{\code{signature(from = "multiGraph", to = "simpleGraph")}:
	  loops and parallel edges are lost in the conversion}
    \item{coerce}{\code{signature(from = "mathgraph", to = "simpleGraph")}:
	  conversion from class 'mathgraph' of package 'mathgraph'}
    \item{coerce}{\code{signature(from = "simpleGraph", to = "mathgraph")}:
	  conversion to class 'mathgraph' of package 'mathgraph'}
    \item{coerce}{\code{signature(from = "simpleGraph", to = "dg.simple.graph")}:
	  conversion to class 'dg.simple.graph' of package 'dynamicGraph'}
    \item{coerce}{\code{signature(from = "simpleGraph", to = "dg.graph")}:
	  conversion to class 'dg.graph' of package 'dynamicGraph'}
    \item{+}{\code{signature(e1 = "simpleGraph", e2 = "vertexSet")}:
	  adds a vertex set to a simple graph by making the new vertices isolated}
    \item{-}{\code{signature(e1 = "simpleGraph", e2 = "vertexSet")}:
	  removes a vertex set from a simple graph by dropping all edges involving the vertex set}
    \item{+}{\code{signature(e1 = "simpleGraph", e2 = "undirectedEdge")}:
	  adds an ordinary undirected edge (not a loop) to a simple graph}
    \item{+}{\code{signature(e1 = "simpleGraph", e2 = "directedEdge")}:
	  adds an ordinary directed edge to a simple graph}
    \item{-}{\code{signature(e1 = "simpleGraph", e2 = "edge")}:
	  removes an edge from a simple graph}
    \item{*}{\code{signature(e1 = "simpleGraph", e2 = "vertexSet")}:
	  restricts a simple graph to a vertex set by dropping all edges involving vertices outside the vertex set}
  }
}

%\references{ ~put references to the literature/web site here ~ }

\author{Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca}

\note{Graphical representation via package 'dynamicGraph' is based on coercion to class \code{dg.graph},
implemented via coercion to class \code{dg.simple.graph}.}

 %~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{adjacencyMatrix-class}} and \code{\link{adjacencyMatrix}}}

%\examples{
%##---- Should be DIRECTLY executable !! ----
%}

\keyword{classes}
