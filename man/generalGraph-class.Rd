
\name{generalGraph-class}

\docType{class}

\alias{generalGraph-class}

\alias{initialize,generalGraph-method}
\alias{show,generalGraph-method}
\alias{display,generalGraph-method}
\alias{dynamic.Graph,generalGraph-method}
\alias{incidenceList<-,generalGraph-method}
\alias{incidenceMatrix<-,generalGraph-method}
\alias{incidenceList,generalGraph-method}
\alias{incidenceMatrix,generalGraph-method}
\alias{names,generalGraph-method}
\alias{names<-,generalGraph-method}
\alias{card,generalGraph-method}
\alias{isEmpty,generalGraph-method}
\alias{isPresent,edge,generalGraph-method}
\alias{areTheSame,generalGraph,generalGraph-method}
\alias{[,generalGraph-method}
\alias{[[,generalGraph-method}
\alias{coerce,simpleGraph,generalGraph-method}
\alias{coerce,multiGraph,generalGraph-method}
\alias{coerce,anyGraph,generalGraph-method}
\alias{coerce,generalGraph,dg.graph-method}
\alias{+,generalGraph,vertexSet-method}
\alias{-,generalGraph,vertexSet-method}
\alias{+,generalGraph,edge-method}
\alias{-,generalGraph,edge-method}
\alias{*,generalGraph,vertexSet-method}

\title{Class "generalGraph"}

\description{A class for general graphs.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("generalGraph", ...)}.
A \code{generalGraph} object consists of two slots, one for each possible representation:
\code{incidenceMatrix} and \code{incidenceList}.
}

\section{Slots}{
  \describe{
    \item{\code{incidenceMatrix}:}{Object of class \code{"incidenceMatrix"}}
    \item{\code{incidenceList}:}{Object of class \code{"incidenceList"}}
  }
}

\section{Extends}{
Class \code{"anyGraph"}, directly, with explicit coerce.
}

\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object="generalGraph")}:
	  constructs a general graph from one of the two possible representations}
    \item{show}{\code{signature(object = "generalGraph")}: displays the available representations of a general graph}
    \item{display}{\code{signature(x = "generalGraph")}: static graphical representation via package 'mathgraph'}
    \item{dynamic.Graph}{\code{signature(object = "generalGraph")}:
	  dynamic graphical representation via package 'dynamicGraph'}
    \item{incidenceList<-}{\code{signature(x = "generalGraph")}: sets the incidence list representation}
    \item{incidenceMatrix<-}{\code{signature(x = "generalGraph")}: sets the incidence matrix representation}
    \item{incidenceList}{\code{signature(object = "generalGraph")}: gets the incidence list representation}
    \item{incidenceMatrix}{\code{signature(object = "generalGraph")}: gets the incidence matrix representation}
	\item{names}{\code{signature(x = "generalGraph")}: gets the character vertex identifiers of a general graph}
	\item{names<-}{\code{signature(x = "generalGraph")}: sets the character vertex identifiers of a general graph}
    \item{card}{\code{signature(object = "generalGraph")}:
	  returns the number of vertices and the total number of edge occurrences in a general graph}
    \item{isEmpty}{\code{signature(object = "generalGraph")}:
	  a graph object is empty if all its possible representations are empty}
    \item{isPresent}{\code{signature(el = "edge", ou = "generalGraph")}:
	  an edge occurs in a graph object if it occurs in its non-empty slots}
    \item{areTheSame}{\code{signature(x = "generalGraph", y = "generalGraph")}:
	  x and y are the same if their non-empty slots represent the same graph}
    \item{[}{\code{signature(x = "generalGraph")}: extracts an induced subgraph}
    \item{[[}{\code{signature(x = "generalGraph")}: extracts the character identifier of a vertex} 
    \item{coerce}{\code{signature(from = "anyGraph", to = "generalGraph")}:
	  all but directed and undirected edges are lost in the conversion}
    \item{coerce}{\code{signature(from = "multiGraph", to = "generalGraph")}:
	  no edges are lost in the conversion as every multi-graph is a general graph}
    \item{coerce}{\code{signature(from = "simpleGraph", to = "generalGraph")}:
	  no edges are lost in the conversion as every simple-graph is a general graph}
    \item{coerce}{\code{signature(from = "generalGraph", to = "dg.graph")}:
	  conversion to class 'dg.graph' of package 'dynamicGraph'}
    \item{+}{\code{signature(e1 = "generalGraph", e2 = "vertexSet")}:
	  adds a vertex set to a general graph by making the new vertices isolated}
    \item{-}{\code{signature(e1 = "generalGraph", e2 = "vertexSet")}:
	  removes a vertex set from a general graph by dropping all edges involving the vertex set}
    \item{+}{\code{signature(e1 = "generalGraph", e2 = "edge")}:
	  adds an edge to a general graph}
    \item{-}{\code{signature(e1 = "generalGraph", e2 = "edge")}:
	  removes an edge from a general graph}
    \item{*}{\code{signature(e1 = "generalGraph", e2 = "vertexSet")}:
	  restricts a general graph to a vertex set by dropping all edges involving vertices outside the vertex set}
  }
}

%\references{ ~put references to the literature/web site here ~ }

\author{Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca}

\note{Graphical representation via package 'dynamicGraph' is based on coercion to class \code{dg.graph}.
Coercion to class \code{dg.graph} is obtained by expanding hyper edges to sets of ordinary edges,
and using dashed lines for these. Graphical representation via package 'mathgraph' is obtained by means of
coercion to class \code{simpleGraph}.}

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{incidenceMatrix-class}} and \code{\link{incidenceMatrix}}}

%\examples{
%##---- Should be DIRECTLY executable !! ----
%}

\keyword{classes}
