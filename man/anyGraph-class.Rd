
\name{anyGraph-class}

\docType{class}

\alias{anyGraph-class}

\alias{initialize,anyGraph-method}
\alias{show,anyGraph-method}
\alias{display,anyGraph-method}
\alias{dynamic.Graph,anyGraph-method}
\alias{incidenceList<-,anyGraph-method}
\alias{incidenceList,anyGraph-method}
\alias{names,anyGraph-method}
\alias{names<-,anyGraph-method}
\alias{card,anyGraph-method}
\alias{isEmpty,anyGraph-method}
\alias{isPresent,edge,anyGraph-method}
\alias{areTheSame,anyGraph,anyGraph-method}
\alias{[,anyGraph-method}
\alias{[[,anyGraph-method}
\alias{coerce,simpleGraph,anyGraph-method}
\alias{coerce,multiGraph,anyGraph-method}
\alias{coerce,generalGraph,anyGraph-method}
\alias{coerce,anyGraph,dg.graph-method}
\alias{+,anyGraph,vertexSet-method}
\alias{-,anyGraph,vertexSet-method}
\alias{+,anyGraph,edge-method}
\alias{-,anyGraph,edge-method}
\alias{*,anyGraph,vertexSet-method}

\title{Class "anyGraph"}

\description{A class for graphs of any kind.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("anyGraph", ...)}.
An \code{anyGraph} object consists of a single slot (\code{incidenceList})
for the only possible representation.
}

\section{Slots}{
  \describe{
    \item{\code{incidenceList}:}{Object of class \code{"incidenceList"}}
  }
}

\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object="anyGraph")}:
	  constructs any graph from incidence list representation}
    \item{show}{\code{signature(object = "anyGraph")}: displays the available representations of any graph}
    \item{display}{\code{signature(x = "anyGraph")}: static graphical representation via package 'mathgraph'}
    \item{dynamic.Graph}{\code{signature(object = "anyGraph")}:
	  dynamic graphical representation via package 'dynamicGraph'}
    \item{incidenceList<-}{\code{signature(x = "anyGraph")}: sets the incidence list representation}
    \item{incidenceList}{\code{signature(object = "anyGraph")}: gets the incidence list representation}
	\item{names}{\code{signature(x = "anyGraph")}: gets the character vertex identifiers of any graph}
	\item{names<-}{\code{signature(x = "anyGraph")}: sets the character vertex identifiers of any graph}
    \item{card}{\code{signature(object = "anyGraph")}:
	  returns the number of vertices and the total number of edge occurrences in any graph}
    \item{isEmpty}{\code{signature(object = "anyGraph")}:
	  a graph object is empty if all its possible representations are empty}
    \item{isPresent}{\code{signature(el = "edge", ou = "anyGraph")}:
	  an edge occurs in a graph object if it occurs in its non-empty slots}
    \item{areTheSame}{\code{signature(x = "anyGraph", y = "anyGraph")}:
	  x and y are the same if their non-empty slots represent the same graph}
    \item{[}{\code{signature(x = "anyGraph")}: extracts an induced subgraph}
    \item{[[}{\code{signature(x = "anyGraph")}: extracts the character identifier of a vertex} 
    \item{coerce}{\code{signature(from = "generalGraph", to = "anyGraph")}:
	  no edges are lost in the conversion to the most general class of graphs}
    \item{coerce}{\code{signature(from = "multiGraph", to = "anyGraph")}:
	  no edges are lost in the conversion to the most general class of graphs}
    \item{coerce}{\code{signature(from = "simpleGraph", to = "anyGraph")}:
	  no edges are lost in the conversion to the most general class of graphs}
    \item{coerce}{\code{signature(from = "anyGraph", to = "dg.graph")}:
	  conversion to class 'dg.graph' of package 'dynamicGraph'}
    \item{+}{\code{signature(e1 = "anyGraph", e2 = "vertexSet")}:
	  adds a vertex set to any graph by making the new vertices isolated}
    \item{-}{\code{signature(e1 = "anyGraph", e2 = "vertexSet")}:
	  removes a vertex set from any graph by dropping all edges involving the vertex set}
    \item{+}{\code{signature(e1 = "anyGraph", e2 = "edge")}:
	  adds an edge to any graph}
    \item{-}{\code{signature(e1 = "anyGraph", e2 = "edge")}:
	  removes an edge from any graph}
    \item{*}{\code{signature(e1 = "anyGraph", e2 = "vertexSet")}:
	  restricts any graph to a vertex set by dropping all edges involving vertices outside the vertex set}
  }
}

%\references{ ~put references to the literature/web site here ~ }

\author{Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca}

\note{Graphical representation via package 'dynamicGraph' is based on coercion to class \code{dg.graph}.
Coercion to class \code{dg.graph} is implemented via coercion to class \code{generalGraph}. Graphical
representation via package 'mathgraph' is obtained by means of coercion to class \code{simpleGraph}.}

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{incidenceList-class}} and \code{\link{incidenceList}}}

%\examples{
%##---- Should be DIRECTLY executable !! ----
%}

\keyword{classes}
