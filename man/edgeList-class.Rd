
\name{edgeList-class}

\docType{class}

\alias{edgeList-class}

\alias{initialize,edgeList-method}
\alias{show,edgeList-method}
\alias{showRel,edgeList,vertexSet-method}
\alias{areTheSame,edgeList,edgeList-method}
\alias{isPresent,edge,edgeList-method}
\alias{[,edgeList-method}
\alias{+,edgeList,edge-method}
\alias{+,edge,edgeList-method}
\alias{-,edgeList,edge-method}
\alias{maxId,edgeList-method}
\alias{recode,edgeList,vertexSet,vertexSet-method}
\alias{coerce,list,edgeList-method}

\title{Class "edgeList"}

\description{Class for multi-sets of edges}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("edgeList", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"} whose elements are of class \code{"edge"}}
  }
}

\section{Extends}{
Class \code{"list"}, from data part.
Class \code{"vector"}, by class \code{"list"}.
}

\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "edgeList")}: constructs a multi-set of edges from a list of edges}
    \item{show}{\code{signature(object = "edgeList")}: dispalys a multi-set of edges in graph brackets (numeric codes)}
    \item{showRel}{\code{signature(object = "edgeList",code="vertexSet")}:
	  dispalys a multi-set of edges in graph brackets (character names}
    \item{areTheSame}{\code{signature(x = "edgeList", y = "edgeList")}:
	  x and y are the same multi-set of edges if they contain the same edges with the same multiplicity}
    \item{isPresent}{\code{signature(el = "edge", ou = "edgeList")}:
	  tells whether an edge belongs to a multi-set of edges}
    \item{[}{\code{signature(x = "edgeList")}: extracts a multi-set of edges}
    \item{+}{\code{signature(e1 = "edgeList", e2 = "edge")}: adds an (occurrence of an) edge to a multi-set of edges}
    \item{+}{\code{signature(e1 = "edge", e2 = "edgeList")}: adds an (occurrence of an) edge to a multi-set of edges}
    \item{-}{\code{signature(e1 = "edgeList", e2 = "edge")}: drops (the first occurrence of) an edge from a multi-set of edges}
	\item{maxId}{\code{signature(x="edgeList")}: gets the maximum numeric identifier of a multi-set of edges}
    \item{recode}{\code{signature(object = "edgeList",src="vertexSet",dst="vertexSet")}:
	  recodes a multi-sets of edges by making the numbers of its edges refer to another \code{"vertexSet"} object}
    \item{coerce}{\code{signature(from = "list", to = "edgeList")}: constructs a multi-set of edges from list input}
  }
}

% \references{ ... }

\author{Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca}

% \note{ ~~further notes~~ }

\section{Warning }{All non-edge elements of the input list are silently discarded by the constructor.}

\seealso{\code{\link{edge-class}},
 \code{\link{undirectedEdge-class}} and
 \code{\link{directedEdge-class}}.
}

% \examples{}

\keyword{classes}
