
\name{undirectedEdge-class}

\docType{class}

\alias{undirectedEdge-class}

\alias{initialize,undirectedEdge-method}
\alias{show,undirectedEdge-method}
\alias{showRel,undirectedEdge,vertexSet-method}
\alias{areTheSame,undirectedEdge,undirectedEdge-method}
\alias{areTheSame,undirectedEdge,edge-method}
\alias{areTheSame,edge,undirectedEdge-method}
\alias{[,undirectedEdge-method}
\alias{coerce,vector,undirectedEdge-method}
\alias{coerce,directedEdge,undirectedEdge-method}
\alias{maxId,undirectedEdge-method}
\alias{recode,undirectedEdge,vertexSet,vertexSet-method}

\title{Class "undirectedEdge"}

\description{Class for undirected edges}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("undirectedEdge", ...)}
  which admit short-hands of the form \code{u(...)}.
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"vector"}
	storing strictly positive numbers that refer to a given \code{"vertexSet"} object}
  }
}

\section{Extends}{
Class \code{"edge"}, directly.
Class \code{"integer"}, from data part.
Class \code{"vector"}, by class \code{"integer"}.
Class \code{"numeric"}, by class \code{"integer"}.
}

\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "undirectedEdge")}:
	  constructs an undirected edge from a vector of strictly positive integers}
	\item{show}{\code{signature(object = "undirectedEdge")}:
	  displays an undirected edge as numbers joined by lines}
	\item{showRel}{\code{signature(object = "undirectedEdge",code="vertexSet")}:
	  displays an undirected edge as names joined by lines}
    \item{areTheSame}{\code{signature(x = "undirectedEdge", y = "undirectedEdge")}:
	  x and y are the same undirected edge if they are the same set of numbers}
    \item{areTheSame}{\code{signature(x = "undirectedEdge", y = "edge")}:
      always returns FALSE, as two edges of different kind are never the same}
    \item{areTheSame}{\code{signature(x = "edge", y = "undirectedEdge")}:
      always returns FALSE, as two edges of different kind are never the same}	  
    \item{[}{\code{signature(x = "undirectedEdge")}: extracts an undirected edge}
    \item{coerce}{\code{signature(from = "vector", to = "undirectedEdge")}: constructs an undirected edge from vector input}
    \item{coerce}{\code{signature(from = "directedEdge", to = "undirectedEdge")}: makes a directed edge undirected}
	\item{maxId}{\code{signature(x="undirectedEdge")}: gets the maximum numeric identifier of an undirected edge}
    \item{recode}{\code{signature(object = "undirectedEdge",src="vertexSet",dst="vertexSet")}:
	  recodes an undirected edge by making its numbers refer to another \code{"vertexSet"} object}
  }
}

% \references{ ... }

\author{Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca}

% \note{ ~~further notes~~ }

\section{Warning }{The constructor will try to handle any vector input
by silently transforming it into a list of strictly positive integers.}

\seealso{\code{\link{edge-class}},
 \code{\link{directedEdge-class}},
 \code{\link{edgeList-class}} and
 \code{\link{u}}.
}

% \examples{}

\keyword{classes}
