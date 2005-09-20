
\name{directedEdge-class}

\docType{class}

\alias{directedEdge-class}

\alias{initialize,directedEdge-method}
\alias{show,directedEdge-method}
\alias{areTheSame,directedEdge,directedEdge-method}
\alias{areTheSame,directedEdge,edge-method}
\alias{areTheSame,edge,directedEdge-method}
\alias{[,directedEdge-method}
\alias{[[,directedEdge-method}
\alias{coerce,vector,directedEdge-method}
\alias{coerce,undirectedEdge,directedEdge-method}
\alias{maxId,directedEdge-method}
\alias{recode,directedEdge,vertexSet,vertexSet-method}

\title{Class "directedEdge"}

\description{Class for directed edges}

\section{Objects from the Class}{
 Objects can be created by calls of the form \code{new("directedEdge", ...)}
 which admit short-hands of the form \code{d(...)} and \code{r(...)}.
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"}
	storing strictly positive numbers that refer to a given \code{"vertexSet"} object}
  }
}

\section{Extends}{
Class \code{"edge"}, directly.
Class \code{"list"}, from data part.
Class \code{"vector"}, by class \code{"list"}.
}

\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "directedEdge")}:
	  constructs a directed edge from a list of strictly positive integers}
	\item{show}{\code{signature(object = "directedEdge")}:
	  displays a directed edge as an ordered sequence of undirected edges joined by arrows}
    \item{areTheSame}{\code{signature(x = "directedEdge", y = "directedEdge")}:
	  x and y are the same directed edge if they are the same ordered sequence of undirected edges}
    \item{areTheSame}{\code{signature(x = "directedEdge", y = "edge")}:
      always returns FALSE, as two edges of different kind are never the same}
    \item{areTheSame}{\code{signature(x = "edge", y = "directedEdge")}:
      always returns FALSE, as two edges of different kind are never the same}
    \item{[}{\code{signature(x = "directedEdge")}: extracts a directed edge}
    \item{[[}{\code{signature(x = "directedEdge")}: extracts an undirected edge}
    \item{coerce}{\code{signature(from = "vector", to = "directedEdge")}: constructs a directed edge from vector input}
    \item{coerce}{\code{signature(from = "undirectedEdge", to = "directedEdge")}: directs an undirected edge}
	\item{maxId}{\code{signature(x="directedEdge")}: gets the maximum numeric identifier of a directed edge}
	\item{recode}{\code{signature(object="directedEdge",src = "vertexSet", dst = "vertexSet")}:
	  recodes a directed edge by making its numbers refer to another \code{"vertexSet"} object}
  }
}

% \references{ ... }

\author{Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca}

% \note{ ~~further notes~~ }

\section{Warning }{The constructor will try to handle any vector input
by silently transforming it into a list of strictly positive integers.}

\seealso{\code{\link{edge-class}},
 \code{\link{undirectedEdge-class}},
 \code{\link{edgeList-class}},
 \code{\link{d}} and \code{\link{r}}.
}

% \examples{}

\keyword{classes}
