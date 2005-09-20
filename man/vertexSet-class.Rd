
\name{vertexSet-class}

\docType{class}

\alias{vertexSet-class}

\alias{initialize,vertexSet-method}
\alias{show,vertexSet-method}
\alias{areTheSame,vertexSet,vertexSet-method}
\alias{[,vertexSet-method}
\alias{coerce,vector,vertexSet-method}
\alias{names,vertexSet-method}
\alias{+,vertexSet,vertexSet-method}
\alias{-,vertexSet,vertexSet-method}

\title{Class "vertexSet"}

\description{Class for vertex sets}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("vertexSet", ...)}
which admit short-hands of the form \code{v(...)}.
}

\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"vector"}
	storing unique character identifiers that are sintactically valid names}
  }
}

\section{Extends}{
Class \code{"character"}, from data part.
Class \code{"vector"}, by class \code{"character"}.
}

\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "vertexSet")}:
	  constructs a vertex set from a vector of unique sintactically valid names}
	\item{show}{\code{signature(object = "vertexSet")}:
	  displays a vertex set as comma separated characters in graph brackets}
    \item{areTheSame}{\code{signature(x = "vertexSet", y = "vertexSet")}:
	  x and y are the same vertex set if they are the same set of character identifiers}
    \item{[}{\code{signature(x = "vertexSet")}: extracts a vertex set}
    \item{coerce}{\code{signature(from = "vector", to = "vertexSet")}: constructs a vertex set from vector input}
    \item{names}{\code{signature(x = "vertexSet")}: gets the character vertex identifiers}
    \item{+}{\code{signature(e1 = "vertexSet", e2 = "vertexSet")}: performs the union of two vertex sets}
    \item{-}{\code{signature(e1 = "vertexSet", e2 = "vertexSet")}: performs the asymmetric difference of two vertex sets}
  }
}

% \references{ ... }

\author{Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca}

% \note{ ~~further notes~~ }

\section{Warning }{The constructor will try to handle any vector input
by silently transforming it into a vector of unique sintactically valid names
obtained via \code{\link{make.names}} from the unique input elements.
}

\seealso{The short-hand \code{\link{v}}.}

% \examples{}

\keyword{classes}
