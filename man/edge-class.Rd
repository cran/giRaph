
\name{edge-class}

\docType{class}

\alias{edge-class}
\alias{areTheSame,edge,edge-method}

\title{Virtual Class "edge"}

\description{Virtual Class for all edges}

\section{Objects from the Class}{A virtual Class: No objects may be created from it.}

\section{Methods}{
  \describe{
    \item{areTheSame}{\code{signature(x = "edge", y = "edge")}:
      always returns FALSE, implementing the idea that two edges of different kind are never the same}
  }
}

% \references{ ... }

\author{Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca}

% \note{ ~~further notes~~ }

% \section{Warning }{....} ~

\seealso{\code{\link{undirectedEdge-class}},
\code{\link{directedEdge-class}} and
\code{\link{edgeList-class}}.
}

% \examples{}

\keyword{classes}
