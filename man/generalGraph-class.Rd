
\name{generalGraph-class}

\docType{class}

\alias{generalGraph-class}

\alias{coerce,simpleGraph,generalGraph-method}
\alias{coerce,multiGraph,generalGraph-method}
\alias{coerce,anyGraph,generalGraph-method}
\alias{incidenceMatrix,generalGraph-method}
\alias{incidenceMatrix<-,generalGraph-method}

\title{Class "generalGraph"}

\description{A class for general graphs.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("generalGraph", ...)}.
%    ~~ describe objects here ~~ 
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
    \item{coerce}{\code{signature(from = "anyGraph", to = "generalGraph")}:
	  all but directed and undirected edges are lost in the conversion}
    \item{coerce}{\code{signature(from = "multiGraph", to = "generalGraph")}:
	  no edges are lost in the conversion as every multi-graph is a general graph}
    \item{coerce}{\code{signature(from = "simpleGraph", to = "generalGraph")}:
	  no edges are lost in the conversion as every simple-graph is a general graph}
    \item{incidenceMatrix}{\code{signature(object = "generalGraph")}: gets the incidence matrix representation}
    \item{incidenceMatrix<-}{\code{signature(x = "generalGraph")}: sets the incidence matrix representation}
  }
}

%\references{ ~put references to the literature/web site here ~ }

\author{Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca}

%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{incidenceMatrix-class}} and \code{\link{incidenceMatrix}}}

%\examples{
%##---- Should be DIRECTLY executable !! ----
%}

\keyword{classes}
