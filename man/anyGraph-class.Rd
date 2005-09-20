
\name{anyGraph-class}

\docType{class}

\alias{anyGraph-class}

\alias{coerce,simpleGraph,anyGraph-method}
\alias{coerce,multiGraph,anyGraph-method}
\alias{coerce,generalGraph,anyGraph-method}
\alias{incidenceList,anyGraph-method}
\alias{incidenceList<-,anyGraph-method}

\title{Class "anyGraph"}

\description{A class for graphs of any kind.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("anyGraph", ...)}.
%    ~~ describe objects here ~~ 
}

\section{Slots}{
  \describe{
    \item{\code{incidenceList}:}{Object of class \code{"incidenceList"}}
  }
}

\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "generalGraph", to = "anyGraph")}:
	  no edges are lost in the conversion to the most general class of graphs}
    \item{coerce}{\code{signature(from = "multiGraph", to = "anyGraph")}:
	  no edges are lost in the conversion to the most general class of graphs}
    \item{coerce}{\code{signature(from = "simpleGraph", to = "anyGraph")}:
	  no edges are lost in the conversion to the most general class of graphs}
    \item{incidenceList}{\code{signature(object = "anyGraph")}: gets the incidence list representation}
    \item{incidenceList<-}{\code{signature(x = "anyGraph")}: sets the incidence list representation}
  }
}

%\references{ ~put references to the literature/web site here ~ }

\author{Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca}

%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{incidenceList-class}} and \code{\link{incidenceList}}}

%\examples{
%##---- Should be DIRECTLY executable !! ----
%}

\keyword{classes}
