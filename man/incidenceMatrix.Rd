
\name{incidenceMatrix}

\alias{incidenceMatrix}
\alias{incidenceMatrix<-}

\title{Incidence matrix representation of a graph}

\description{Retrieve or set the incidence matrix representation of a graph}

\usage{incidenceMatrix(object, ...)
incidenceMatrix(x, force = TRUE)<- value}

\arguments{
  \item{object}{a graph object from which the representation should be retrieved}
  \item{\dots}{additional parameters to be used when retrieving the representation}
  \item{x}{a graph object in which the representation should be set}
  \item{force}{a logical value telling whether the representation should be set even if this amounts to changing the graph}
  \item{value}{an object of class \code{"incidenceMatrix"} containing the representation to be set}
}

\details{The functions \code{incidenceMatrix} and \code{incidenceMatrix<-} are generic.}

\value{The function \code{incidenceMatrix} returns an object of  class \code{"incidenceMatrix"}
containing the incidence matrix representation to be retrieved. The function \code{incidenceMatrix<-}
returns a graph object in which the incidence matrix representation has been set.}

%\references{ ... }

\author{Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca}

% \note{ ~~further notes~~ }

% \section{Warning }{....} ~

\seealso{\code{\link{incidenceMatrix-class}} and \code{\link{generalGraph-class}}}

% \examples{}

\keyword{ methods }
