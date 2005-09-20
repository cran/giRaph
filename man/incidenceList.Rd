
\name{incidenceList}

\alias{incidenceList}
\alias{incidenceList<-}

\title{Incidence list representation of a graph}

\description{Retrieve or set the incidence list representation of a graph.}

\usage{incidenceList(object, ...)
incidenceList(x, force = TRUE) <- value}

\arguments{
  \item{object}{a graph object from which the representation should be retrieved}
  \item{\dots}{additional parameters to be used when retrieving the representation}
  \item{x}{a graph object in which the representation should be set}
  \item{force}{a logical value telling whether the representation should be set even if this amounts to changing the graph}
  \item{value}{an object of class \code{"incidenceList"} containing the representation to be set}
}

\details{The functions \code{incidenceList} and \code{incidenceList<-} are generic.}

\value{The function \code{incidenceList} returns an object of  class \code{"incidenceList"}
containing the incidence list representation to be retrieved. The function \code{incidenceList<-}
returns a graph object in which the incidence list representation has been set.}

%\references{ ... }

\author{Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca}

% \note{ ~~further notes~~ }

% \section{Warning }{....} ~

\seealso{\code{\link{incidenceList-class}} and \code{\link{anyGraph-class}}}

% \examples{}

\keyword{ methods }
