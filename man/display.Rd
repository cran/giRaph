\name{display}
\alias{display}

\title{Make a display of the graph using the graphics window }

\description{Uses the package \pkg{mathgraph} to create a simple display
of a simple graph}

\usage{display(x, ...)}

\arguments{
  \item{x}{an object of class \code{\link{simpleGraph-class}}.}
  \item{\dots}{ further arguments passed to \code{\link[mathgraph]{plot.mathgraph}}.}
}

\value{A display in the grpahics window.
}

\author{Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca}

\seealso{\code{\link{simpleGraph-class}},\code{\link[mathgraph]{plot.mathgraph}}}

\examples{
G<-new("incidenceList",
       V=letters[1:12],
       E=list(
              d(6,5,c(2,4),c(1,3)),
              u(2,4,5),
              d(2,4),d(4,2),
              d(1,7),d(3,7),d(4,7),
              d(5,8),d(5,8),d(5,8),
              u(6,9),d(6,9),
              u(9,9),
              d(9,8),d(9,12),
              u(7,8),u(8,12),u(12,11),u(11,7),
              u(11,8),
              d(11,10)
             )
      )

sg <- new("simpleGraph",adjacencyMatrix=as(G,"adjacencyMatrix"))
display(sg)

gg <-new("generalGraph",incidenceList=G)
display(gg)
}

\keyword{ graphs }
\keyword{ methods }
