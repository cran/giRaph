\name{dynamic.Graph}
\alias{dynamic.Graph}

\title{DynamicGraph display of simple graph}

\description{Uses the package \pkg{dynamicGraph} to create an advanced,
  interactive display of a simple graph }

\usage{
dynamic.Graph(object, ...)
}

\arguments{
  \item{object}{ an object of class \code{\link{simpleGraph-class}}. }
  \item{\dots}{ further arguments passed to \code{\link[dynamicGraph]{DynamicGraph}}. }
}

\value{
  A dynamicGraph window is open in which the graph can be inspected
  interactively. 
}

\note{This interface is still very experimental.}

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
dynamic.Graph(sg)

G.1 <- new("incidenceList", 
           E = list(u(1, 2), d(1, 3), u(3), 
                    d(2, 5), d(2, 5), d(3, c(1, 4), 5),
                    u(2, 4, 5), d(c(3, 4), c(2, 1)), r(1, 5)), 
           V = 5:10)

ag <- new("anyGraph", incidenceList = G.1)
dynamic.Graph(ag)
}

\keyword{ graphs }
\keyword{ methods }
