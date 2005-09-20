
library(giRaph)

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

I<-as(G,"incidenceMatrix")

A<-as(I,"adjacencyList")

X<-as(I,"adjacencyMatrix")
