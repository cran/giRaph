## f8-interfaces.R --- 
## Author          : Jens Henrik Badsberg, Claus Dethlefsen, Luca la Rocca
## Created On      : Wed Nov 03 19:22:57 2004
## Last Modified By: Claus Dethlefsen
## Last Modified On: Sat Sep 17 08:34:16 2005
## Update Count    : 73
## Status          : Unknown, Use with caution!
######################################################


# 'dynamicGraph' method for objects of class 'simpleGraph'
setMethod("dynamic.Graph", signature(object = "simpleGraph"),
          function(object, ...)
  {
    require(dynamicGraph)
    iList <- incidenceList(object)
    Names <- names(iList)
    Edges  <- list()
    m <- length(iList@E)
    if (m>0) {
      for(i in 1:m) {
        edge <- iList@E[[i]]
        oriented <- is(edge,"directedEdge")
        if (oriented) warning("Don't know how to tag as oriented\n")
        vertex.indices <- as.numeric(unlist(edge))
        Edges[[i]] <- vertex.indices
#        Edges[[i]] <- new("dg.VertexEdge",oriented=oriented,
#                          vertex.indices=vertex.indices)
      }
    }
      DynamicGraph(names=Names,edge.list=Edges,
                   object=object,...)
  })

## Bioconductors 'graph' (or, rather graphNEL)
##
## setAs("simpleGraph","graphNEL",function(from,to){
##   require(graph)
##   Nodes <- names(from)
##   G <- incidenceList(from)
##   Edges <- G@edgeList
##   
##   n <- length(Edges)
##   ndirected <- sum(unlist(lapply(Edges,is,"directedEdge")))
##   nundirected<- n - ndirected
## 
##   if (ndirected>=nundirected) edgemode <- "directed"
##   else edgemode <- "undirected"
## 
##   if (edgemode=="directed") Edges <- lapply(Edges,d)
##   else Edges <- lapply(Edges,u)
## 
##   G@edgeList <- new("edgeList",Edges)
##   A <- as(G,"adjacencyList")
##   A <- lapply(A,function(x) {
##     if (!is(x,"reverseEdge")) return(unlist(x))
##   })
## 
##   A <- lapply(A,function(i) list(edges=Nodes[i]))
##   
##   new(to,nodes=Nodes,edgeL=A,edgemode=edgemode)
##   })
## 
## setAs("graphNEL","simpleGraph",function(from,to){
##   require(graph)
## 
##   ## apparently not working
##   
##   N <- nodes(from)
##   A <- edges(from)
##   mode <- edgemode(p)
## 
##   A <- lapply(A, function(i) match(i, N))
##   
##   if (mode=="directed")
##     A <- lapply(A,function(x) lapply(x,d))
##   else
##     A <- lapply(A,function(x) lapply(x,u))
## 
##   I <- new("adjacencyList",A)
##   X <- as(I,"adjacencyMatrix")
## 
##   new(to,adjacencyMatrix=X)
##   })

## mathgraph
setOldClass("mathgraph")

setAs("simpleGraph","mathgraph",function(from,to) {

  I <- incidenceList(from)
  M <- matrix( unlist(lapply(I@E,unlist)), ncol=2, byrow=TRUE)
  colnames(M) <- c("e1","e2")
  directed <- unlist(lapply(I@E,is,"directedEdge"))

  X <- adjacencyMatrix(from)
  rsum <- apply(X,2,sum)
  csum <- apply(X,1,sum)
  idx.iso <- (1:ncol(X))[rsum==0&csum==0]
  if (length(idx.iso)>0) {
    M <- rbind(M,matrix(rep(idx.iso,2),nrow=length(idx.iso)))
  }
  
  attr(M,"directed") <- directed
  class(M) <- "mathgraph"
  return(M)
})

setAs("mathgraph","simpleGraph",function(from,to) {
  cat("Not implemented, yet!\n")
})

setMethod("display","simpleGraph",function(x,...) {
   require(mathgraph)
   plot(as(x,"mathgraph"),...)
 })

