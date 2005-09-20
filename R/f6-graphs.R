## f6-graphs.R --- 
## Author          : Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca
## Created On      : Fri Jun 24 10:40:00 2005
## Last Modified By: Luca La Rocca
## Last Modified On: Sat Jun 25 12:19:00 2005
## Update Count    : 4
## Status          : Unknown, Use with caution!
######################################################

# keeping default 'initialize' and 'show' methods

# MAYBE property checking methods for graphs

# MAYBE extractor methods for graphs

# MAYBE 'name' methods for graphs

# see file 'f7-operators.R' for '+/-' methods

## -----------------------------------------------------------
## Getting the representations from a graph
## -----------------------------------------------------------

# getting the adjacency matrix of a simple graph
setMethod("adjacencyMatrix", "simpleGraph",
          function(object, ...) {
          if (!isEmpty(object@adjacencyMatrix))
            object@adjacencyMatrix
          else if (!isEmpty(object@incidenceList)) {
            as(object@incidenceList,"adjacencyMatrix")
          } else if (!isEmpty(object@adjacencyList)) 
            as(object@adjacencyList,"adjacencyMatrix")
          else if (!isEmpty(object@incidenceMatrix))
            as(object@incidenceMatrix,"adjacencyMatrix")
        })
# an 'adjacencyMatrix' object is returned

# getting the adjacency list of a multigraph
setMethod("adjacencyList", "multiGraph",
          function(object, ...) {
            if (!isEmpty(object@adjacencyList))
              object@adjacencyList
            else if(is(object,"simpleGraph") && (!isEmpty(object@adjacencyMatrix)))
              as(object@adjacencyMatrix,"adjacencyList")
            else if (!isEmpty(object@incidenceMatrix))
              as(object@incidenceMatrix,"adjacencyList")
            else if (!isEmpty(object@incidenceList))
              as(object@incidenceList,"adjacencyList")
          })
# an 'adjacencyList' object is returned

# getting the incidence matrix of a general graph
setMethod("incidenceMatrix", "generalGraph",
          function(object, ...) {
            if (!isEmpty(object@incidenceMatrix))
              object@incidenceMatrix
            else if (is(object,"simpleGraph") && (!isEmpty(object@adjacencyMatrix)))
              as(object@adjacencyMatrix,"incidenceMatrix")
            else if (is(object,"multiGraph") && (!isEmpty(object@adjacencyList)))
              as(object@adjacencyList,"incidenceMatrix")
            else if (!isEmpty(object@incidenceList))
              as(object@incidenceList,"incidenceMatrix")
          })
# an 'incidenceMatrix' object is returned

# getting the incidence list of any graph
setMethod("incidenceList", "anyGraph",
          function(object, ...) {
            if (!isEmpty(object@incidenceList)) 
              object@incidenceList
            else if (is(object,"simpleGraph") && (!isEmpty(object@adjacencyMatrix)))
              as(object@adjacencyMatrix,"incidenceList")
            else if (is(object,"multiGraph") && (!isEmpty(object@adjacencyList)))
              as(object@adjacencyList,"incidenceList")
            else if (is(object,"generalGraph") && (!isEmpty(object@incidenceMatrix)))
              as(object@incidenceMatrix,"incidenceList")
          })
# an 'incidenceList' object is returned

## ---------------------
## Replacement methods
## ---------------------

# setting the adjacency matrix of a simple graph
setReplaceMethod("adjacencyMatrix", "simpleGraph",
                 function(x, force=TRUE,value){
                   if (force) {
                     x@adjacencyMatrix <- value
                     x@incidenceList <- new("incidenceList")
                     x@incidenceMatrix <- new("incidenceMatrix")
                     x@adjacencyList <- new("adjacencyList")
                   } else if(areTheSame(adjacencyMatrix(x),value))
                       x@adjacencyMatrix <- value
                     else
                       warning("Not matching adjacency matrix")
                   return(x)
                 })

# setting the adjacency list of a multigraph
setReplaceMethod("adjacencyList", "multiGraph",
                 function(x, force = TRUE, value){
                   if (force) {
                     x@adjacencyList <- value
                     x@incidenceList <- new("incidenceList")
                     x@incidenceMatrix <- new("incidenceMatrix")
                     if(is(x,"simpleGraph")) x@adjacencyMatrix <- new("adjacencyMatrix")
                   } else if(areTheSame(adjacencyList(x),value))
                       x@adjacencyList <- value
                     else
                       warning("Not matching adjacency list")
                   return(x)
                 })

# setting the incidence matrix of a general graph
setReplaceMethod("incidenceMatrix", "generalGraph",
                 function(x, force = TRUE, value){
                   if (force) {
                     x@incidenceMatrix <- value
                     x@incidenceList <- new("incidenceList")
                     if(is(x,"multiGraph")) x@adjacencyList <- new("adjacencyList")
                     if(is(x,"simpleGraph")) x@adjacencyMatrix <- new("adjacencyMatrix")
                   } else if(areTheSame(incidenceMatrix(x),value))
                       x@incidenceMatrix <- value
                     else
                       warning("Not matching incidence matrix")
                   return(x)
                 })

# setting the incidence list of any graph
setReplaceMethod("incidenceList", "anyGraph",
                 function(x, force = TRUE, value){
                   if (force) {
                     x@incidenceList <- value
                     if(is(x,"generalGraph"))x@incidenceMatrix <- new("incidenceMatrix")
                     if(is(x,"multiGraph")) x@adjacencyList <- new("adjacencyList")
                     if(is(x,"simpleGraph")) x@adjacencyMatrix <- new("adjacencyMatrix")
                   } else if(areTheSame(incidenceList(x),value))
                       x@incidenceList <- value
                     else
                       warning("Not matching incidence list")
                   return(x)
                 })

## -----------------------------------------------------------
## Typecasting between graphs
## -----------------------------------------------------------

## from bottom to top

# typecasting from 'simpleGraph' to 'multiGraph'
setAs("simpleGraph","multiGraph",function(from,to) {
  if (!isEmpty(from@incidenceList))
    return( new("multiGraph", incidenceList=from@incidenceList))
  else if(!isEmpty(from@incidenceMatrix))
    return( new("multiGraph", incidenceMatrix=from@incidenceMatrix))
  else if(!isEmpty(from@adjacencyList))
    return( new("multiGraph", adjacencyList=from@adjacencyList))
  else
    return( new("multiGraph", adjacencyList=as(from@adjacencyMatrix,"adjacencyList")))
})

# typecasting from 'simpleGraph' to 'generalGraph'
setAs("simpleGraph","generalGraph",function(from,to) {
  if (!isEmpty(from@incidenceList))
    return( new("generalGraph", incidenceList=from@incidenceList))
  else if(!isEmpty(from@incidenceMatrix))
    return( new("generalGraph", incidenceMatrix=from@incidenceMatrix))
  else if(!isEmpty(from@adjacencyList))
    return( new("generalGraph", incidenceMatrix=as(from@adjacencyList,"incidenceMatrix")))
  else
    return( new("generalGraph", incidenceMatrix=as(from@adjacencyMatrix,"incidenceMatrix")))
})

# typecasting from 'simpleGraph' to 'anyGraph'
setAs("simpleGraph","anyGraph",function(from,to) {
  if (!isEmpty(from@incidenceList)) 
    return( new("anyGraph", incidenceList=from@incidenceList))
  else if(!isEmpty(from@incidenceMatrix))
    return( new("anyGraph", incidenceList=as(from@incidenceMatrix,"incidenceList")))
  else if(!isEmpty(from@adjacencyList))
    return( new("anyGraph", incidenceList=as(from@adjacencyList,"incidenceList")))
  else 
    return( new("anyGraph", incidenceList=as(from@adjacencyMatrix,"incidenceList")))
})

# typecasting from 'multiGraph' to 'generalGraph'
setAs("multiGraph","generalGraph",function(from,to) {
  if (!isEmpty(from@incidenceList))
    return( new("generalGraph", incidenceList=from@incidenceList))
  else if(!isEmpty(from@incidenceMatrix))
    return( new("generalGraph", incidenceMatrix=from@incidenceMatrix))
  else 
    return( new("generalGraph", incidenceMatrix=as(from@adjacencyList,"incidenceMatrix")))
})

# typecasting from 'multiGraph' to 'anyGraph'
setAs("multiGraph","anyGraph",function(from,to) {
  if (!isEmpty(from@incidenceList))
    return( new("anyGraph", incidenceList=from@incidenceList))
  else if(!isEmpty(from@incidenceMatrix))
    return( new("anyGraph", incidenceList=as(from@incidenceMatrix,"incidenceList")))
  else 
    return( new("anyGraph", incidenceList=as(from@adjacencyList,"incidenceList")))
})

# typecasting from 'generalGraph' to 'anyGraph'
setAs("generalGraph","anyGraph",function(from,to) {
  if (!isEmpty(from@incidenceList)) 
    return( new("anyGraph", incidenceList=from@incidenceList))
  else 
    return( new("anyGraph", incidenceList=as(from@incidenceMatrix,"incidenceList")))
})

## from top to bottom

# typecasting from 'anyGraph' to 'generalGraph'
setAs("anyGraph","generalGraph",function(from,to) {
  warning("Coercing anyGraph to generalGraph, possibly loosing information")
    return( new("generalGraph", incidenceMatrix=as(incidenceList(from),"incidenceMatrix")) )
})

# typecasting from 'anyGraph' to 'multiGraph'
setAs("anyGraph","multiGraph",function(from,to) {
  warning("Coercing anyGraph to multiGraph, possibly loosing information")
    return( new("multiGraph",
                adjacencyList=as(incidenceList(from),"adjacencyList")) )
})

# typecasting from 'anyGraph' to 'simpleGraph'
setAs("anyGraph","simpleGraph",function(from,to) {
  warning("Coercing anyGraph to simpleGraph, possibly loosing information")
    return( new("simpleGraph", adjacencyMatrix=as(incidenceList(from),"adjacencyMatrix")) )
})

# typecasting from 'generalGraph' to 'multiGraph'
setAs("generalGraph","multiGraph",function(from,to) {
  warning("Coercing generalGraph to multiGraph, possibly loosing information")
    return( new("multiGraph",
                adjacencyList=as(incidenceMatrix(from),"adjacencyList")) )
})

# typecasting from 'generalGraph' to 'simpleGraph'
setAs("generalGraph","simpleGraph",function(from,to) {
  warning("Coercing generalGraph to simpleGraph, possibly loosing information")
    return( new("simpleGraph", adjacencyMatrix=as(incidenceMatrix(from),"adjacencyMatrix")) )
})

# typecasting from 'multiGraph' to 'simpleGraph'
setAs("multiGraph","simpleGraph",function(from,to) {
  warning("Coercing multiGraph to simpleGraph, possibly loosing information")
    return( new("simpleGraph", adjacencyMatrix=as(adjacencyList(from),"adjacencyMatrix")) )
})
