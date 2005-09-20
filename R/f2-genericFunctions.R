## f2-genericFunctions.R --- 
## Author          : Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca
## Created On      : Tue Nov 30 14:23:00 2004
## Last Modified By: Luca La Rocca
## Last Modified On: Sun Jun 26 09:00:00 2005
## Update Count    : 10
## Status          : Unknown, Use with caution!
######################################################

# checking whether an object is empty
if (!isGeneric("isEmpty")) {
    if (is.function("isEmpty"))
        fun <- isEmpty
    else
        fun <- function(object,...) standardGeneric("isEmpty")
    setGeneric("isEmpty", fun)
} # end of if

# isEmpty method for class 'vector'
setMethod("isEmpty","vector",function(object,...){length(object)==0})
# a 'logical' value answering the question is returned

# 'isEmpty' method for class 'NULL'
setMethod("isEmpty","NULL",function(object,...) TRUE)
# a 'NULL' object is always empty

###

# checking whether two objects represent the same mathematical entity
if (!isGeneric("areTheSame")) {
    if (is.function("areTheSame"))
        fun <- areTheSame
    else
        fun <- function(x,y) standardGeneric("areTheSame")
    setGeneric("areTheSame", fun)
} # end of if

###

# checking whether an object is present in another object
if (!isGeneric("isPresent")) {
    if (is.function("isPresent"))
        fun <- isPresent
    else
        fun <- function(el,ou) standardGeneric("isPresent")
    setGeneric("isPresent", fun)
} # end of if

###

# getting the character identifiers of an object
if (!isGeneric("names")) {
    if (is.function("names"))
        fun <- names
    else
        fun <- function(x) standardGeneric("names")
    setGeneric("names", fun)
} # end of if

###

# getting the maximum numeric identifier of an object
if (!isGeneric("maxId")) {
    if (is.function("maxId"))
        fun <- maxId
    else
        fun <- function(x) standardGeneric("maxId")
    setGeneric("maxId", fun)
} # end of if

###

# recoding an object from a source code to a destination code
if (!isGeneric("recode")) {
    if (is.function("recode"))
        fun <- recode
    else
        fun <- function(object,src,dst) standardGeneric("recode")
    setGeneric("recode", fun)
} # end of if

###

# getting the incidence list
if (!isGeneric("incidenceList")) {
    if (is.function("incidenceList"))
      fun <- incidenceList
    else
      fun <- function(object, ...) standardGeneric("incidenceList")
    setGeneric("incidenceList", fun)
  } # end of if

# getting the incidence matrix
if (!isGeneric("incidenceMatrix")) {
    if (is.function("incidenceMatrix"))
      fun <- incidenceMatrix
    else
      fun <- function(object, ...) standardGeneric("incidenceMatrix")
    setGeneric("incidenceMatrix", fun)
  } # end of if

# getting the adjacency list
if (!isGeneric("adjacencyList")) {
    if (is.function("adjacencyList"))
      fun <- adjacencyList
    else
      fun <- function(object, ...) standardGeneric("adjacencyList")
    setGeneric("adjacencyList", fun)
  } # end of if

# getting the adjacency matrix
if (!isGeneric("adjacencyMatrix")) {
    if (is.function("adjacencyMatrix"))
      fun <- adjacencyMatrix
    else
      fun <- function(object, ...) standardGeneric("adjacencyMatrix")
    setGeneric("adjacencyMatrix", fun)
  } # end of if

###

# setting the incidence list
if (!isGeneric("incidenceList<-")) {
    if (is.function("incidenceList<-"))
      fun <- incidenceList
    else
      fun <- function(x, force=TRUE, value) standardGeneric("incidenceList<-")
    setGeneric("incidenceList<-", fun)
  } # end of if

# setting the incidence matrix
if (!isGeneric("incidenceMatrix<-")) {
    if (is.function("incidenceMatrix<-"))
      fun <- incidenceMatrix
    else
      fun <- function(x, force=TRUE, value) standardGeneric("incidenceMatrix<-")
    setGeneric("incidenceMatrix<-", fun)
  } # end of if

# setting the adjacency list
if (!isGeneric("adjacencyList<-")) {
    if (is.function("adjacencyList<-"))
      fun <- adjacencyList
    else
      fun <- function(x, force=TRUE, value) standardGeneric("adjacencyList<-")
    setGeneric("adjacencyList<-", fun)
  } # end of if

# setting the adjacency matrix
if (!isGeneric("adjacencyMatrix<-")) {
    if (is.function("adjacencyMatrix<-"))
      fun <- adjacencyMatrix
    else
      fun <- function(x, force=TRUE, value) standardGeneric("adjacencyMatrix<-")
    setGeneric("adjacencyMatrix<-", fun)
  } # end of if

###

# 'dynamicGraph'
if (!isGeneric("dynamic.Graph")) {
  if (is.function("dynamic.Graph"))
    fun <- dynamic.Graph
  else
    fun <- function(object, ...) standardGeneric("dynamic.Graph")
  setGeneric("dynamic.Graph", fun)
}

# 'display'
if (!isGeneric("display")) {
    if (is.function("display"))
      fun <- display
    else
      fun <- function(x,...) standardGeneric("display")
    setGeneric("display", fun)
  }

###

if (exists("fun")) rm(fun)
