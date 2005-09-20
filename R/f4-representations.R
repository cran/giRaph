## f4-representations.R --- 
## Author          : Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca
## Created On      : Tue Nov 30 16:50:00 2004
## Last Modified By: Luca La Rocca
## Last Modified On: Sun Jun 26 10:18:00 2005
## Update Count    : 16
## Status          : Unknown, Use with caution!
######################################################

## The four representations:
##     G=(V,E): incidenceList
##     I: incidenceMatrix
##     A: adjacencyList
##     X: adjacencyMatrix
##
## Equivalence (~) of these are as follows
##
## anyGraph:     G
## generalGraph: G ~ I
## multiGraph:   G ~ I ~ A
## simpleGraph:  G ~ I ~ A ~ X
##
## where equivalence means that for the given family of graphs
## no information is lost when coercing from one representation to the other.
## Character vertex identifiers are kept as dim-attributes
## on the X and I matrices, and on the A-list.

## construction and visualization

# constructor method for class 'incidenceList'
setMethod("initialize","incidenceList",
                 function(.Object,V=character(0),E=list()){
                     .Object@V<-new("vertexSet",V)
                     .Object@E<-new("edgeList",
                        E[unlist(lapply(E,function(x){isEmpty(x)||maxId(x)<=length(.Object@V)}))])
                     return(.Object)
                 } # end of function
                ) # end of SetMethod
# a valid 'incidenceList' object is returned

# constructor method for class 'incidenceMatrix'
setMethod("initialize","incidenceMatrix",
                 function(.Object,I=matrix(0,nrow=0,ncol=0)){
                     .Object@.Data<-as(I,"matrix")
                     n<-ncol(.Object@.Data) # number of vertices
                     m<-nrow(.Object@.Data) # number of edges
                     if(n>0){ # make valid character vertex identifiers
                        Vnames<-colnames(.Object@.Data)
                        if(is(Vnames,"NULL")||any(duplicated(Vnames)))
                            colnames(.Object)<-make.names(seq(1:n)) # default vertex names
                        else
                            colnames(.Object)<-make.names(Vnames,unique=TRUE) # vertex names from input
                        if(m>0){ # make valid entries
                            for(i in 1:m){ # for all edges
                                nonzeros<-.Object@.Data[i,]!=0
                                    .Object@.Data[i,nonzeros]<-as.numeric(factor(rank(.Object@.Data[i,nonzeros],
                                                                                      ties.method="min")))
                            } # end of for
                            nonempty<-apply(.Object@.Data,1,function(x){sum(x)>0}) #Êfind non-empty edges
                            .Object@.Data<-matrix(.Object@.Data[nonempty,],sum(nonempty),n)
                        } # end of if
                     } # end of if
                     rownames(.Object)<-NULL # there should not be any edge names
                     return(.Object)
                 } # end of function
                ) # end of SetMethod
# a valid 'incidenceMatrix' object is returned

# constructor method for class 'adjacencyList'
setMethod("initialize","adjacencyList",
          function(.Object,id=character(0),pa=list(),ne=list()){
            Vnames<-names(new("vertexSet",id))
            n<-length(Vnames)
            .Object@.Data<-rep(list(list()),n)
            names(.Object)<-Vnames
            if((length(pa)==n||length(pa)==0)&&
               (length(ne)==n||length(ne)==0)){ # consider adding edges
                if(n>0){ # there are vertices
                    if(length(pa)>0){ # parents are given
                        for(i in 1:n){ # add parents
                            aux<-as(pa[[i]],"integer")
                            aux[(aux>0)&(aux<=n)]
                            .Object@.Data[[i]]$pa<-aux
                        } # end of for
                    } # end of if
                    if(length(ne)>0){ # neighbours are given
                        for(i in 1:n){ # add neighbours
                            aux<-as(ne[[i]],"integer")
                            aux<-aux[(aux>0)&(aux<=n)]
                            .Object@.Data[[i]]$ne<-aux
                        } # end of for
                    } # end of if
                    for(i in 1:n){ # add children information
                        for (j in .Object@.Data[[i]]$pa) .Object@.Data[[j]]$ch<-c(.Object@.Data[[j]]$ch,i)
                    } # end of for
                } # end of if
            } # end of if
            return(.Object)
          } # end of function
         ) # end of SetMethod
# a valid 'adjacencyList' object is returned

# constructor method for class 'adjacencyMatrix'
setMethod("initialize","adjacencyMatrix",
                 function(.Object,X=matrix(0,nrow=0,ncol=0)){
                     aux<-as(X,"matrix")
                     n<-min(nrow(aux),ncol(aux))
                     Rnames<-rownames(aux)
                     Cnames<-colnames(aux)
                     if(n>0){ # make valid entries and character vertex identifiers
                        .Object@.Data<-matrix(0,n,n)
                        if(n==1){ # single vertex
                            if(is(Rnames,"NULL")||is(Cnames,"NULL")||(Rnames[1]!=Cnames[1])){
                                rownames(.Object)<-"X1" # default vertex name
                                colnames(.Object)<-rownames(.Object)
                            }else{
                                rownames(.Object)<-make.names(Rnames[1]) # vertex name from input
                                colnames(.Object)<-rownames(.Object)
                            } # end of if-else
                        }else{ # two or more vertices
                            for(i in 1:n) for (j in seq(1,n)[-i]) if(aux[i,j]!=0) .Object@.Data[i,j]<-1
                            if(is(Rnames,"NULL")||is(Cnames,"NULL")||any(Rnames!=Cnames)||any(duplicated(Rnames))){
                                rownames(.Object)<-make.names(seq(1:n)) # default vertex names
                                colnames(.Object)<-rownames(.Object)
                            }else{
                                rownames(.Object)<-make.names(Rnames,unique=TRUE) # vertex names from input
                                colnames(.Object)<-rownames(.Object)
                            } # end of if-else
                        } # end of if-else
                     } # end of if
                     return(.Object)
                 } # end of function
                ) # end of SetMethod
# a valid 'adjacencyMatrix' object is returned

# show method for class 'incidenceList'
setMethod("show","incidenceList",
                 function(object){
                     cat("An object of class \"incidenceList\"",fill=T)
                     cat("V=")
                     show(object@V)
                     cat("E=")
                     show(object@E)
                 } # end of function
                ) # end of setMethod
# a shorter representation than the default one

# show method for class 'adjacencyList'
setMethod("show","adjacencyList",
                 function(object){
                     cat("An object of class \"adjacencyList\"",fill=T)
                     if(!isEmpty(object)){ # something to show
                         Vnames<-names(object)
                         for(i in 1:length(object)){
                             blank<-rep(" ",nchar(Vnames[i]))
                             cat(Vnames[i]," <- {",sep="")
                             cat(Vnames[object[[i]]$pa],sep=",")
                             cat("}",fill=T)
                             cat(blank," -- {",sep="")
                             cat(Vnames[object[[i]]$ne],sep=",")
                             cat("}",fill=T)
                             cat(blank," -> {",sep="")
                             cat(Vnames[object[[i]]$ch],sep=",")
                             cat("}",fill=T)
                         } # end of for
                     } # end of if
                 } # end of function
                ) # end of setMethod
# a shorter representation than the default one

# keeping default show methods for classes 'incidenceMatrix' and 'adjacencyMatrix'

## property checking

# 'isEmpty' method for class 'incidenceList'
setMethod("isEmpty","incidenceList", function(object,...) isEmpty(object@V))
# an 'incidenceList' is empty if such is its 'vertexSet'

# 'isEmpty' method for class 'incidenceMatrix'
setMethod("isEmpty","incidenceMatrix", function(object,...) ncol(object)==0)
# an 'incidenceMatrix' with no columns represents no vertices

# 'isEmpty' method for class 'adiacencyList' is inherited from 'vector'

# 'isEmpty' method for class 'adiacencyMatrix'
setMethod("isEmpty","adjacencyMatrix", function(object,...) nrow(object)==0)
# an 'adjacencyMatrix' is empty if it has no entries

# comparison method for class 'incidenceList'
setMethod("areTheSame",c("incidenceList","incidenceList"),
                 function(x,y){
                     res<-(areTheSame(x@V,y@V))&&(length(x@E)==length(y@E))
                     if(res) # maybe
                        res<-areTheSame(x@E,recode(y@E,y@V,x@V))
                     return(res)
                 } #Êend of function
         ) # end of setMethod
# a 'logical' value answering the question is returned

# comparison method for class 'incidenceMatrix'
setMethod("areTheSame",c("incidenceMatrix","incidenceMatrix"),
                 function(x,y){
                    res<-(setequal(names(x),names(y)))&&(nrow(x)==nrow(y))
                    if(res&&nrow(x)>0){ # maybe and non-trivial
                        y<-y[,names(x)]
                        m<-nrow(x) # number of edges
                        unmatched<-rep(TRUE,m) # refers to 'y'
                        for(i in 1:m){ # match 'x[i,]' in 'y'
                            found<-FALSE # refers to 'x[i,]'
                            for(j in which(unmatched)){ # try all unmatched 'y[j,]'
                                if(all(x[i,]==y[j,])){ # matched
                                    found<-T
                                    unmatched[j]<-FALSE
                                    break
                                 } # end of if
                            } # end of for (j)
                            if(!found){ # not matched
                                res<-FALSE
                                break
                            } # end of if
                        } # end of for (i)
                     res<-res&&!any(unmatched)
                    } # end of if
                    return(res)
                 } # end of function
         ) # end of setMethod
# a 'logical' value answering the question is returned

# comparison method for class 'adjacencyList'
setMethod("areTheSame",c("adjacencyList","adjacencyList"),
                 function(x,y){
                    res<-FALSE
                    if(setequal(names(x),names(y))){ # maybe
                        y<-y[match(names(x),names(y))]
                        res<-rep(NA,length(x))
                        for(i in 1:length(res)){
                            res[i]<-setequal(x[[i]]$ne,y[[i]]$ne)&&
                            setequal(x[[i]]$pa,y[[i]]$pa)&&
                            setequal(x[[i]]$ch,y[[i]]$ch)
                        } # end of for
                        res<-all(res)
                    } #Êend of if
                    return(res)
                 } #Êend of function
         ) # end of setMethod
# a 'logical' value answering the question is returned

# comparison method for class 'adjacencyMatrix'
setMethod("areTheSame",c("adjacencyMatrix","adjacencyMatrix"),
                 function(x,y){
                    Xnames<-names(x)
                    res<-setequal(Xnames,names(y))
                    if(res&&nrow(x)>0){ # maybe and non-trivial
                        res<-all(x==y[Xnames,Xnames])
                    } # end of if
                    return(res)
                 } # end of function
         ) # end of setMethod
# a 'logical' value answering the question is returned

# MAYBE give 'isPresent' method for edges in representations

## extraction

# multi extractor method for class 'incidenceList'
setMethod("[","incidenceList",
          function(x,i,j=NA,drop=NA){
            oldV<-x@V
            x@V<-x@V[i]
            aux<-recode(x@E,oldV,x@V)
            noshorter<-logical(0)
            for(i in 1:length(aux)) noshorter[i]<-(length(unlist(aux[[i]]))==length(unlist(x@E[[i]])))
            x@E<-aux[noshorter]
            return(x)
          } # end of function
         ) # end of setMethod
# the subgraph induced by 'i' is extracted

# single extractor method for class 'incidenceList'
setMethod("[[","incidenceList",
          function(x,i,j=NA,drop=NA) return(x@V[[i]])
         ) # end of setMethod
# the name of the i-th vertex is extracted

# multi extractor method for class 'adjacencyList'
setMethod("[","adjacencyList",
          function(x,i,j=NA,drop=NA){
            A<-new("adjacencyList",id=names(x)[i]) # new empty adjacency list
            n<-length(A) # number of vertices in output
            if(n>0){ # there are vertices in output
                from<-names(x)
                to<-names(A)
                for(i in 1:n){ # deal with each of them
                    j<-match(to[i],from) # find its original position
                    aux<-match(from[x[[j]]$ne],to)
                    A[[i]]$ne<-aux[!is.na(aux)]
                    aux<-match(from[x[[j]]$pa],to)
                    A[[i]]$pa<-aux[!is.na(aux)]
                    aux<-match(from[x[[j]]$ch],to)
                    A[[i]]$ch<-aux[!is.na(aux)]
                } # end of for
            } # end of if
            return(A)
          } # end of function
         ) # end of setMethod
# the subgraph induced by 'i' is extracted

# keeping default single extractor method for class 'adjacencyList'

# keeping default extractor methods for matrix representations

## typecasting

# see file 'f5-conversions.R'

## special

# 'names' method for class 'incidenceList'
setMethod("names", "incidenceList", function(x) names(x@V))
# take the names from the 'vertexSet'

# 'names' method for class 'incidenceMatrix'
setMethod("names", "incidenceMatrix", function(x) colnames(x))
# take the names of the columns

# 'names' method for class 'adjacencyList' is inherited from class 'list'

# 'names' method for class 'adjacencyMatrix'
setMethod("names", "adjacencyMatrix", function(x) colnames(x))
# take the names of the columns

# MAYBE give replacement 'names' methods for the four representations

# see file 'f7-operators.R' for '+/-' methods
