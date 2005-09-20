## f5-conversions.R --- 
## Author          : Jens Henrik Badsberg, Claus Dethlefsen, Luca La Rocca
## Created On      : Fri Jun 24 10:55:00 2005
## Last Modified By: Claus Dethlefsen
## Last Modified On: Sat Sep 17 08:00:02 2005
## Update Count    : 6
## Status          : Unknown, Use with caution!
######################################################

## -----------------------------------------------------------
## CONVERSIONS between representations
## -----------------------------------------------------------
#f\t|  X  |  A  |  I  |  G
#X  |  *  |  /  |  /  |  +
#A  |  /  |  *  |  /  | (+) 
#I  |  +  |  +  |  *  | (+)
#G  |  +  | (+) |  +  |  *

## Note that conversions from a more general representation
## to a simpler representation silently (i.e. no warning)
## drop all edges not "available" in the new representation.

## ---------------------------------------------------
## 'incidenceList' -> ...
## ---------------------------------------------------

### typecasting from 'incidenceList' to 'incidenceMatrix'
setAs("incidenceList", "incidenceMatrix", function(from,to) {
  
  Vnames <- names(from)
  n <- length(Vnames)
  E <- from@E
  m <- length(E)
  
  I <- matrix(0, ncol = n,nrow=m)
  colnames(I) <- Vnames
  
  if (m>0) {
    counter <- 1
    for (e in 1:m) {
      edge <- E[[e]]
      if (is(edge,"undirectedEdge")&&!isEmpty(edge)){
          I[counter, edge@.Data ] <- 1
          counter <- counter + 1
      } else if (is(edge,"directedEdge")&&length(edge)>1) {
          I[counter, unlist(edge) ] <- rep(1:length(edge),unlist(lapply(edge,length)))
          counter <- counter + 1
      } ## else do nothing (other types of edge are ignored)
    } # end of for '(e in 1:m)'
    if (counter==1) return(new(to,I[-(1:m),]))
    else if (counter==2) return(matrix(I[1,],nrow=1,ncol=n))
    else return(new(to,I[1:(counter-1),]))
  } # end of 'if(m>0)'
  
  return(new(to,I))
})
# an 'incidenceMatrix' object is returned

### typecasting from 'incidenceList' to 'adjacencyList'
setAs("incidenceList","adjacencyList", function(from,to) {

  Vnames <- names(from)
  n <- length(Vnames)
  E <- from@E
  m <- length(E)

  A <- new("adjacencyList",id=Vnames)

  if (m>0) {
    for (h in 1:m) {
      edge <- E[[h]]
      q <- length(unlist(edge))
      if (is(edge,"undirectedEdge")){
          if (q==2) {
              A[[edge@.Data[1]]]$ne[length(A[[edge@.Data[1]]]$ne)+1] <- edge@.Data[2]
              A[[edge@.Data[2]]]$ne[length(A[[edge@.Data[2]]]$ne)+1] <- edge@.Data[1]
          } else if (q==1){
              A[[edge@.Data[1]]]$ne[length(A[[edge@.Data[1]]]$ne)+1] <- edge@.Data[1]
          } # end of "undirected edge"
      } else if (is(edge,"directedEdge")&&length(edge)>1) {
          if (q==2) {
            A[[edge@.Data[[1]]]]$ch[length(A[[edge@.Data[[1]]]]$ch)+1] <- edge@.Data[[2]]
            A[[edge@.Data[[2]]]]$pa[length(A[[edge@.Data[[2]]]]$pa)+1] <- edge@.Data[[1]]
          } # end of "directed edge"
      } ## else do nothing (other types of edge are ignored)
    } # end of 'for (h in 1:m)'
  } # end of 'if(m>0)'
  
  return(A)
})
# an 'adjacencyList' object is returned

### typecasting from 'incidenceList' to 'adjacencyMatrix'
setAs("incidenceList", "adjacencyMatrix", function(from, to) {

  Vnames <- names(from)
  n <- length(Vnames)
  E <- from@E
  m <- length(E)
  
  X <- matrix(0, ncol = n, nrow=n)
  dimnames(X) <- list(Vnames, Vnames)
  
  if (m>0){
    for (i in 1:m) {
      edge <- E[[i]]
      q <- length(unlist(edge))
      if (is(edge,"undirectedEdge")){
        if (q==2){
          X[edge[[1]],edge[[2]]] <- 1
          X[edge[[2]],edge[[1]]] <- 1
        } # end of "undirected edge"
      } else if (is(edge,"directedEdge")&&length(edge)>1){
        if (q==2){
          X[edge[[1]],edge[[2]]] <- 1
        } # end of "directed edge"
      } ## else do nothing (hyperedges are ignored)
        ## note that multiple edges are reduced to a single edge
        ## and that 1->2, 2<-1 becomes 1-2 and so on.
    } # end of 'for (h in 1:m)'
  } # end of 'if(m>0)'

  return(new(to,X))
})
# an 'adjacencyMatrix' object is returned

## ---------------------------------------------------
## 'incidenceMatrix' -> ...
## ---------------------------------------------------

### typecasting from 'incidenceMatrix' to 'incidenceList'
setAs("incidenceMatrix", "incidenceList", function(from,to) {

  Vnames <- names(from)
  n <- length(Vnames)
  m <- nrow(from)
  
  if(n==0) return(new("incidenceList")) # empty
  
  E <- list(NA,m)
    
  if (m>0) {
      for (i in 1:m) {
          edge <- from[i,]
          edgeorder <- edge[!edge==0]
          if (all(edgeorder==1))
              E[[i]] <- new("undirectedEdge",(1:n)[!edge==0])
          else if (!any(duplicated(edgeorder)))
              E[[i]] <- new("directedEdge",(1:n)[!edge==0][sort.list(edgeorder)])
          else {
              res <- rep( list( list()), max(edge) )
              for (j in unique(edgeorder)) {res[[j]] <- (1:n)[edge==j]}
              E[[i]] <- new("directedEdge",res)
          } # end of if-else if-else
      } # end of 'for (i in 1:m)'
  } # end of 'if (m>0)'
  
  return(new("incidenceList",V=Vnames,E=E))

})
# an 'incidenceList' object is returned

### typecasting from 'incidenceMatrix' to 'adjacencyList'
setAs("incidenceMatrix", "adjacencyList", function(from,to){

  Vnames <- names(from)
  n <- length(Vnames)
  m <- nrow(from)
  
  A <- new("adjacencyList",id=Vnames)

  if(m>0){
      for (i in 1:m) {
        edge <- from[i,]
        edgeorder <- edge[!edge==0] 
        q <- length(edgeorder)
        if (all(edgeorder==1)) { # "undirected edge"
              edge <- (1:n)[edge!=0]
              if (q==2) {
                A[[ edge[1] ]]$ne <- c(A[[ edge[1] ]]$ne, edge[2])
                A[[ edge[2] ]]$ne <- c(A[[ edge[2] ]]$ne, edge[1])
              } else if (q==1) {
                A[[ edge[1] ]]$ne <- c(A[[ edge[1] ]]$ne, edge[1])
              } # end of if-else
        } else { # "directed edge"
              if (q==2) {
                start <- (1:n)[edge==1]
                end   <- (1:n)[edge==2]
                A[[ start ]]$ch <- c(A[[ start ]]$ch, end)
                A[[ end ]]$pa   <- c(A[[ end ]]$pa, start)
              } # end of if
        } # end of if-else
      } # end of 'for (i in 1:m)'
  } # end of 'if(m>0)'
  
  return((A))
})
# an 'adjacencyList' object is returned

### typecasting from 'incidenceMatrix' to 'adjacencyMatrix'
setAs("incidenceMatrix", "adjacencyMatrix", function(from,to) {

  Vnames <- names(from)
  n <- length(Vnames)
  m <- nrow(from)
  
  X <- matrix(0,nrow=n,ncol=n)
  dimnames(X) <- list(Vnames,Vnames)
  
  if (m>0) {
    for (i in 1:m) {
      edge <- from[i,]
      if (sum(edge)==3) # "directed edge"
          X[edge==1,edge==2] <- 1
      else if (sum(edge)==2) { # "undirected edge"
        idx <- (1:n)[edge==1]
        X[idx[1],idx[2]] <- 1
        X[idx[2],idx[1]] <- 1
      } # end of if-else if
    } # end of 'for (i in 1:m)'
  } # end of 'if(m>0)'
  
  return(new("adjacencyMatrix",X))
})
# an 'adjacencyMatrix' object is returned

## ---------------------------------------------------
## 'adjacencyList' -> ...
## ---------------------------------------------------

### typecasting from 'adjacencyList' to 'incidenceList'
setAs("adjacencyList","incidenceList", function(from,to) {

  A <- from
  Vnames <- names(A)
  n <- length(Vnames)
  
  if(n==0) # empty
      return(new("incidenceList"))
  else{ # not empty
      E <- list()
      for(i in 1:n){
          a <- A[[i]]
          if (length(a$ne[a$ne>=i])>0)
              for (j in a$ne[a$ne>=i])
                  E[[length(E)+1]] <- new("undirectedEdge",i,j)
          if (length(a$ch)>0)
              for (j in a$ch)
                  E[[length(E)+1]] <- new("directedEdge",i,j)
      } # end of for

  return(new("incidenceList",V=Vnames,E=E))
  } # end of if-else
})
# an 'incidenceList' object is returned

### typecasting from 'adjacencyList' to 'incidenceMatrix'
setAs("adjacencyList", "incidenceMatrix", function(from,to) {

  A <- from
  Vnames <- names(A)
  n <- length(Vnames)

  aux <- list()
  
  if(n>0){ # not empty
      for(i in 1:n){
          a <- A[[i]]
          if (length(a$ne[a$ne>=i])>0)
              for (j in a$ne[a$ne>=i])
                  aux[[length(aux)+1]] <- c(i,j) # undirected edge
          if (length(a$ch)>0)
              for (j in a$ch)
                  aux[[length(aux)+1]] <- list(i,j) # directed edge
      } # end of for
  } # end of if

  m<-length(aux)
  
  I <- matrix(0, ncol = n,nrow=m)
  colnames(I) <- Vnames

  if(m>0){
      for(e in 1:m){
          edge<-aux[[e]]
          if(is(edge,"list")){ # directed edge
              I[e,edge[[1]]]<-1
              I[e,edge[[2]]]<-2
          }else{ # undirected edge
              I[e,edge]<-1
          } # end of if-else
      } # end of for
  } # end of if
  
  return(new(to,I))
})
# an 'incidenceMatrix' object is returned

### typecasting from 'adjacencyList' to 'adjacencyMatrix'
setAs("adjacencyList","adjacencyMatrix", function(from, to) {

  A <- from
  Vnames <- names(A)
  n <- length(Vnames)

  X <- matrix(0, ncol = n, nrow=n)
  dimnames(X) <- list(Vnames, Vnames)

  if(n>0){ # not empty
      for(i in 1:n){
          a <- A[[i]]
          if (length(a$ne[a$ne>i])>0)
              for (j in a$ne[a$ne>i]){ # undirected edges
                  X[i,j]<-1
                  X[j,i]<-1
              } # end of for
          if (length(a$ch)>0)
              for (j in a$ch) # directed edges
                  X[i,j]<-1
      } # end of 'for (i in 1:n)'
  } # end of 'if(n>0)'
  
  return(new(to,X))
  })
# an 'adjacencyMatrix' object is returned

## ---------------------------------------------------
## 'adjacencyMatrix' -> ...
## ---------------------------------------------------

### typecasting from 'adjacencyMatrix' to 'incidenceList'
setAs("adjacencyMatrix","incidenceList", function(from,to) {

  X<-from
  Vnames <- names(X)
  n <- length(Vnames)

  E<-list()

  if(n>1){ # maybe there are edges
  for(i in seq(1,n-1)){
          for(j in seq(i+1,n)){
              if(X[i,j]){ # edge
                  if(X[j,i]){ # undirected
                      E[[length(E)+1]]<-new("undirectedEdge",i,j)
                  }else{ # directed edge
                      E[[length(E)+1]]<-new("directedEdge",i,j)
                  } # end of if-else
              } else if (X[j,i]){ # directed
                  E[[length(E)+1]]<-new("directedEdge",j,i)
              } # end of if-else
          } # end of for (j)
      } # end of for (i)
  } # end of if
  
  new(to,V=Vnames,E=E)
})
# an 'incidenceList' object is returned

### typecasting from 'adjacencyMatrix' to 'incidenceMatrix'
setAs("adjacencyMatrix", "incidenceMatrix", function(from,to) {
  
  X<-from
  Vnames <- names(X)
  n <- length(Vnames)
  
  aux <- list()
  
  if(n>1){ # maybe there are edges
  for(i in seq(1,n-1)){
          for(j in seq(i+1,n)){
              if(X[i,j]){ # edge
                  if(X[j,i]){ # undirected
                      aux[[length(aux)+1]]<-c(i,j)
                  }else{ # directed edge
                      aux[[length(aux)+1]]<-list(i,j)
                  } # end of if-else
              } else if (X[j,i]){ # directed
                  aux[[length(aux)+1]]<-list(j,i)
              } # end of if-else
          } # end of for (j)
      } # end of for (i)
  } # end of if

  m<-length(aux)
  
  I <- matrix(0, ncol = n,nrow=m)
  colnames(I) <- Vnames

  if(m>0){
      for(e in 1:m){
          edge<-aux[[e]]
          if(is(edge,"list")){ # directed edge
              I[e,edge[[1]]]<-1
              I[e,edge[[2]]]<-2
          }else{ # undirected edge
              I[e,edge]<-1
          } # end of if-else
      } # end of for
  } # end of if
  
  return(new(to,I))
})
# an 'incidenceMatrix' object is returned

### typecasting from 'adjacencyMatrix' to 'adjacencyList'
setAs("adjacencyMatrix","adjacencyList", function(from, to) {

  X<-from
  Vnames <- names(X)
  n <- length(Vnames)
  
#  A <- rep(list(list()),n)
#  names(A) <- Vnames
  
  A <- new("adjacencyList",id=Vnames)
  
  if(n>1){ # maybe there are edges
  for(i in seq(1,n-1)){
          for(j in seq(i+1,n)){
              if(X[i,j]){ # edge
                  if(X[j,i]){ # undirected
                      A[[i]]$ne[length(A[[i]]$ne)+1]<-j
					  A[[j]]$ne[length(A[[j]]$ne)+1]<-i
                  }else{ # directed edge
				      A[[i]]$ch[length(A[[i]]$ch)+1]<-j
				      A[[j]]$pa[length(A[[j]]$pa)+1]<-i
                  } # end of if-else
              } else if (X[j,i]){ # directed
				      A[[i]]$pa[length(A[[i]]$pa)+1]<-j
				      A[[j]]$ch[length(A[[j]]$ch)+1]<-i
              } # end of if-else
          } # end of for (j)
      } # end of for (i)
  } # end of if

  return((A))
#  return(new(to,A))
  })
# an 'adjacencyList' object is returned
