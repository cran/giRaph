## f7-operators.R --- 
## Author          : Jens Henrik Badsberg, Claus Dethlefsen, Luca la Rocca
## Created On      : Wed Oct 27 10:59:27 2004
## Last Modified By: Luca La Rocca
## Last Modified On: Sun Jun 26 09:18:00 2005
## Update Count    : 135
## Status          : Unknown, Use with caution!
######################################################

## ----------------------------------------------
## representation +/- vertex set
## ----------------------------------------------

# operation 'incidenceList' + 'vertexSet'
setMethod("+",signature=c("incidenceList","vertexSet"),
          function(e1,e2) {
            e1@V<-e1@V+e2
            return(e1)
          } # end of function
         ) # end of setMethod
# note that it is not necessary to recode the edges

# operation 'incidenceList' - 'vertexSet'
setMethod("-",signature=c("incidenceList","vertexSet"),
          function(e1,e2) {
            Vnames<-names(e1)
            keepV<-match(setdiff(Vnames,names(e2)),Vnames)
            return(e1[keepV])
          } #Êend of function
         ) # end of setMethod
# note that in general some edges are dropped

# operation 'incidenceMatrix' + 'vertexSet'
setMethod("+",signature=c("incidenceMatrix","vertexSet"),
          function(e1,e2) {
            newids<-setdiff(names(e2),names(e1))
            addN<-length(newids)
            if (addN>0){
              oldC<-ncol(e1@.Data)
              e1@.Data <- cbind(e1@.Data,matrix(0,nrow(e1@.Data),addN))
              colnames(e1@.Data) <- c(colnames(e1@.Data)[1:oldC],newids)
            } # end of if
            return(e1)
          } # end of function
         ) #Êend of setMethod
# note that the new vertices will be isolated vertices

# operation 'incidenceMatrix' - 'vertexSet'
setMethod("-",signature=c("incidenceMatrix","vertexSet"),
          function(e1,e2) {
            Vnames<-names(e1)
            Vkeep<-match(setdiff(Vnames,names(e2)),Vnames)
            if(isEmpty(Vkeep))
                Ekeep<-rep(FALSE,nrow(e1))
            else
                Ekeep<-apply(e1,1,function(x){sum(x[-Vkeep])==0})
            colnames(e1@.Data)<-NULL
            e1@.Data<-matrix(e1@.Data[Ekeep,Vkeep],sum(Ekeep),length(Vkeep))
            colnames(e1@.Data)<-Vnames[Vkeep]
            return(e1)
          } # end of function
         ) # end of setMethod
# note that in general some edges are dropped

# operation 'adjacencyList' + 'vertexSet'
setMethod("+",signature=c("adjacencyList","vertexSet"),
          function(e1,e2) {
            Vnames<-names(e1)
            Vadd<-setdiff(names(e2),Vnames)
            e1@.Data<-c(e1@.Data,rep(list(list()),length(Vadd)))
            names(e1)<-c(Vnames,Vadd)
            return(e1)
          } # end of function
         ) # end of setMethod
# note that the new vertices will be isolated vertices

# operation 'adjacencyList' - 'vertexSet'
setMethod("-",signature=c("adjacencyList","vertexSet"),
          function(e1,e2) {
            Vnames<-names(e1)
            return(e1[match(setdiff(Vnames,names(e2)),Vnames)])
          } # end of function
        ) # end of setMethod
# note that in general some edges are dropped

# operation 'adjacencyMatrix' + 'vertexSet'
setMethod("+",signature=c("adjacencyMatrix","vertexSet"),
          function(e1,e2) {
            newids<-setdiff(names(e2),names(e1))
            addN<-length(newids)
            if (addN>0){
              oldN<-nrow(e1@.Data)
              e1@.Data <- cbind(e1@.Data,matrix(0,oldN,addN))
              e1@.Data <- rbind(e1@.Data,matrix(0,addN,oldN+addN))
              rownames(e1@.Data) <- c(rownames(e1@.Data)[1:oldN],newids)
              colnames(e1@.Data) <- rownames(e1@.Data)
            } # end if
            return(e1)
          } # end of function
         ) # end of setMethod
# note that the new vertices will be isolated vertices

# operation 'adjacencyMatrix' - 'vertexSet'
setMethod("-",signature=c("adjacencyMatrix","vertexSet"),
          function(e1,e2) {
            Vnames<-names(e1)
            keepV<-match(setdiff(Vnames,names(e2)),Vnames)
            newN<-length(keepV)
            rownames(e1@.Data)<-NULL
            colnames(e1@.Data)<-NULL
            e1@.Data<-matrix(e1@.Data[keepV,keepV],newN,newN)
            rownames(e1@.Data)<-Vnames[keepV]
            colnames(e1@.Data)<-rownames(e1@.Data)
            return(e1)
          } # end of function
         ) # end of setMethod
# note that in general some edges are dropped

## ----------------------------------------------
## representation +/- edge
## ----------------------------------------------

# operation 'incidenceList' + 'edge'
setMethod("+",signature=c("incidenceList","edge"),
          function(e1,e2) {
          if(maxId(e2)<=length(e1@V)) e1@E<-e1@E+e2
          return(e1)
          } # end of function
         ) # end of setMethod
# the edge is added to the multi-set of edges (if meaningful for the vertex set)

# operation 'incidenceList' - 'edge'
setMethod("-",signature=c("incidenceList","edge"),function(e1,e2) return(e1@E-e2))
# the edge is removed from the multi-set of edges (if present)

# operation 'incidenceMatrix' + 'undirectedEdge'
setMethod("+",signature=c("incidenceMatrix","undirectedEdge"),
          function(e1,e2) {
            if((length(e2)>0)&&max(e2)<=ncol(e1)){ # add
                e1@.Data<-rbind(e1@.Data,rep(0,ncol(e1)))
                e1@.Data[nrow(e1@.Data),e2@.Data]<-1
            } # end of if
            return(e1)
          } # end of function
         ) # end of setMethod
# the edge is added to the multi-set of edges
# (if non-empty and meaningful for the vertex set)

# operation 'incidenceMatrix' - 'undirectedEdge'
setMethod("-",signature=c("incidenceMatrix","undirectedEdge"),
          function(e1,e2) {
            if((length(e2)>0)&&(max(e2)<=ncol(e1))&&(nrow(e1)>0)){
                eline<-rep(0,ncol(e1))
                eline[e2@.Data]<-1
                where<-match(T,apply(e1@.Data,1,function(x){all(x==eline)}))
                if(!is.na(where)) e1@.Data<-matrix(e1@.Data[-where,],nrow(e1)-1,ncol(e1))
            } # end of if
            return(e1)
          } # end of function
         ) # end of setMethod
# the edge is removed from the multi-set of edges
# (if non-empty, meaningful for the vertex set and present)

# operation 'incidenceMatrix' + 'directedEdge'
setMethod("+",signature=c("incidenceMatrix","directedEdge"),
          function(e1,e2) {
            if((length(e2)>1)&&(maxId(e2)<=ncol(e1))){ # add
                e1@.Data<-rbind(e1@.Data,rep(0,ncol(e1)))
                e1@.Data[nrow(e1@.Data),unlist(e2)]<-rep(1:length(e2),unlist(lapply(e2,length)))
            } # end of if
            return(e1)
          } # end of function
         ) # end of setMethod
# the edge is added to the multi-set of edges
# (if proper and meaningful for the vertex set)

# operation 'incidenceMatrix' - 'directedEdge'
setMethod("-",signature=c("incidenceMatrix","directedEdge"),
          function(e1,e2) {
            if((length(e2)>1)&&(maxId(e2)<=ncol(e1))&&(nrow(e1)>0)){
                eline<-rep(0,ncol(e1))
                eline[unlist(e2)]<-rep(1:length(e2),unlist(lapply(e2,length)))
                where<-match(T,apply(e1@.Data,1,function(x){all(x==eline)}))
                if(!is.na(where)) e1@.Data<-matrix(e1@.Data[-where,],nrow(e1)-1,ncol(e1))
            } # end of if
            return(e1)
          } # end of function
         ) # end of setMethod
# the edge is removed from the multi-set of edges
# (if non-empty, meaningful for the vertex set and present)

# operation 'adjacencyList' + 'undirectedEdge'
setMethod("+",signature=c("adjacencyList","undirectedEdge"),
          function(e1,e2) {
          if((length(e2)==2)&&(max(e2)<=length(e1))){ # add
            one<-e2[1]
            two<-e2[2]
            e1[[one]]$ne<-c(e1[[one]]$ne,two)
            e1[[two]]$ne<-c(e1[[two]]$ne,one)
          }else if((length(e2)==1)&&(e2<=length(e1))){ # loop
            e1[[e2]]$ne<-c(e1[[e2]]$ne,e2)
          } #Êend of if-else if
          return(e1)
          } # end of function
         ) # end of setMethod
# the edge is added to the multi-set of edges
# (if ordinary and meaningful for the vertex set)

# operation 'adjacencyList' - 'undirectedEdge'
setMethod("-",signature=c("adjacencyList","undirectedEdge"),
          function(e1,e2) {
            if((length(e2))==1&&(e2<=length(e1))){ # loop
                try<-match(e2,e1[[e2]]$ne) # first match
                if(!is.na(try)) e1[[e2]]$ne<-e1[[e2]]$ne[-try]
            }else if((length(e2)==2)&&(max(e2)<=length(e1))){
                one<-e2[1]
                two<-e2[2]
                try<-match(two,e1[[one]]$ne) # first match
                if(!is.na(try)){
                    e1[[one]]$ne<-e1[[one]]$ne[-try]
                    e1[[two]]$ne<-e1[[two]]$ne[-match(one,e1[[two]]$ne)]
                } #Êend of if
            } # end of if-else if
            return(e1)
          } # end of function
         ) # end of setMethod
# the edge is removed from the multi-set of edges (if present)

# operation 'adjacencyList' + 'directedEdge'
setMethod("+",signature=c("adjacencyList","directedEdge"),
          function(e1,e2) {
          if((length(e2)==2)&&(length(unlist(e2))==2)&&(maxId(e2)<=length(e1))){ # add
            one<-e2[[1]]
            two<-e2[[2]]
            e1[[one]]$ch<-c(e1[[one]]$ch,two)
            e1[[two]]$pa<-c(e1[[two]]$pa,one)
          } #Êend of if
          return(e1)
          } # end of function
         ) # end of setMethod
# the edge is added to the multi-set of edges
# (if ordinary and meaningful for the vertex set)

# operation 'adjacencyList' - 'directedEdge'
setMethod("-",signature=c("adjacencyList","directedEdge"),
          function(e1,e2) {
            if((length(e2)==2)&&(length(unlist(e2))==2)&&(maxId(e2)<=length(e1))){
                one<-e2[[1]]
                two<-e2[[2]]
                try<-match(two,e1[[one]]$ch) # first match
                if(!is.na(try)){
                    e1[[one]]$ch<-e1[[one]]$ch[-try]
                    e1[[two]]$pa<-e1[[two]]$pa[-match(one,e1[[two]]$pa)]
                } #Êend of if
            } # end of if
            return(e1)
          } # end of function
         ) # end of setMethod
# the edge is removed from the multi-set of edges (if present)

# operation 'adjacencyMatrix' + 'undirectedEdge'
setMethod("+",signature=c("adjacencyMatrix","undirectedEdge"),
          function(e1,e2) {
          if((length(e2)==2)&&(max(e2)<=nrow(e1))){ # add
            one<-e2[1]
            two<-e2[2]
            e1@.Data[one,two]<-1
            e1@.Data[two,one]<-1
          } #Êend of if
          return(e1)
          } # end of function
         ) # end of setMethod
# the edge is added to the set of edges (if it is ordinary,
# it is not a loop, and it is meaningful for the vertex set)

# operation 'adjacencyMatrix' - 'undirectedEdge'
setMethod("-",signature=c("adjacencyMatrix","undirectedEdge"),
          function(e1,e2) {
          if((length(e2)==2)&&(max(e2)<=nrow(e1))){ # remove
            one<-e2[1]
            two<-e2[2]
            e1@.Data[one,two]<-0
            e1@.Data[two,one]<-0
          } #Êend of if
          return(e1)
          } # end of function
         ) # end of setMethod
# the edge is removed from the set of edges

# operation 'adjacencyMatrix' + 'directedEdge'
setMethod("+",signature=c("adjacencyMatrix","directedEdge"),
          function(e1,e2) {
          if((length(e2)==2)&&(length(unlist(e2))==2)&&(maxId(e2)<=nrow(e1))){ # add
            one<-e2[[1]]
            two<-e2[[2]]
            e1@.Data[one,two]<-1
          } #Êend of if
          return(e1)
          } # end of function
         ) # end of setMethod
# the edge is added to the set of edges (if it is ordinary,
# and it is meaningful for the vertex set)

# operation 'adjacencyMatrix' - 'directedEdge'
setMethod("-",signature=c("adjacencyMatrix","directedEdge"),
          function(e1,e2) {
          if((length(e2)==2)&&(length(unlist(e2))==2)&&(maxId(e2)<=nrow(e1))){ # remove
            one<-e2[[1]]
            two<-e2[[2]]
            e1@.Data[one,two]<-0
          } #Êend of if
          return(e1)
          } # end of function
         ) # end of setMethod
# the edge is removed from the set of edges

# MAYBE '+/-' methods and additional operators for graphs, e.g.
# Graph1 + Graph2 could be the union of graphs
# Graph1[1:10] could be subGraph induced by the first 10 nodes.
# Of course a vertexSet could be provided (or a named vector/list). 
# Graph1 - Graph2 could remove nodes (and edges) in Graph2 from Graph1.
# Graph1 <= Graph2  Test if Graph1 is a subgraph of Graph2
# Graph1 >= Graph2  Test if Graph2 is a subgraph of Graph1
