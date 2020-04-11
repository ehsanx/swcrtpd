library(arrangements)

### The code was created by Liang Xu
temp <- c(6,4,2) # # number of cluster for each type (S,L) or (S,M,L)
M <- temp
N = rep(3,4) # #of clus transit per period
p = length(N) 


X0=permutations(c(0:min(temp[3],sum(temp)/p)),k=(p-1),replace=T)
X1<-cbind(X0,apply(X0,1,function(x){ifelse(sum(x)<=temp[3]&&sum(x)>=temp[3]-sum(temp)/p,temp[3]-sum(x),NA)}))
X1<-X1[!rowSums(!is.finite(X1)),] #X1 gives all the ways the large clusters can be assigned

X2=permutations(c(0:min(temp[2],sum(temp)/p)),k=(p-1),replace=T)
X3<-cbind(X2,apply(X2,1,function(x){ifelse(sum(x)<=temp[2]&&sum(x)>=temp[2]-sum(temp)/p,temp[2]-sum(x),NA)}))
X3<-X3[!rowSums(!is.finite(X3)),] #X3 gives all the ways the medium clusters can be assigned

X6<-matrix(0,ncol=2*p)
for (i in 1:dim(X1)[1]){
  X4<-matrix(X1[i,],nrow = dim(X3)[1],ncol=p,byrow = T)
  X5<-cbind(X4,X3,apply(X3+X4,1,function(x){ifelse(max(x)<=sum(temp)/p&&sum(x)==(sum(temp)-temp[1]),sum(x),NA)}))
  X5<-X5[!rowSums(!is.finite(X5)),]
  X6=rbind(X6,X5[,1:(2*p)]) #X6 is a dummy matrix that contains all the possible unique allocations; columns 1 to p for large clusters, and columns (p+1) to 2p for medium clusters
}

X6<-X6[2:(dim(X6)[1]),]

Xs=matrix(sum(temp)/p,ncol=p,nrow=dim(X6)[1])-X6[,1:p]-X6[,(p+1):(2*p)] #allocations for small clusters
Xl=X6[,1:p] #allocations for medium clusters
Xm=X6[,(p+1):(2*p)] #allocations for large clusters

allocs <- list()
for(i in 1:dim(X6)[1]){
  allocs[[i]] <- rbind(Xl[i,],Xm[i,],Xs[i,])
}

allocs <- array(unlist(allocs), dim = c(length(temp), p, dim(X6)[1])) 


rand <-function(M,N,allocs,k){
  ord <-c()
  for (i in 1:length(M)){
    deno <-c()
    for (j in 1:length(N)){
      temp <-c()
      temp <- rep(j,allocs[i,j,k])
      deno <- c(deno,temp)
    }
    ord <- c(ord,deno)
  }
  return(ord)
}


multi <- function(M,N,allocs,k){
  # k is the kth unique allocation you want to evaluate
  temp <-c()
  final <-c()
  for (i in 1:length(M)){
    for (j in 1:length(N)){
      temp[j] <- factorial(allocs[i,j,k])
      deno <- prod(temp)
    }
    final[i] <- factorial(M[i])/deno
  }
  return(prod(final))
}

Tx <- matrix(ncol=sum(M), nrow=dim(X6)[1])
for(i in 1:dim(X6)[1]){
  Tx[i,]<-rand(M,N,allocs,i)}
# allocations

v <- c()
for(i in 1:dim(X6)[1]){
  v[i]<-multi(M,N,allocs,i)}
w<-v/sum(v)

## weights
