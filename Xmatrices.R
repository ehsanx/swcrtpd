cal_X <- function(M,cluster.size,SDB){
### M=c(S,L) or M=c(S,M,L)
### cluster.size inputs as the same order as M
  clus.size.list <- c()
  for (i in 1:length(M)){
    temp <- sample(c(floor(cluster.size[i]),floor(cluster.size[i])+1),
                   prob= c(1-(cluster.size[i]-floor(cluster.size[i])),
                           (cluster.size[i]-floor(cluster.size[i]))),
                   M[i], replace=T)
    clus.size.list <- c(temp,clus.size.list)
  }
  # Randomly pick a nearby integer if the cluster size is not
  transit.p <- c()
  for (i in 1:length(clus.size.list)){
    temp.p <-sample(0:4,clus.size.list[i],replace = T)
    transit.p <-c(transit.p,temp.p)
  }
  X1 <- transit.p
  X2 <- clus.size.list
  totclus <- sum(M)
  tots <- sum(clus.size.list)
  clus.id <- rep(1:totclus,clus.size.list)
  clus.mean.temp <- rnorm(sum(M), sd=SDB)
  clus.mean <- rep(clus.mean.temp,clus.size.list)
  #conterfactuals
  y_trt = clus.mean + transit.p + (-0.26) + rnorm(tots)  # simulate outcomes
  y_ctr = clus.mean + transit.p + 0 + rnorm(tots)
  return(list("ID"=clus.id,"transitp"=X1,"clus.size"=X2, "tot"=tots,
              "y_trt"=y_trt,"y_ctr"=y_ctr))
}

XYmtr <- foreach(i=1:10000) %dopar% {try(cal_X(
  ### 10000 was chosen as our simulation for power calculation (in powercal.R) was based on
  ### 10000 runs
  M=c(24,16,8), # number of clusters of each type (S,M,L) or (S,L)
  cluster.size=c(6.5, 20.61, 65.27), # avg cluster size of each type
  SDB_0.01 # cluster level variance
  ),TRUE)}
