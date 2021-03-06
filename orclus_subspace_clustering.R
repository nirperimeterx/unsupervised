# definition of a function for parameterized data simulation
sim.orclus <- function(k = 3, nk = 100, d = 10, l = 4, sd.cl = 0.05, sd.rest = 1, locshift = 1){
  ### input parameters for data generation
  # k # nk # d # l
  # sd.cl
  # sd.rest # locshift
  #number of clusters
  #observations per cluster
  #original dimension of the data
  #subspace dimension where the clusters are concentrated
  #(univariate) standard deviations for data generation (within cluster subspace concentration) standard deviations in the remaining space
  #parameter of a uniform distribution to sample different cluster means
  x <- NULL
  for(i in 1:k){
    # cluster centers
    apts <- locshift*matrix(runif(l*k), ncol = l)
    # sample points in original space
    xi.original <- cbind(matrix(rnorm(nk * l, sd = sd.cl), ncol=l) + matrix(rep(apts[i,], nk), ncol = l, byrow = TRUE),
                         matrix(rnorm(nk * (d-l), sd = sd.rest), ncol = (d-l)))
    # subspace generation
    sym.mat <- matrix(nrow=d, ncol=d)
    for(m in 1:d){
      for(n in 1:m){
        sym.mat[m,n] <- sym.mat[n,m] <- runif(1)
      }
    }
    subspace <- eigen(sym.mat)$vectors
    # transformation
    xi.transformed <- xi.original %*% subspace
    x <- rbind(x, xi.transformed)
  }
  clids <- rep(1:k, each = nk)
  result <- list(x = x, cluster = clids)
  return(result)
}
# simulate data of 2 classes where class 1 consists of 2 subclasses
simdata <- sim.orclus(k = 3, nk = 200, d = 15, l = 4, sd.cl = 0.05, sd.rest = 1, locshift = 1)
x <- simdata$x
y <- c(rep(1,400), rep(2,200))
res <- orclass(x, y, k = 3, l = 4, k0 = 15, a = 0.75)
res
# compare results
table(res$predict.train$class, y)
