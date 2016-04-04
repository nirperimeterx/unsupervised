kmeans_search <- function (clustering.set.in,account){
  #rm(list=setdiff(ls(), c("testing.set","clustering.set.in","dom.data.90min","training.set","clustering.test.set")))
  #clustering.set.in <- clustering.set
  clustering.set.in <- as.data.frame(lapply(clustering.set.in,as.numeric))
  clustering.set.in[is.na(clustering.set.in)] <- 0
  
  #dat <- clustering.set.reduced
  clustering.set.in$n_productSub <- NULL
  vid_score <- clustering.set.in$vid_score
  clustering.set.in$vid_score <- NULL
  
 # running PCA
  cluster.pca <- prcomp(clustering.set.in,
                   center = TRUE,
                   scale. = TRUE)

  pca.dat <- as.data.frame(cluster.pca$x[,1:10])
  #plot(cluster.pca)
  jpeg(file = paste("pca_comps_",account,".jpeg",sep=""))
  plot(cluster.pca, type='l')
  dev.off()
  #cluster.pca_summary <- summary(cluster.pca)[[1]]
  #write.table(cluster.pca_summary, file=paste("PCA_Summary_",account,".txt",sep=""), sep="\t")
  #plot3d(pca.dat$PC1, pca.dat$PC2, pca.dat$PC3)
    
  # searching optimal k  
  # using sillouett [Currently CRASHES!!!]
  # asw <- numeric(20)
  # for (k in 2:20)
  #   asw[[k]] <- pam(tst, k) $ silinfo $ avg.width
  # optimalK <- which.max(asw)
  # cat("silhouette-optimal number of clusters:", optimalK+1, "\n")
 
  # searching optimal k  
  # using elbow
  wss <- (nrow(clustering.set.in)-1)*sum(apply(clustering.set.in,2,var))
  for (i in 2:20) wss[i] <- sum(kmeans(clustering.set.in,
                                       centers=i)$withinss)
  jpeg(file = paste("clustering_elbow_plit_",account,".jpeg",sep=""))
  plot(1:20, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  dev.off()
  
  # get testing data
  #setwd(paste(getwd(),"/data/",account,sep=""))
  #testing.set <- read.table(file = paste(account,"_testing_set",sep=""),sep = ",", header = TRUE, fill = TRUE)
  # account
  # fit$size
  # fit$cluster
  
  #### RESEARCH THIS #######
  #*****************************************************************
  # Determine number of clusters
  #****************************************************************** 
  # correlation = cor(clustering.set.in, method = 'pearson')    
  # dissimilarity = 1 - (correlation)
  # distance = as.dist(dissimilarity)
  
  n = ncol(clustering.set.in)
  n1 = ceiling(n*2/3)
  
  # percentage of variance explained by clusters
  p.exp = rep(0,n1)
  
  # minimum correlation among all variables in each cluster  
  min.cor = matrix(1,n1,n1)  
  
  for (i in 2:n1) {
    fit = kmeans(clustering.set.in, centers=i, iter.max=50)
    p.exp[i] = 1- fit$tot.withinss / fit$totss
    
    # for (j in 1:i) {
    #   index = fit$cluster == j
    #   min.cor[i,j] = min(correlation[index,index])
    # }
  }
  
  # minimum number of clusters that explain at least 90% of variance
  optimalK <- min(which(p.exp > 0.75))
  
  # minimum number of clusters such that correlation among all components in each cluster is at least 40%
  # will not always work
  # min(which(apply(min.cor[-1,],1,min,na.rm=T) > 0.4)) + 1
  # 
  # # number of clusters based on elbow method
  # find.maximum.distance.point(p.exp[-1]) + 1
  
  # compute distance vector from new points to centers
  
  #choose optimal k where sos derivative is large for the first time
  # optimalK <- which.max(diff(wss)) + 1
  # optimalK <- which.min(wss)
  # run the kmeans with the optimal k
  fit <- kmeans(clustering.set.in, optimalK, iter.max = 50) 
  jpeg(file = paste("clustering_2PCA_plot_",account,".jpeg",sep=""))
  clusplot(clustering.set.in, fit$cluster)
  dev.off()
  # get cluster means 
  centroids <- as.data.frame(aggregate(clustering.set.in,by=list(fit$cluster),FUN=mean))
 
 return (centroids) 
  
}