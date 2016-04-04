library(fpc)
# eps is radius of neighborhood, MinPts is no of neighbors
# within eps
cluster <- dbscan(sampleiris[,-5], eps=0.6, MinPts=4)
plot(cluster, sampleiris)
plot(cluster, sampleiris[,c(1,4)])
# Notice points in cluster 0 are unassigned outliers
table(cluster$cluster, sampleiris$Species)