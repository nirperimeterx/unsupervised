This is a generic clustering (currently kmeans) system that does the following steps:
1. Pre-processing - read data from files (results of big queries on legit , low vid_score, dom ready data, on 2 weeks, for each account seperately)
2. Feature selection - using  univariate analysis (variance mostly) and bivariate analysis (correlation) and PCA to exclude low variance and co-linear variables, and reduce overall dimensions (pva)
3. search for an optimal k (with silhouette width) for clustering, performs the clustering
4. Prediction method - given that clustering is based on legit data only, the system measure the distance between new data points and clusters, and constructs a distance vector for each new data point. Then using statistical method (based on std) the system determines if the new data points resembles enough to one of the clusters or not, and by that establish how suspicous it is 
