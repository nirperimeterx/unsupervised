account = "90min"
# load image from file
load(paste("~/Documents/r_projects/data/",account,".RData", sep=""))
dat <- dom.data.90min
rm(dom.data.90min)
setwd('~/Documents/r_projects/')
library("bigrquery")
library("data.table")
library("ggplot2")
library("foreign")
library("caret")
library("corrplot")
library("plyr")
library("sjPlot")
library("caret")
library("arules")
library("arulesViz")
library("gplots")
library("cluster")
library("rgl")
library("dplyr")
library("lattice")
library("fpc")
library("cluster")
source("subset_colclasses.R")
source("visualizations.R")
source("chi_square_test.R")
source("run_correlation_feature_selection.R")
source("get_dat_from_local_files.R")
source("new_points_feature_selection.R")
source("syncLevels.R")
source("kmeans_search.R")
#source("association_rules_preprocessing.R")
#source("query.R")

# 1. get data either from query or from local files
#dom.data.90min <- get_data_from_local_files("wix") 
#get testing set from local file
#testing.set <- get_test_data_from_local_files(account)
#setwd('~/Documents/r_projects/data')
#save.image(file = "wix.RData")
# 2. add new features

# 1. ip <> socket ip
# 2. dirty sessions (have more than one vid) - already in is_proxy
#dom.ready.dat$dirty_session <- ave(dom.ready.dat[, c("vid")], dom.ready.dat[, c("session")], FUN = function(x) length(unique(x))) > 1
#dom.ready.dat$ip_socket_compat <- dom.ready.dat$ip == dom.ready.dat$socket_ip

# 3. correlations and consequently features selection
# 3.1 - run correlation for legit data only
result <- run_correlation_feature_selection(dat,account)
training.set <- result[[1]]
scheme <- result[[2]]
factors.scheme <- result[[3]]
logical.scheme <- result[[4]]
numeric.scheme <- result[[5]]
dummy.scheme <- result[[6]]
sets <- new_points_feature_selection(training.set,testing.set,dat,factors.scheme,logical.scheme,numeric.scheme,dummy.scheme)
training.set <- sets[[1]]
testing.set <- sets[[2]]
 # 4. modelling

# 4.1 run kmeans with searching and visualize the optimal k
# 4.1.1 run kmeans on negative (kosher, normal) data only - according to this approach, we only cluster legitimate traffic, build centroids for each cluster
# and when a new impression arrives, its feature vector is built and a distance logic is invoked:
# according to this distance logic, if the new impression is at AN EQUAL distance from all legitimate centroids (think of a point within a circle, where all the legit centroids
# are on the perimeter :-) of the circle and the new impression vector is a point exactly (+- confidence interval) inside the circle at a radius distance from the perimeter =>
# then we can conclude this impression DOES NOT belong to any of the legitimate cluster - THUS it is abnormal
clust.centroids <- kmeans_search(training.set,account)
pred.result <- clustering_prediction(clust.centroids, testing.set)



