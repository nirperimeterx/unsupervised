clustering_prediction <- function(new.data, kcentroids){
    # Need to fix the following row: need to get as parameters the exlude.list (new_points_feature_selection.R)
    # medians and exchane the NA with these median
    new.data[is.na(new.data)] <- 1
    #new.data <- new.data[1:3,]
    # check if factors exists and change (remove after new_points_features_selection is fixed)
    indx <- sapply(new.data, is.factor)
    new.data[indx] <- lapply(new.data[indx], function(x) as.numeric(as.character(x)))
    distanceMatrix <- matrix(NA, nrow=dim(new.data)[1], ncol=dim(kcentroids)[1])
    for(i in 1:nrow(kcentroids)) {
      #distanceMatrix[,i] <- sqrt(sum((kcentroids[i,]-new.data)^2))
      cent <- t(kcentroids[i,])
      subtract <- sweep(new.data, 1, cent,"-")
      power <- apply(subtract, c(1,2), function(x) x^2)
      sumation <- apply(power,1,sum)
      squareroot <- lapply(sumation,sqrt)
      distanceMatrix[,i] <- unlist(squareroot)
    }
    sd.coeff <- apply(distanceMatrix, 1, sd) / apply(distanceMatrix, 1, mean) 
    # prediction
    # if new point distance distribution (sd.coeff) is larger than .75 precentile, means that 
    # distance matrix variance is large enough to establish that the new point is close enough to one of hte centroids
    percentile.75 <- summary(sd.coeff)[[5]]
    predictions <- !(sd.coeff > percentile.75) # TRUE means non-human 
    return (predictions)
  
}