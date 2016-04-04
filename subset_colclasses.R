subset_colclasses <- function(DF, colclasses) {
  DF[,sapply(DF, function(vec, test) class(vec) %in% test, test=colclasses)]
}