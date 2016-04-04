new_points_feature_selection <- function(training.set,testing.set, dat, factors.scheme,logical.scheme,numeric.scheme,dummy.scheme){
  #training.cols <- names(dat) 
  #testing.set <- testing.set[,training.cols]
  
  # TESTING SET - DEVIDE by variables types
  # sync testing set scheme against params
  testint.set <- testing.set[,c(logical.scheme,numeric.scheme)]
  testing.set.factos.data <- testing.set[,c(factors.scheme)]
  
  # Testing set DEVIDE by variables types
  testing.logical.variables <- testing.set[,c(logical.scheme)]
  testing.numeric.variables <- testing.set[,c(numeric.scheme)]

 ################ FACTORS SYNC######################################################### 
  #sync factor levels between training and testing sets
  # TRAINING SET - FACTORS
  training.set.factors.data <- dat[,c(factors.scheme)]
  
  # sync factor levels
  testing.set.factors.levels <- mapply(FUN = syncLevels,training.set.factors.data,testing.set.factos.data)
  levels(testing.set.factos.data) <- testing.set.factors.levels
  
  testing.set.factos.data <- data.frame(unclass(testing.set.factos.data))
  
  ######### TESTING FACTORS -> DUMMY ###########################################
  # first check that no 2 levels factors occur accidently
  testing.set.non_two.level.factors <- testing.set.factos.data[sapply(FUN =function(x) nlevels(x) > 2,X=testing.set.factos.data)]
  testing.dummy.vars <- dummyVars(~., data = testing.set.non_two.level.factors)
  testing.set.factors.dummy.clean <- as.data.frame(predict(testing.dummy.vars,testing.set.non_two.level.factors))
  
  testing.set.bind <- cbind(testing.logical.variables,testing.numeric.variables,testing.set.factors.dummy.clean)
  exclude.list <- unlist(setdiff(names(training.set),names(testing.set.bind)))
  proj.list <- names(training.set)
  proj.list <- proj.list[!proj.list %in% exclude.list]
  # sync dummies schemas
  tesing.set.bind.synched <- testing.set.bind[,proj.list]
  # add empty column according to exclude.list scheme to sync number of columns
  empt.mat <- matrix(NA, ncol = length(exclude.list), nrow = nrow(tesing.set.bind.synched))
  empt.df <- data.frame(empt.mat)
  names(empt.df) <- exclude.list
  tesing.set.bind.synched.supplemented <- cbind(tesing.set.bind.synched,empt.df)
  final.testing.set <- tesing.set.bind.synched.supplemented %>% select(noquote(order(colnames(tesing.set.bind.synched.supplemented))))
  final.training.set <- training.set %>% select(noquote(order(colnames(training.set))))
  return(list(final.training.set,final.testing.set))
}


