prepare_newpoint_for_prediction <- function(testing.set){
  # NEED TO SYNC LEVELS OF FACTORS from training set onto testing set
  logical.variables <- names(subset_colclasses(testing.set, c("logical")))
  numeric.variables <- names(subset_colclasses(testing.set, c("integer","numeric")))
  factor.variables <- names(subset_colclasses(testing.set, c("factor")))
  char.variables <- names(subset_colclasses(testing.set, c("character")))
  
  # cast character to factor
  chars.vars.data <- testing.set[,char.variables]
  # what do to with them ?
  chars_to_factors.vars.data <- as.data.frame(lapply(chars.vars.data, as.factor))
  # nominal\factor variables
  factor.vars.data <- testing.set[,factor.variables]
  if (nrow(chars_to_factors.vars.data) >0 ) factor.vars.data <- cbind(factor.vars.data,chars_to_factors.vars.data)
  two.level.factors <-     factor.vars.data[sapply(FUN =function(x) nlevels(x) == 2,X=factor.vars.data)]
  non_two.level.factors <- factor.vars.data[sapply(FUN =function(x) nlevels(x) > 2,X=factor.vars.data)]
  
  # remove non-factors or factor with too many levels
  logic.ids.factors <- function(x) nlevels(x) < 50 
  non_two.level.factors.clean <- as.data.frame(non_two.level.factors[sapply(non_two.level.factors,logic.ids.factors)])
  dummyVars <- dummyVars(~., data = non_two.level.factors.clean)
  non_two.level.factors.clean.dummy <- as.data.frame(predict(dummyVars,non_two.level.factors.clean))
  
  logical.vars.data <- testing.set[,logical.variables]
  logical.vars.data <- cbind(logical.vars.data,two.level.factors)
  logical.vars.data[] <- lapply(logical.vars.data,as.integer)
  
  numeric.vars.data <- testing.set[,numeric.variables]
  
  clustering.test.set <- cbind(non_two.level.factors.clean.dummy,numeric.vars.data,logical.vars.data)
  clustering.test.set <- clustering.test.set[,c(scheme)]
}