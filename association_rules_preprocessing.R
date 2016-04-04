association_rules_preprocessing <- function(dat){
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  # devide to three groups of variables
  
  # incase account name in the data (should not be)
  dat$account_name = NULL
  # delete customer params fields
  customer.param.fields.ids <- grep("custom",names(dat))
  dat <- dat[,-c(customer.param.fields.ids)]
  
  # remove na
  dat[is.na(dat)] <- ""
  
  # DEVIDE by variables types
  logical.variables <- names(subset_colclasses(dat, c("logical")))
  numeric.variables <- names(subset_colclasses(dat, c("integer","numeric")))
  factor.variables <- names(subset_colclasses(dat, c("factor")))
  char.variables <- names(subset_colclasses(dat, c("character")))
  
  # cast character to factor
  chars.vars.data <- dat[,char.variables]
  # what do to with them ?
  chars_to_factors.vars.data <- as.data.frame(lapply(chars.vars.data, as.factor))
  
  # nominal\factor variables
  factor.vars.data <- dat[,factor.variables]
  two.level.factors <-     factor.vars.data[sapply(FUN =function(x) nlevels(x) == 2,X=factor.vars.data)]
  non_two.level.factors <- factor.vars.data[sapply(FUN =function(x) nlevels(x) > 2,X=factor.vars.data)]
  
  # binary variables processing
  logical.vars.data <- dat[,logical.variables]
  logical.vars.data <- cbind(logical.vars.data,two.level.factors)
  logical.vars.data[] <- lapply(logical.vars.data,as.integer)
  #sjp.corr(logical.vars.data)
  #sjt.corr(logical.vars.data)
  binary_corr <- cor(logical.vars.data)
  binary_corr[is.na(binary_corr)] <- 0
  hc = findCorrelation(binary_corr, cutoff=0.7) # putt any value as a "cutoff" 
  hc = sort(hc)
  logical.vars.data.reduced = logical.vars.data[,-c(hc)]
  
  # numeric correlations
  numeric.vars.data <- dat[,numeric.variables]
  levelplot(cor(numeric.vars.data))
  
  # nominal\factor variables
  # remove near zero variance variables
  #nzv_cols <- nearZeroVar(non_two.level.factors)
  #if(length(nzv_cols) > 0) non_two.level.factors <- non_two.level.factors[, -nzv_cols]
  
  # remove non-factors or factor with too many levels
  logic.ids.factors <- function(x) nlevels(x) < 50 
  non_two.level.factors.clean <- as.data.frame(non_two.level.factors[sapply(non_two.level.factors,logic.ids.factors)])
  combos <- combn(ncol(non_two.level.factors.clean),2)
  chi.sq.results <- as.data.frame(adply(combos, 2,chi_square_test))
  write.table(chi.sq.results,"chi_sq_result.csv",sep=",")
  # exclude ccorrelated variables
  #exclude.list <- unique(subset(chi.sq.results,p.value < 0.01)$Row)
  #character.vars.data.clean <- character.vars.data.clean[,-c(exclude.list)]
  
  # dummy variables
  dummyVars <- dummyVars(~., data = non_two.level.factors.clean)
  non_two.level.factors.clean.clean.dummy <- predict(dummyVars,non_two.level.factors.clean)
  # check which dummy has near zero variance
  #nzv_cols_dummy <- nearZeroVar(character.vars.data.clean.dummy)
  #if(length(nzv_cols_dummy) > 0) character.vars.data.clean.dummy.clean <- character.vars.data.clean.dummy[, -nzv_cols_dummy]
  #character.vars.data.clean.dummy.clean <- as.data.frame(character.vars.data.clean.dummy.clean)
  # run binary correlation analysis to exclude correlated variables
  dummy_corr <- cor(non_two.level.factors.clean.clean.dummy)
  binary_corr[is.na(dummy_corr)] <- 0
  cor_cutoff <- findCorrelation(dummy_corr, cutoff=0.7) # putt any value as a "cutoff" 
  cor_cutoff <- sort(cor_cutoff)
  non_two.level.factors.clean.clean.dummy.reduced <- non_two.level.factors.clean.clean.dummy[,-c(cor_cutoff)]
  
  # glue everything together
  clustering.set <- cbind(non_two.level.factors.clean.clean.dummy.reduced,numeric.vars.data,logical.vars.data)
  # run correlation co-linearity analysis on the entire set to reduce more columns of different origin type
  # run binary correlation analysis to exclude correlated variables
  entire_corr <- cor(clustering.set)
  entire_corr[is.na(entire_corr)] <- 0
  gen_cor_cutoff <- findCorrelation(entire_corr, cutoff=0.7) # putt any value as a "cutoff" 
  gen_cor_cutoff <- sort(gen_cor_cutoff)
  clustering.set.reduced <- clustering.set[,-c(gen_cor_cutoff)]
  
  # delete created_incident (won't be available during prediction)
  clustering.set$created_incident <- NULL
  
  return(clustering.set)
}


