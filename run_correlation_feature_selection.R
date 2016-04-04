run_correlation_feature_selection <- function(dat,account){
  
  # data cleansing
  dat[,-1] <- gsub("[", "", as.matrix(dat[,-1]),fixed = T)
  dat[,-1] <- gsub("]", "", as.matrix(dat[,-1]),fixed = T)
  setwd("~/Documents/r_projects/visualizations") 
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  # devide to three groups of variables
  
  # incase account name in the data (should not be)
  dat$account_name = NULL 
  dat$site_history_length <- as.numeric(dat$site_history_length)
  dat$window_history <- as.numeric(dat$window_history)
  dat$f_pluginsLength <- as.numeric(dat$f_pluginsLength)
  # delete customer params fields
  #customer.param.fields.ids <- grep("custom",names(dat))
  #dat <- dat[,-c(customer.param.fields.ids)]
  
  # remove na
  # dat[is.na(dat)] <- ""
  # dat$vid_score[dat$vid_score == ""] <- NA
  # dat$vid_score <- as.numeric(dat$vid_score)
  # grep ("history", names(dat))
  
  # DEVIDE by variables types
  logical.variables <- names(subset_colclasses(dat, c("logical")))
  numeric.variables <- names(subset_colclasses(dat, c("integer","numeric")))
  factor.variables <- names(subset_colclasses(dat, c("factor")))
  char.variables <- names(subset_colclasses(dat, c("character")))
  
 ################ FACTORS ######################################################### 
  
  # cast character to factor
  chars.vars.data <- dat[,char.variables]
  # what do to with them ?
  chars_to_factors.vars.data <- as.data.frame(lapply(chars.vars.data, as.factor))
  
  # nominal\factor variables
  factor.vars.data <- dat[,factor.variables]
  if (nrow(chars_to_factors.vars.data) >0 ) factor.vars.data <- cbind(factor.vars.data,chars_to_factors.vars.data)
  two.level.factors <-     factor.vars.data[sapply(FUN =function(x) nlevels(x) == 2,X=factor.vars.data)]
  non_two.level.factors <- factor.vars.data[sapply(FUN =function(x) nlevels(x) > 2,X=factor.vars.data)]
  hidden.logical <- non_two.level.factors[sapply(FUN=function(x) length(intersect(c("true","false"),levels(x))) == 2,
                               X=non_two.level.factors)]
  hidden.logical[] <- lapply(hidden.logical, as.character)
  hidden.logical[hidden.logical==""] <- "false"
  hidden.logical[] <- lapply(hidden.logical, as.factor)
  hidden.logical[] <- lapply(hidden.logical,as.integer)
  # remove hidden logicals
  cond <- ! names(non_two.level.factors) %in% names(hidden.logical)
  non_two.level.factors <- subset( non_two.level.factors, select =  cond)
  # remove non-factors or factor with too many levels
  ids.factors <- function(x,y) nlevels(x) < y 
  non_two.level.factors.clean <- as.data.frame(non_two.level.factors[sapply(non_two.level.factors,ids.factors,100)])
  # preprocess data for chi square test filter
  chi.square.data <- non_two.level.factors.clean
  chi.square.data[chi.square.data=="" | chi.square.data=="missing"] <- NA
  chi.square.data <- data.frame(lapply(chi.square.data, as.character), stringsAsFactors=FALSE)
  chi.square.data <- data.frame(lapply(chi.square.data, as.factor))
  ids.factors <- function(x,y) nlevels(x) > y
  chi.square.data <- as.data.frame(chi.square.data[sapply(chi.square.data,ids.factors,2)])
  combos <- combn(ncol(chi.square.data),2)
  chi.sq.results <- as.data.frame(adply(combos, 2,chi_square_test,chi.square.data))
  #plot(chi.sq.results)
  #write.table(chi.sq.results,paste("chi_sq_result_",account,".csv",sep=","))
  # exclude ccorrelated variables
  exclude.list <- unique(subset(chi.sq.results,p.value < 0.01)$Row)
  non_two.level.factors.clean <- as.data.frame(non_two.level.factors.clean[,-c(exclude.list)])
  
  # dummy variables
  dummyVars <- dummyVars(~., data = non_two.level.factors.clean)
  non_two.level.factors.clean.dummy <- as.data.frame(predict(dummyVars,non_two.level.factors.clean))
  # check which dummy has near zero variance
  nzv_cols_dummy <- nearZeroVar(non_two.level.factors.clean.dummy)
  if(length(nzv_cols_dummy) > 0) non_two.level.factors.clean.dummy <- non_two.level.factors.clean.dummy[, -nzv_cols_dummy]
  non_two.level.factors.clean.dummy <- as.data.frame(non_two.level.factors.clean.dummy)
  # run binary correlation analysis to exclude correlated variables
  dummy_corr <- cor(non_two.level.factors.clean.dummy, use = 'pairwise')
  cor_cutoff <- findCorrelation(dummy_corr, cutoff=0.7) # putt any value as a "cutoff"
  cor_cutoff <- sort(cor_cutoff)
  #jpeg(file = paste("dummy_factors_heatmap2_",account,".jpeg",sep=""))
  #heatmap.2(dummy_corr, Rowv=FALSE, Colv=FALSE, dendrogram="none", 
            #key=TRUE, density.info="none", trace="none", 
           # col=colorpanel(100, "gold", "cornflowerblue"), scale="row",cexRow=0.6, cexCol=0.6)
  #dev.off()
  non_two.level.factors.clean.dummy.reduced <- non_two.level.factors.clean.dummy[,-c(cor_cutoff)]
 
  # remove near zero variance variables
  nzv_cols <- nearZeroVar(non_two.level.factors.clean.dummy.reduced)
  if(length(nzv_cols) > 0) non_two.level.factors.clean.dummy.reduced <- non_two.level.factors.clean.dummy.reduced[, -nzv_cols]
  
############### BINARY VARIABLES ########################################################################## 
  
  # binary variables processing
  logical.vars.data <- dat[,logical.variables]
  logical.vars.data <- cbind(logical.vars.data,two.level.factors)
  logical.vars.data <- cbind(logical.vars.data,hidden.logical)
  logical.vars.data[] <- lapply(logical.vars.data,as.integer)
  #sjp.corr(logical.vars.data)
  #sjt.corr(logical.vars.data)
  binary_corr <- cor(logical.vars.data,use='pairwise')
  #levelplot(binary_corr)
  jpeg(file = paste("binary_heatmap2_",account,".jpeg",sep=""))
  heatmap.2(binary_corr, Rowv=FALSE, Colv=FALSE, dendrogram="none", 
            key=TRUE, density.info="none", trace="none", 
            col=colorpanel(100, "coral", "cornflowerblue"), scale="none",cexRow=0.8, cexCol=0.8 )
  dev.off()
  hc <- findCorrelation(binary_corr, cutoff=0.7) # putt any value as a "cutoff" 
  hc <- sort(hc)
  logical.vars.data.reduced <- logical.vars.data[,-c(hc)]
  logical.scheme <- names(logical.vars.data.reduced)
############### NUMERIC VARIABLES ########################################################################## 
  
  # numeric correlations
  numeric.vars.data <- dat[,numeric.variables]
  numeric.corr <- cor(numeric.vars.data,use='pairwise')
  numeric.corr <- findCorrelation(numeric.corr,cutoff=0.7)#use='pairwise')
  numeric.corr <- sort(numeric.corr)
  numeric.vars.data.reduced <- numeric.vars.data[,-c(numeric.corr)]
  #jpeg(file = paste("numeric_heatmap2_",account,".jpeg",sep=""))
  #heatmap.2(numeric.corr, Rowv=FALSE, Colv=FALSE, dendrogram="none", 
           # key=TRUE, density.info="none", trace="none", 
           # col=colorpanel(100, "cornflowerblue", "chocolate1"), scale="none",cexRow=0.8, cexCol=0.8 )
  #dev.off()
  numeric.scheme <- names(numeric.vars.data.reduced)
################# CBIND Everything ####################################################  
  
  # glue everything together
  clustering.set <- cbind(non_two.level.factors.clean.dummy.reduced,numeric.vars.data.reduced,logical.vars.data.reduced)
  # run correlation co-linearity analysis on the entire set to reduce more columns of different origin type
  # run binary correlation analysis to exclude correlated variables
  entire_corr <- cor(clustering.set)
  entire_corr[is.na(entire_corr)] <- 0
  gen_cor_cutoff <- findCorrelation(entire_corr, cutoff=0.7) # putt any value as a "cutoff" 
  gen_cor_cutoff <- sort(gen_cor_cutoff)
  if (length(gen_cor_cutoff) > 0) clustering.set.reduced <- clustering.set[,-c(gen_cor_cutoff)]
  else clustering.set.reduced <- clustering.set
  
  # delete created_incident (won't be available during prediction)
  clustering.set$created_incident <- NULL
  scheme <- names(clustering.set) # dummied
  non_two.level.factors.clean.scheme <- names(non_two.level.factors.clean)
  dummy.scheme <- names(non_two.level.factors.clean.dummy)
  return(list(clustering.set,scheme,non_two.level.factors.clean.scheme,logical.scheme,numeric.scheme,dummy.scheme))
}


