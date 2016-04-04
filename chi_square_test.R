chi_square_test <- function(x,data.set)  
  {
    print (paste(x[1],x[2], sep="|")) 
    proj.set <- data.set[,c(x[1],x[2])]
    #proj.set[proj.set=="" | proj.set=="missing"] <- NA
    #proj.set.clean <- proj.set[complete.cases(proj.set),]
    #proj.set.clean <- data.frame(lapply(proj.set.clean, as.character), stringsAsFactors=FALSE)
    #proj.set.clean <- data.frame(lapply(proj.set.clean, as.factor))
    if (min(unlist(lapply(proj.set,FUN = function(x) length(levels(x))))) < 2) return (NULL)
    else {
      test <- chisq.test(proj.set[, 1], proj.set[, 2])
      
      out <- data.frame("Row" = colnames(data.set)[x[1]]
                        , "Column" = colnames(data.set[x[2]])
                        , "Chi.Square" = round(test$statistic,3)
                        ,  "df"= test$parameter
                        ,  "p.value" = round(test$p.value, 3)
      )
      return(out)
    }
  
  
}  