syncLevels <- function (training.factor, testing.factor) {
  
  sync.levels <- union ( levels(testing.factor) , levels(training.factor) )  
  
  return (factor(testing.factor, levels = sync.levels))
  
}