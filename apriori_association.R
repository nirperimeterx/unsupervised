apriori_association <- function (dat){
  patterns = random.patterns(nItems = 1000);
  data("AdultUCI")
  t <- as.data.frame(AdultUCI)
  str(t)
  t$income <- as.factor(t$income)
  t$education <- as.factor(t$income)
  numeric.variables <- names(subset_colclasses(t,c("integer")))
  t <- t[ , -which(names(t) %in% numeric.variables)] 
  t.trans = as(t, "transactions");
  rules = apriori(t.trans, parameter=list(support=0.01, confidence=0.5));
  inspect(head(sort(rules, by="lift"),3));
  plot(rules);
  head(quality(rules));
  plot(rules, measure=c("support","lift"), shading="confidence");
  plot(rules, shading="order", control=list(main ="Two-key plot"));
  subrules2 = head(sort(rules, by="lift"), 10);
  plot(subrules2, method="graph");
  plot(subrules2, method="graph", control=list(type="items"));
  plot(subrules2, method="paracoord");
  plot(subrules2, method="matrix3D");
  plot(subrules2, method="paracoord", control=list(reorder=TRUE));
  plot(subrules2, method="grouped")#, interactive=TRUE)
  oneRule = sample(rules, 1);
  inspect(oneRule);
  itemFrequencyPlot(t.trans, support = 0.1, cex.names=0.8);
  fsets = eclat(t.trans, parameter = list(support = 0.05), control = list(verbose=FALSE));
  singleItems = fsets[size(items(fsets)) == 1];
  singleSupport = quality(singleItems)$support;
  names(singleSupport) = unlist(LIST(items(singleItems), decode = FALSE));
  head(singleSupport, n = 5);
  itemsetList = LIST(items(fsets), decode = FALSE);
  allConfidence = quality(fsets)$support / sapply(itemsetList, function(x)
  max(singleSupport[as.character(x)]));
  quality(fsets) = cbind(quality(fsets), allConfidence);
  summary(fsets);
}