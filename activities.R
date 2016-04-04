library("ggplot2")
setwd("~/Documents/r_projects")
dat <- read.table(file="test.csv",header=T,sep=",")
attach (dat)
summary(dat)
dat[is.na(dat$vid_score),c('vid_score')] <- 0
# visualizations univariate:
# boxplots
p <- ggplot(dat, aes(factor(is_os_family_windows), vid_score))
p + geom_boxplot()
