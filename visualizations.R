visualization <- function(){
  identifiers <- dom.ready.dat[,c("session","vid","_id","cookie"),]
  setwd('~/Documents/r_projects/visualizations')
  # dsitributions
  proj.test <- dom.ready.dat[,c("vid","vid_score","_id","session")]
  
  # count unique vid's by session
  proj.test.count.void.by.session <- aggregate(vid ~ session, data = proj.test, function(x) length(unique(x)))
  attach(proj.test.count.void.by.session)
  min <- summary(proj.test.count.void.by.session$vid)[[1]]
  first <- summary(proj.test.count.void.by.session$vid)[[2]]
  second <- summary(proj.test.count.void.by.session$vid)[[3]]
  third <- summary(proj.test.count.void.by.session$vid)[[4]]
  mean <- summary(proj.test.count.void.by.session$vid)[[5]]
  max <- summary(proj.test.count.void.by.session$vid)[[6]]
  proj.test.count.void.by.session.bp <- qplot(factor(0),vid,data=proj.test.count.void.by.session,geom='boxplot') +
    ggtitle(paste("Distribution vid by session: 1st - ",first,"| second: ", second, "| third: ", third, "| mean: ", mean, "| max", max,sep=""))
  ggsave(filename = paste("void.by.session.bp.jpg",sep=""))
  
  # count unique sessions by vid
  proj.test.count.session.by.vid <- aggregate(session ~ vid, data = proj.test, function(x) length(unique(x)))
  attach(proj.test.count.session.by.vid)
  min <- summary(proj.test.count.session.by.vid$session)[[1]]
  first <- summary(proj.test.count.session.by.vid$session)[[2]]
  second <- summary(proj.test.count.session.by.vid$session)[[3]]
  third <- summary(proj.test.count.session.by.vid$session)[[4]]
  mean <- summary(proj.test.count.session.by.vid$session)[[5]]
  max <- summary(proj.test.count.session.by.vid$session)[[6]]
  qplot(factor(0),session,data=proj.test.count.session.by.vid,geom='boxplot') +
    ggtitle(paste("Distribution session by vid: 1st - ",first,"| second: ", second, "| third: ", third, "| mean: ", mean, "| max", max,sep=""))
  ggsave(filename = paste("session_by_void.bp.jpg",sep=""))
  # count unique _ids (activities) by session
  proj.test.count._id.by.session <- aggregate(`_id` ~ session, data = proj.test, function(x) length(unique(x)))
  attach(proj.test.count._id.by.session)
  min <- summary(proj.test.count._id.by.session$`_id`)[[1]]
  first <- summary(proj.test.count._id.by.session$`_id`)[[2]]
  second <- summary(proj.test.count._id.by.session$`_id`)[[3]]
  third <- summary(proj.test.count._id.by.session$`_id`)[[4]]
  mean <- summary(proj.test.count._id.by.session$`_id`)[[5]]
  max <- summary(proj.test.count._id.by.session$`_id`)[[6]]
  qplot(factor(0),proj.test.count._id.by.session$`_id`,data=proj.test.count._id.by.session,geom='boxplot') +
    ggtitle(paste("Distribution unique `_id` (activities) by session: 1st - ",first,"| second: ", second, "| third: ", third, "| mean: ", mean, "| max", max,sep=""))
  ggsave(filename = paste("avtivities_by_session.bp.jpg",sep=""))
  # count unique _ids (activities) by vid's
  proj.test.count._id.by.vid <- aggregate(`_id` ~ vid, data = proj.test, function(x) length(unique(x)))
  attach(proj.test.count._id.by.vid)
  min <- summary(proj.test.count._id.by.vid$`_id`)[[1]]
  first <- summary(proj.test.count._id.by.vid$`_id`)[[2]]
  second <- summary(proj.test.count._id.by.vid$`_id`)[[3]]
  third <- summary(proj.test.count._id.by.vid$`_id`)[[4]]
  mean <- summary(proj.test.count._id.by.vid$`_id`)[[5]]
  max <- summary(proj.test.count._id.by.vid$`_id`)[[6]]
  qplot(factor(0),proj.test.count._id.by.vid$`_id`,data=proj.test.count._id.by.vid,geom='boxplot') +
    ggtitle(paste("Distribution `_id` (activities) by vid's: 1st - ",first,"| second: ", second, "| third: ", third, "| mean: ", mean, "| max", max,sep=""))
  ggsave(filename = paste("avtivities_by_vid.bp.jpg",sep=""))
  
  proj.test.count._id.by.session_not_unique <- aggregate(`_id` ~ session, data = proj.test, function(x) length(x))
  attach(proj.test.count._id.by.session_not_unique)
  min <- summary(proj.test.count._id.by.session_not_unique$`_id`)[[1]]
  first <- summary(proj.test.count._id.by.session_not_unique$`_id`)[[2]]
  second <- summary(proj.test.count._id.by.session_not_unique$`_id`)[[3]]
  third <- summary(proj.test.count._id.by.session_not_unique$`_id`)[[4]]
  mean <- summary(proj.test.count._id.by.session_not_unique$`_id`)[[5]]
  max <- summary(proj.test.count._id.by.session_not_unique$`_id`)[[6]]
  qplot(factor(0),proj.test.count._id.by.session_not_unique$`_id`,data=proj.test.count._id.by.session,geom='boxplot') +
    ggtitle(paste("Distribution `_id` (activities) by session: 1st - ",first,"| second: ", second, "| third: ", third, "| mean: ", mean, "| max", max,sep=""))
  ggsave(filename = paste("non_unique_avtivities_by_session.bp.jpg",sep=""))
  
  # score distribution
  m <- ggplot(dom.ready.dat, aes(x = vid_score))
  m + geom_density()
}




