library("bigrquery")
project <- "future-spot-826" # put your projectID here
researchProj <- "ecstatic-spirit-101413"
researchQuery <- "select * from [Nir."
query = "SELECT *, rand() as random "
FromPart = " FROM [px_data.domready_20160316] "
fromRange = "FROM TABLE_DATE_RANGE(px_data.domready_, TIMESTAMP('2016-03-07'), TIMESTAMP('2016-03-21'))"
GoodWherePart = " WHERE (vid_score is null OR vid_score < 60 ) and account_name in ('Wix','Airbnb','90min','SuperSonic') order by random limit 100000"
BadWherePart = " WHERE vid_score > 59 AND ip_classification = 'unknown' AND created_incident = true and account_name in ('Wix','Airbnb','90min','SuperSonic') order by random limit 100000"
NaturalWherePart = " WHERE account_name in ('Wix','Airbnb','90min','SuperSonic') order by random limit 100000"

sql.good = paste(query,fromRange,GoodWherePart, sep="") 
sql.bad = paste(query,fromRange,BadWherePart, sep="")
sql.natural = paste(query,fromRange,NaturalWherePart, sep="")
#sample.real <- readline(prompt="Enter an sample realization set size: ")
sample.real <- 1
for(i in 1:as.numeric(sample.real)){
  assign(paste('dom.ready.dat.good', i, sep=''), query_exec(sql.good, project ))
  #assign(paste('dom.ready.dat.good', i, sep=''), query_exec(sql.good, project ))
  #assign(paste('dom.ready.dat.bad', i, sep='' ), query_exec(sql.bad, project  ))
}
# bind all query results sets
dom.ready.dat <- do.call(rbind, mget(ls(pattern="^dom.ready.dat")))
dom.ready.dat_bkp <- dom.ready.dat
dom.ready.dat$random <- NULL
dom.ready.dat$timestamp <- NULL
# replace na with blanks
dom.ready.dat[is.na(dom.ready.dat)] <- ""
# vid score - repalce blanks with zeros
dom.ready.dat$vid_score[dom.ready.dat$vid_score==""] <- 0
dom.ready.dat$vid_score <- as.numeric(dom.ready.dat$vid_score)
# for POC purpase, filter in only supersonic
#dom.ready.dat <- subset(dom.ready.dat,account_name == "Wix")
#dom.ready.dat$`dom.ready.dat$vid` <- NULL
#another option to do it
#rbindlist(mget(ls(pattern="^dom.ready.dat")))
dom.ready.dat <- dom.ready.dat[c(48,1:47,49:92)]
# remove all filters (white/black lists)
lists <- c("whitelist","blacklist")
dom.ready.dat <- subset(dom.ready.dat,!(dom.ready.dat$filter_type %in% lists))
# remove duplicates
dom.ready.dat <- dom.ready.dat[!duplicated(dom.ready.dat), ]
# 0. prepare legit (score = 0)
legit.dom.ready.dat <- subset(dom.ready.dat,vid_score == 0)

attach(dom.ready.dat)
attach(legit.dom.ready.dat)
# clean environment
rm(list=setdiff(ls(), c("dom.ready.dat","legit.dom.ready.dat")))

save.image("~/Documents/r_projects/domready.RData")

# join domready and incidents outer join to domready
#sqljoin <- "SELECT * FROM [px_data.incidents_20160314] inc
#right outer join each [px_data.domready_20160314] domready
#on inc.vid = domready.vid
#LIMIT 10000"
#dom.incidents <- query_exec(sqljoin, project )

# visualizations
#visualization()

#proj.test.count._id.by.session <- aggregate(`_id` ~ session, data = proj.test, function(x) length(unique(x)))

#proj.test.count._id.by.vid <- aggregate(`_id` ~ vid, data = proj.test, function(x) length(unique(x)))

#proj.test.count.vid.by.session <- aggregate(vid ~ session, data = proj.test, function(x) length(unique(x)))
#proj.test.count.session.by.vid <- aggregate(session ~ vid, data = proj.test, function(x) length(unique(x)))

#setwd('~/Documents/r_projects/arff')

#write.arff(dom.ready.dat, "dom.ready.dat.arff", eol = "\n")

