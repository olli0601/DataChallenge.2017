setwd('H:/R/Data Science Challenge')
require(DataChallenge.2017)
library(e1071)
data(train)

outfile.base <- 'DataChallenge_LUIGI_sub9_'

#Load data
data(pov)
pov
data(pop13)
pop13
data(gdp)
gdp
data(ihr)
ihr
data(nut)
nut

#Data enrichment
d1 <- merge(train, pov[,c(1:2,21)], by=c('ISO','YEAR'), all.x=TRUE)
d2 <- merge(d1,gdp, by=c('ISO','YEAR'), all.x=TRUE)
d2$gdp.cap <- d2$GDP/d2$POV_POPULATION_TOTAL_SP_POP_TOTL_
d3 <- merge(d2,whoreg,by='ISO')
d4 <- merge(d3,ihr[,c(1,2,6,8)], by=c('ISO','YEAR'), all.x=TRUE)
d5 <- merge(d4,data.frame(nut[,c(1,2,139,140)],nut$NUT_NUMBER_OF_INFANT_DEATHS_SH_DTH_IMRT_,nut$NUT_MORTALITY_RATE_INFANT_PER_1_000_LIVE_BIRTHS_SP_DYN_IMRT_IN_),by=c('ISO','YEAR'), all.x=TRUE)

#Estimation
d6 <- d5[!is.na(d5$DISASTERS_N)]
reg1 <- lm(DISASTERS_N~gdp.cap+IHR_RISK_COMMUNICATION +IHR_RESPONSE +nut.NUT_NUMBER_OF_INFANT_DEATHS_SH_DTH_IMRT_+nut.NUT_MORTALITY_RATE_INFANT_PER_1_000_LIVE_BIRTHS_SP_DYN_IMRT_IN_ + nut.NUT_NUMBER_OF_INFANT_DEATHS_SH_DTH_IMRT_ + nut.NUT_MORTALITY_RATE_INFANT_PER_1_000_LIVE_BIRTHS_SP_DYN_IMRT_IN_, data = d6)
summary(reg1)

d5[YEAR == '2015',11] <- impute(d5[YEAR == '2015',11], what = c('mean'))
d5[YEAR == '2015',12] <- impute(d5[YEAR == '2015',12], what = c('mean'))
d5[YEAR == '2016',11] <- impute(d5[YEAR == '2016',11], what = c('mean'))
d5[YEAR == '2016',12] <- impute(d5[YEAR == '2016',12], what = c('mean'))

sub <- subset(d5, YEAR>=2015,select = c(ISO,YEAR,gdp.cap,IHR_RISK_COMMUNICATION,IHR_RESPONSE,nut.NUT_NUMBER_OF_INFANT_DEATHS_SH_DTH_IMRT_,nut.NUT_MORTALITY_RATE_INFANT_PER_1_000_LIVE_BIRTHS_SP_DYN_IMRT_IN_,nut.NUT_MORTALITY_RATE_INFANT_PER_1_000_LIVE_BIRTHS_SP_DYN_IMRT_IN_,nut.NUT_NUMBER_OF_INFANT_DEATHS_SH_DTH_IMRT_)) 
#new <- data.frame(gdp.cap = sub$gdp.cap, WHO_REGION = sub$WHO_REGION, IHR_RESPONSE = sub$IHR_RESPONSE, IHR_RISK_COMMUNICATION = sub$IHR_RISK_COMMUNICATION)
sub[, PREDICTION:= predict(reg1,newdata = sub)]

#Make zero of counrties that are missing or negative
sub$PREDICTION[is.na(sub$PREDICTION)] <- 0
sub$PREDICTION[sub$PREDICTION<0] <- 0

#Make zero if prediction lower than 0.3
#sub$PREDICTION[sub$PREDICTION<0.3] <- 0

sub <- unique(subset(sub, select=c(ISO,YEAR,PREDICTION)))
sub[, TEAM_ID:='LUIGI']
sub[, SUBMISSION_ID:='sub3']

dc.check.submission(sub)
write.csv(sub, row.names=FALSE, file=paste0(outfile.base,'predictions.csv'))

