## Matthew Coates
## Rerun meta-analysis on pharyngitis excluding clinic-based studies

rm(list=ls())
library(data.table)
library(metafor)


## set directories
if (Sys.info()[1] == 'Windows') {
  codedir <- paste0("[Insert own directory]/rhd_invest_priv/")
  outdir <- paste0("[Insert own directory]/rhd_investment_case/")
} else {
  codedir <- paste0("[Insert own directory]/rhd_invest_priv/")
  outdir <- paste0("[Insert own directory]/rhd_investment_case/")
}


d <- openxlsx::read.xlsx(paste0(codedir,"/data/other_inputs/pharyngitis_data.xlsx"),sheet=1)
d <- data.table(d)
setnames(d,c("study","mean","lower","upper"))
# d[,mean:=mean/100]
# d[,upper:=upper/100]
# d[,lower:=lower/100]
d[,log_mean:=log(mean)]
d[,log_lower:=log(lower)]
d[,log_upper:=log(upper)]
d[,log_se:=(log_upper-log_lower)/(2*1.96)]

d1 <- copy(d[study!="Jose 2018"])

pearce <- rma(yi=d1$log_mean,sei=d1$log_se,weights=c(1,1,1,1,1))

## more or less reproduce Pearce analysis
est <- exp(pearce$beta[1])
lower <- exp(pearce$ci.lb)
upper <- exp(pearce$ci.ub)
paste0(round(est,1)," (",round(lower,1)," - ",round(upper,1),")")

## now conduct without Engel included
d2 <- copy(d1[study!="Engel 2012"])
test <- rma(yi=d2$log_mean,sei=d2$log_se,weights=c(1,1,1,1))

est <- exp(test$beta[1])
lower <- exp(test$ci.lb)
upper <- exp(test$ci.ub)
paste0(round(est,1)," (",round(lower,1)," - ",round(upper,1),")")


## now consider adding Jose et al. (2018) per reviewer comment
## in table 3 of Jose et al., we obtain 394 + 103 + 29 + 107 + 1 + 6 = 640 pharyngitis visits
## Methods say roster of approximately 225-250 children was maintained by  periodically recruiting additional volunteers (307 children total, but not for whole time period)
## So, assuming average of 237.5 students over 2 years, the denominator is 475 person-years
## 640/475 = 1.35
d3 <- copy(d[study!="Engel 2012"])
d3[is.na(log_se),log_se:=.05] ## not really using uncertainty from the meta-analysis since doing sensitivity analysis on parameter, so inserted value here based on the others
test2 <- rma(yi=d3$log_mean,sei=d3$log_se,weights=c(1,1,1,1,1))

est <- exp(test2$beta[1])
lower <- exp(test2$ci.lb)
upper <- exp(test2$ci.ub)
paste0(round(est,1)," (",round(lower,1)," - ",round(upper,1),")")










## looking at just GAS pharyngitis

d <- openxlsx::read.xlsx(paste0(codedir,"/data/other_inputs/pharyngitis_data.xlsx"),sheet=2)
d <- data.table(d)
setnames(d,c("study","mean","lower","upper"))
# d[,mean:=mean/100]
# d[,upper:=upper/100]
# d[,lower:=lower/100]
d[,log_mean:=log(mean)]
d[,log_lower:=log(lower)]
d[,log_upper:=log(upper)]
d[,log_se:=(log_upper-log_lower)/(2*1.96)]

d1 <- copy(d[study!="Jose 2018"])

pearce <- rma(yi=d1$log_mean,sei=d1$log_se,weights=c(.1434,.1418,.1428,.1436,.1435,.1428,.1421))

## more or less reproduce Pearce analysis
est <- exp(pearce$beta[1])
lower <- exp(pearce$ci.lb)
upper <- exp(pearce$ci.ub)
paste0(round(est,2)," (",round(lower,2)," - ",round(upper,2),")")



## now conduct without Engel included
d2 <- copy(d1[study!="Engel 2012"])
test <- rma(yi=d2$log_mean,sei=d2$log_se,weights=c(.1418,.1428,.1436,.1435,.1428,.1421))

est <- exp(test$beta[1])
lower <- exp(test$ci.lb)
upper <- exp(test$ci.ub)
paste0(round(est,1)," (",round(lower,1)," - ",round(upper,1),")")


## now conduct without Engel included and with Jose
d3 <- copy(d[study!="Engel 2012"])
d3[is.na(log_se),log_se:=.05] ## not really using uncertainty from the meta-analysis since doing sensitivity analysis on parameter, so inserted value here based on the others
test2 <- rma(yi=d3$log_mean,sei=d3$log_se,weights=c(1,1,1,1,1,1,1))

est <- exp(test2$beta[1])
lower <- exp(test2$ci.lb)
upper <- exp(test2$ci.ub)
paste0(round(est,1)," (",round(lower,1)," - ",round(upper,1),")")

