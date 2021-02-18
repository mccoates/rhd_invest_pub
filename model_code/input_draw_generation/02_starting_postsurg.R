## Matthew Coates
## post-surg starting pop

rm(list=ls())

library(data.table)
library(ggplot2)
library(EnvStats)
library(stringr)
library(gtools)
################################################################
## set directories, load functions, load metadata
################################################################
if (Sys.info()[1] == 'Windows') {
  codedir <- paste0("[INSERT OWN DIRECTORY HERE]/rhd_invest_pub/")
  outdir <- paste0("[INSERT OWN DIRECTORY HERE]/rhd_investment_case/")
} else {
  codedir <- paste0("[INSERT OWN DIRECTORY HERE]/rhd_invest_pub/")
  outdir <- paste0("[INSERT OWN DIRECTORY HERE]/rhd_investment_case/")
}
indir <- paste0(outdir,"/data/")

## load functions
source(paste0(codedir,"/functions/swap_location_names.R"))
source(paste0(codedir,"/functions/map_ids_names.R"))

drawnum <- 4000

## read in AU locations (https://au.int/en/member_states/countryprofiles2)
locs <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/AU_member_states.xlsx")))
locs <- locs[!location_name %in% c("Sahrawi Republic")] ## we don't have burden estimates for Sahrawi Republic, so we will drop

## convert to GBD country names
locs <- swap_locnames(data=locs,out="GBD",version=2017)


## using survival range of 85%-97% loosely from Rusingiza and Zuhlke (REMEDY)
## using roughly 3000 to 5000 surgeries from Zilla (based on rates of cardiac surgery and % RHD)
surv <- .97
surgs <- 4000

peryr <- matrix(rep(surgs*surv,20*20),nrow=20)
for (i in c(2:20)) {
  peryr[,i] <- c(0,(peryr[,(i-1)]*surv)[1:19])
}

sum(peryr[20,])



## using a % of RHD with HF receiving surgery--range 
d <- readRDS(paste0(codedir,"/data/other_inputs/full_inc_cases_long_calcs.RDS"))
d <- d[age <= 45]
d <- d[,list(cases=sum(cases)),by=c("location_name","year")]
d[,surg:=cases*surgs]


surv <- .85
surgs <- .02

d[,surg:=cases*surgs]

peryr <- matrix(rep(d$surg*surv,18),nrow=18)
for (i in c(2:18)) {
  peryr[,i] <- c(0,(peryr[,(i-1)]*surv)[1:17])
}

sum(peryr[18,])



## mini simulation to obtain an age distribution for people with RHD post-surgery at starting point
## the total numbers are simulated separately
gbd <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_inc_prev_death_inputs.RDS"))
gbd <- gbd[!age_group_id %in% c(2,3,4,22,27)]
gbd <- map_to_names(d=gbd,codedir=codedir,keep_ids=T,gbd=2017)
gbd <- gbd[location_name %in% c(locs$location_name,"African Union")]
gbd[,c("age","loc_level"):=NULL]

## extend rates to single-year age groups
gbd <- gbd[!age_group_id %in% c(2,3,4,22,27)]
gbd[age_group_id == 28,age_group_name:="0"]
gbd[age_group_id==28,age_group_id:=1]
gbd <- rbind(gbd,gbd,gbd,gbd,gbd)
gbd <- gbd[order(location_name,year,sex,cause_name,measure_name,age_group_id)]
gbd[, id := seq_len(.N), by = c("location_name","year","sex","cause_name","measure_name","age_group_id")]
gbd <- gbd[!(age_group_name=="0" & id > 1) & !(age_group_name=="1 to 4" & id > 4)]
gbd[,age:=id-1+as.numeric(substr(age_group_name,1,2))]
gbd <- gbd[!age %in% c(96:99)]

gbd <- dcast.data.table(gbd,year+sex+age+cause_name+location_name~measure_name,value.var="val")
gbd <- dcast.data.table(gbd,year+sex+age+location_name~cause_name,value.var=c("Deaths","Prevalence","Incidence"))

pop <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_pop.RDS"))
gbd <- merge(gbd,pop,by=c("location_name","age","year","sex"),all.x=T)

params <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/intervention_params_uncert.xlsx")))
params <- params[,c(1:9),with=F]
setnames(params,c("intervention","intervention_id","effect_type","effect","lower","upper","uncert_dist","coverage_start","coverage_start_sd"))

cov <- readRDS(paste0(outdir,"/data/starting_coverage.RDS"))
cov <- cov[intervention_id == 5]
cov <- cov[,list(starting_coverage=mean(starting_coverage)),by=c("location_name")]

gbd <- merge(gbd,cov,by=c("location_name"),all.x=T)


## to get rough age distribution
## run from 2000 to 2017, assuming those getting surgery are 5% of RHD incidence (this is just for age distribution, and country distribution,
## so only the relative proportions matter, not
## the overall numbers), and then subject to mortality risks
tmp <- list()
for (i in c(2000:2017)) {
  tmp[[paste0(i)]] <- copy(gbd[year==i])
  tmp[[paste0(i)]][,newsurg:=starting_coverage*`Incidence_Rheumatic heart disease`*pop]
  tmp[[paste0(i)]][age > 45 | age < 10,newsurg:=0]
  if (i == 2000) tmp[[paste0(i)]][,cohort:=0]
  if (i > 2000) {
    toage <- copy(tmp[[paste(i-1)]])
    toage[age<95,age:=age+1]
    toage[,cohort:=cohort*.95]
    toage <- toage[,list(cohort=sum(cohort)),by=c("location_name","sex","age")]
    tmp[[paste0(i)]] <- merge(tmp[[paste0(i)]],toage,by=c("location_name","sex","age"),all.x=T)
    tmp[[paste0(i)]][age==0,cohort:=0]
    tmp[[paste0(i)]][,cohort:=cohort+newsurg]
  }
}

out <- copy(tmp[["2017"]])
out[,pct_agesex:=cohort/sum(cohort),by=c("location_name")] ## get age/sex percents within location
out[,tot_cohort:=sum(cohort),by=c("location_name")]
out[location_name!="African Union",pct_geog:=tot_cohort/sum(cohort)] ## get location percents

sum(out[age==5 & location_name!="African Union"]$pct_geog)
out[location_name=="African Union",pct_geog:=1]

gg <- ggplot(out,aes(x=age,y=pct_agesex)) + geom_bar(stat="identity") 
print(gg)


set.seed(362380)
surgs <- exp(rnorm(n=drawnum,mean=log((7000)),sd=(log(20000)-log(2000))/(2*1.96)))

surgs <- data.table(data.frame(counts=rep(surgs,96),draw=rep(c(1:drawnum),96),age=rep(c(0:95),each=drawnum)))
check <- copy(surgs[draw==1])
surgs <- merge(surgs,out[,c("location_name","age","sex","pct_geog","pct_agesex"),with=F],by=c("age"),all.x=T,allow.cartesian = T)
surgs <- surgs[order(location_name,draw,sex,age)]
surgs[,counts:=counts*pct_geog*pct_agesex]

sum(surgs[draw==1 & location_name=="African Union"]$counts)
sum(surgs[draw==1 & location_name!="African Union"]$counts)
check[draw==1]

surgs[,c("pct_geog","pct_agesex"):=NULL]
setnames(surgs,c("counts"),c("postsurg_start"))

toplot <- copy(surgs)
toplot <- toplot[,list(postsurg_start=mean(postsurg_start)),by=c("age","location_name","sex")]
gg <- ggplot(toplot[location_name=="African Union"],aes(x=as.numeric(age),y=postsurg_start)) + geom_bar(stat="identity") 
print(gg)
sum(toplot[location_name=="African Union"]$postsurg_start)
sum(toplot[location_name!="African Union"]$postsurg_start)

saveRDS(surgs,paste0(outdir,"/data/estimated_postsurg_input.RDS"))

