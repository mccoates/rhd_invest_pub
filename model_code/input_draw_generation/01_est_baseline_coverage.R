## Matthew Coates
## Derive input assumptions about intervention coverage across countries


rm(list=ls())

library(data.table)
library(ggplot2)
library(EnvStats)
library(stringr)
library(gtools)
library(MASS)

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

numdraws <- 4000

## read in AU locations (https://au.int/en/member_states/countryprofiles2)
locs <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/AU_member_states.xlsx")))
locs <- locs[!location_name %in% c("Sahrawi Republic")] ## we don't have burden estimates for Sahrawi Republic, so we will drop

## convert to GBD country names
locs <- swap_locnames(data=locs,out="GBD",version=2017)

## load intervention baseline coverage for the AU
params <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/intervention_params_uncert.xlsx")))
params <- params[,c(1:9),with=F]
setnames(params,c("intervention","intervention_id","effect_type","effect","lower","upper","uncert_dist","coverage_start","coverage_start_sd"))

## need to make these country-leve starting coverage estimates (for intervention_ids 1,2,4-7)



#############
## There is little information on coverage
## Starting coverage has relatively low effect on the model outcomes, as they are mostly linked to the change in coverage unrelated to the starting level
## But we do need an assumption about starting level for starting parameters
## We assume that the coverage has a roughly linear relationship with the UHC index constructed by IHME
## We also assume some aggregate coverage values across the AU
## We can therefore create a spread for country-level coverage consistent with these assumptions
uhc <- readRDS(paste0(codedir,"/data/gbd_inputs/GBD_UHC_index.RDS"))

## load numbers we need for weighting so that the country-level coverage numbers aggregate to AU estimate using weighted average
## some interventions require different numbers than others
## intervention_id 1: Pharyngitis (children aged 5-15)--no good data on variation across countries, assume weighted by population in age range
## intervention_id 2: Prophylactic penicillin for people with history of acute RF--no good measure of this; the incidence of RHD is a good proxy for levels of ARF
## intervention_id 3: N/A
## intervention_id 4: HF management--assume roughly proportional to RHD deaths (don't have good estimates of severity distribution across countries, likely similar)
## intervention_id 5: Cardiac surgery--assume roughly proportional to RHD deaths under 40 
## intervention_id 6: N/A--not evaluated
## intervention_id 7: prophylactic penicillin for RHD--RHD prevalence

## get epi info we need: pop 5-15, RHD incidence, RHD prevalence, RHD deaths under 40, RHD deaths
gbd <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_inc_prev_death_inputs.RDS"))
gbd <- gbd[age_group_id %in% c(2:20,30:32,235) & year ==2017]
gbd <- map_to_names(d=gbd,codedir=codedir,keep_ids=T,gbd=2017)
gbd <- gbd[location_name %in% c(locs$location_name,"African Union")]
gbd[,c("age","loc_level"):=NULL]
gbd <- gbd[cause_name=="Rheumatic heart disease"]
gbd <- dcast.data.table(gbd,cause_name+location_name+sex+age_group_name+age_group_id+year~measure_name,value.var="val")
gbd[,u40_deaths:=Deaths]
gbd[age_group_id > 12,u40_deaths:=0] 

pop <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_pop_aggage.RDS"))
gbd <- merge(gbd,pop,by=c("location_name","sex","age_group_id","year"),all.x=T)
gbd[,pop5to14:=0]
gbd[age_group_id %in% c(6:7),pop5to14:=pop]
gbd[,Deaths:=Deaths*pop]
gbd[,u40_deaths:=u40_deaths*pop]
gbd[,Prevalence:=Prevalence*pop]
gbd[,Incidence:=Incidence*pop]

gbd <- gbd[,list(Deaths=sum(Deaths),u40_deaths=sum(u40_deaths),
                 Prevalence=sum(Prevalence),Incidence=sum(Incidence),
                 pop5to14=sum(pop5to14),pop=sum(pop)),by=c("location_name","cause_name")]


gbd <- merge(gbd,uhc,by=c("location_name"),all.x=T)
gbd <- gbd[year == 2019]

## could use overall index, or e.g. CKD treatment, cancer treatment, diabetes treatment, IHD treatment, stroke treatment, appendicitis treatment
## but none really represents cardiac surgery
## likely that e.g. CKD treatment, cancer treatment, stroke treatment, appendicitis treatment represent more complex care?
gbd <- dcast.data.table(gbd,location_name+Deaths+u40_deaths+Prevalence+Incidence+pop5to14+pop+year~indicator_name,value.var="val")

gg <- ggplot(data=gbd,aes(x=`Appendicitis treatment`,y=`UHC effective coverage index`)) + geom_point() + geom_label(aes(label=location_name))
print(gg)

gg <- ggplot(data=gbd,aes(x=`Stroke treatment`,y=`UHC effective coverage index`)) + geom_point() + geom_label(aes(label=location_name))
print(gg)

cor(gbd[,c("UHC effective coverage index","CKD treatment","Stroke treatment","Appendicitis treatment")])
## but overall index more intuitive--there may be some odd modeling artefacts in individual conditions in the GBD index (e.g. Kenya appendicitis)

gbd <- gbd[,c("location_name","Deaths","u40_deaths","Prevalence","Incidence","pop5to14","pop","UHC effective coverage index"),with=F]
setnames(gbd,c("UHC effective coverage index"),c("index"))

## now, we want to transform the index into a coverage such that the weighted average coverage is the coverage specified in the parameters file for the AU
## and the coverage varies linearly with the index values
## intervention_id 1: Pharyngitis (children aged 5-15)--no good data on variation across countries, assume weighted by population in age range
## intervention_id 2: Prophylactic penicillin for people with history of acute RF--no good measure of this; the incidence of RHD is a good proxy for levels of ARF
## intervention_id 3: N/A
## intervention_id 4: HF management--assume roughly proportional to RHD deaths (don't have good estimates of severity distribution across countries, likely similar)
## intervention_id 5: Cardiac surgery--assume roughly proportional to RHD deaths under 40 
## intervention_id 6: N/A--not evaluated
## intervention_id 7: prophylactic penicillin for RHD--RHD prevalence
gbd <- gbd[!location_name %in% c("African Union")]

## NOTE--THE CODE HERE USES "pctile"--THESE AREN'T PERCENTILES--THEY ARE VALUES STANDARDIZED TO THE MIN AND MAX OBSERVED ON THE INDEX
gbd[,index_pctile:=(index-min(index))/(max(index)-min(index))]


## intervention 1
intervention_1_min <- .05
gbd[,weighted_mean:=weighted.mean(index,w=pop5to14)]
gbd[,weighted_mean_pctile:=(weighted_mean-min(index))/(max(index)-min(index))]
gbd[,coverage_1:=index_pctile*(params[intervention_id == 1]$coverage_start-intervention_1_min)/weighted_mean_pctile+intervention_1_min]
gbd[,check:=sum(coverage_1*pop5to14)/sum(pop5to14)]

## intervention 2
intervention_2_min <- .02
gbd[,weighted_mean:=weighted.mean(index,w=Incidence)]
gbd[,weighted_mean_pctile:=(weighted_mean-min(index))/(max(index)-min(index))]
gbd[,coverage_2:=index_pctile*(params[intervention_id == 2]$coverage_start-intervention_2_min)/weighted_mean_pctile+intervention_2_min]
gbd[,check:=sum(coverage_2*Incidence)/sum(Incidence)]

## intervention 4
intervention_4_min <- .05
gbd[,weighted_mean:=weighted.mean(index,w=Deaths)]
gbd[,weighted_mean_pctile:=(weighted_mean-min(index))/(max(index)-min(index))]
gbd[,coverage_4:=index_pctile*(params[intervention_id == 4]$coverage_start-intervention_4_min)/weighted_mean_pctile+intervention_4_min]
gbd[,check:=sum(coverage_4*Deaths)/sum(Deaths)]

## intervention 5
intervention_5_min <- .02
gbd[,weighted_mean:=weighted.mean(index,w=u40_deaths)]
gbd[,weighted_mean_pctile:=(weighted_mean-min(index))/(max(index)-min(index))]
gbd[,coverage_5:=index_pctile*(params[intervention_id == 5]$coverage_start-intervention_5_min)/weighted_mean_pctile+intervention_5_min]
gbd[,check:=sum(coverage_5*u40_deaths)/sum(u40_deaths)]

## intervention 7
intervention_7_min <- .02
gbd[,weighted_mean:=weighted.mean(index,w=Prevalence)]
gbd[,weighted_mean_pctile:=(weighted_mean-min(index))/(max(index)-min(index))]
gbd[,coverage_7:=index_pctile*(params[intervention_id == 7]$coverage_start-intervention_7_min)/weighted_mean_pctile+intervention_7_min]
gbd[,check:=sum(coverage_7*Prevalence)/sum(Prevalence)]

gbd <- gbd[,c("location_name",paste0("coverage_",c(1,2,4,5,7)),"pop5to14","Incidence","Deaths","u40_deaths","Prevalence"),with=F]
gbd <- melt(gbd,id.vars=c("location_name","pop5to14","Incidence","Deaths","u40_deaths","Prevalence"),value.name="coverage_start")
gbd[,intervention_id:=as.numeric(gsub("coverage_","",variable))]
gbd[,variable:=NULL]
setnames(params,c("coverage_start","coverage_start_sd"),c("coverage_start_au","coverage_start_au_sd"))
gbd <- merge(gbd,params[,c("intervention_id","coverage_start_au","coverage_start_au_sd"),with=F],by=c("intervention_id"),all.x=T)
gbd[,logit_coverage_start_au:=logit(coverage_start_au)]
gbd[,logit_coverage_start_au_sd:=(logit(coverage_start_au+coverage_start_au_sd*1.96)-logit(coverage_start_au-coverage_start_au_sd*1.96))/(2*1.96)] ## could use delta method instead, but the uncertainty here is a guess regardless
gbd[,logit_coverage_start:=logit(coverage_start)]
gbd[,weight:=0]
gbd[intervention_id==1,weight:=pop5to14]
gbd[intervention_id==2,weight:=Incidence]
gbd[intervention_id==4,weight:=Deaths]
gbd[intervention_id==5,weight:=u40_deaths]
gbd[intervention_id==7,weight:=Prevalence]

## derive an SD such that when we aggregate countries, the sd for the AU reflects the initial sd specified
## need to draw these correlated across countries--if independent, uncertainty for aggregate will be very small (sd shrinks by order of magnitude)
## don't draw separately for intervention 2 and 7
cormat <- matrix(rep(1,length(unique(gbd$location_name))*length(unique(gbd$location_name))),ncol=length(unique(gbd$location_name)))

au_sd_implied <- c()
au_mean_implied <- c()
for (i in c(1,2,4,5)) {
  
  cat(paste0("running ", i,"\n")); flush.console()
  
  sds <- gbd[intervention_id == i]$logit_coverage_start_au_sd
  d <- diag(sds)
  covarmat <- d%*%cormat%*%d
  sims <- matrix(mvrnorm(n=10000,mu=gbd[intervention_id == i]$logit_coverage_start,
                       Sigma=covarmat),ncol=10000,byrow=T)
  au <- cbind(copy(gbd[intervention_id == i]),sims)
  au <- au[,lapply(.SD,weighted.mean,w=weight),by=c("intervention_id"),.SDcols=c(paste0("V",c(1:10000)))]
  au_sd_implied <- c(au_sd_implied,apply(inv.logit(as.matrix(au[,c(2:ncol(au)),with=F])),MARGIN=1,sd))
  au_mean_implied <- c(au_mean_implied,apply(inv.logit(as.matrix(au[,c(2:ncol(au)),with=F])),MARGIN=1,mean))
  
}
au_sd_implied
au_mean_implied
params
## approximate au inputs-- discrepancy actually from sd input in normal space not really reflecting logit (taking mean in normal space)
cor(sims[1,],sims[2,])

## now create sims to pass on
set.seed(5034606)
dat <- list()
for (i in c(1,2,4,5)) {
  
  cat(paste0("running ", i,"\n")); flush.console()
  
  sds <- gbd[intervention_id == i]$logit_coverage_start_au_sd
  d <- diag(sds)
  covarmat <- d%*%cormat%*%d
  sims <- inv.logit(matrix(mvrnorm(n=numdraws,mu=gbd[intervention_id == i]$logit_coverage_start,
                         Sigma=covarmat),ncol=numdraws,byrow=T))
  dat[[paste0(i)]] <- cbind(copy(gbd[intervention_id == i]),sims)

}
dat[[paste0(7)]] <- copy(dat[[paste0(2)]])
dat[[paste0(7)]][,intervention_id:=7]
dat <- rbindlist(dat)


## create AU agg
au <- copy(dat)
au <- au[,lapply(.SD,weighted.mean,w=weight),by=c("intervention_id"),.SDcols=c(paste0("V",c(1:numdraws)))]
au[,location_name:="African Union"]

## create regional aggs
au <- copy(dat)
au <- au[,lapply(.SD,weighted.mean,w=weight),by=c("intervention_id"),.SDcols=c(paste0("V",c(1:numdraws)))]
au[,location_name:="African Union"]

regs <- copy(dat)
regs <- merge(regs,locs[,c("location_name","au_region"),with=F],by=c("location_name"),all.x=T)
regs <- regs[,lapply(.SD,weighted.mean,w=weight),by=c("au_region","intervention_id"),.SDcols=c(paste0("V",c(1:numdraws)))]
setnames(regs,"au_region","location_name")

dat <- dat[,c("intervention_id","location_name",paste0("V",c(1:numdraws))),with=F]
dat <- rbind(dat,au)
dat <- rbind(dat,regs)
dat <- melt(dat,id.vars=c("intervention_id","location_name"),value.name="starting_coverage",variable.name="draw")
dat[,draw:=as.numeric(gsub("V","",draw))]

sumout <- copy(dat[location_name %in% c("African Union","Eastern Africa","Central Africa","Southern Africa","Northern Africa","Western Africa") & draw %in% c(1:1000)])
sumout <- sumout[,list(mean=mean(starting_coverage*100),lower=quantile(starting_coverage*100,probs=.025),upper=quantile(starting_coverage*100,probs=.975)),by=c("location_name","intervention_id")]
sumout[,disp:=paste0(round(mean,1)," (",round(lower,1),"-",round(upper,1),")")]
sumout <- merge(sumout,params[,c("intervention","intervention_id"),with=F],by=c("intervention_id"),all.x=T)
sumout <- sumout[order(intervention_id)]
#sumout <- sumout[,c("location_name","intervention","disp"),with=F]
sumout[,intervention:=factor(intervention,levels=c(unique(sumout$intervention)))]
sumout <- dcast.data.table(sumout,intervention~location_name,value.var="disp")

write.csv(sumout,paste0(outdir,"/results/tables/appendix/starting_coverage_summary.csv"))



saveRDS(dat,paste0(outdir,"/data/starting_coverage.RDS"))



