## Matthew Coates

rm(list=ls())

library(data.table)
library(ggplot2)
library(EnvStats)
library(stringr)
library(gtools)
library(scales)
library(grid)
library(gridExtra)

################################################################
## set directories, load functions, load metadata
################################################################
if (Sys.info()[1] == 'Windows') {
  codedir <- paste0("[Insert own directory]/rhd_invest_priv/")
  outdir <- paste0("[Insert own directory]/rhd_investment_case/")
} else {
  codedir <- paste0("[Insert own directory]/rhd_invest_priv/")
  outdir <- paste0("[Insert own directory]/rhd_investment_case/")
}
indir <- paste0(outdir,"/data/")

## load functions
source(paste0(codedir,"/functions/swap_location_names.R"))
source(paste0(codedir,"/functions/map_ids_names.R"))


############################################
## Load and format GBD estimates
############################################

gbd <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_inc_prev_death_inputs.RDS"))
gbd <- map_to_names(gbd,codedir=codedir,keep_ids=T,gbd=2017)
gbd <- gbd[!location_name %in% c("Fiji")] ## fiji in dataset for other purposes

ihd <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_ihd.RDS"))
ihd <-  map_to_names(ihd,codedir=codedir,keep_ids=T,gbd=2017)
ihd <- ihd[metric_name=="Rate"]
ihd[,c("metric_id","metric_name"):=NULL]
## get rates per person to be consistent with other dataset
ihd[,val:=val/100000]
ihd[,upper:=upper/100000]
ihd[,lower:=lower/100000]


gbd <- rbind(gbd,ihd)

d <- copy(gbd[location_name=="African Union" & measure_name=="Deaths" & !age_group_id %in% c(22,2,3,4)])
# extend rates to single-year age groups
d <- d[!age_group_id %in% c(2,3,4,22,27)]
d[age_group_id == 28,age_group_name:="0"]
d[age_group_id==28,age_group_id:=1]
d <- rbind(d,d,d,d,d)
d <- d[order(location_name,year,sex,cause_name,age_group_id)]
d[, id := seq_len(.N), by = c("location_name","year","sex","cause_name","age_group_id")]
d <- d[!(age_group_name=="0" & id > 1) & !(age_group_name=="1 to 4" & id > 4)]
d[,age:=id-1+as.numeric(substr(age_group_name,1,2))]
d <- d[!age %in% c(96:99)]

d <- dcast.data.table(d,year+sex_id+age+age_group_name+age_group_id+sex+measure_name~cause_name,value.var="val")
d[is.na(`Ischemic heart disease`),`Ischemic heart disease`:=0]
d[is.na(`Rheumatic heart disease`),`Rheumatic heart disease`:=0]
d[is.na(`Non-rheumatic valvular heart disease`),`Non-rheumatic valvular heart disease`:=0]
d[,c("age_group_name","sex","Rheumatic heart disease","Non-rheumatic valvular heart disease"),with=F]
d[,envelope:=`Ischemic heart disease` + `Rheumatic heart disease`]
d[,envelope2:=`Cardiovascular diseases`]

###########################################################################
## now get age pattern of death rates from dismod
## load SSA CSMR from DisMod
###########################################################################
dismod <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/gbd_inputs/extracted_epiviz_csmr.xlsx")))
dismod[,age:=1:nrow(dismod)]
dismod <- dismod[age %in% c(1:95)]
dismod[,male:=as.numeric(str_split_fixed(dismod$male,pattern=",",n=Inf)[,2])]
dismod[,female:=as.numeric(str_split_fixed(dismod$female,pattern=",",n=Inf)[,2])]

## adding age 0 and enforcing no deaths in that age group
add <- copy(dismod[age==1])
add[,age:=0]
add[,c("male","female"):=0]
dismod <- rbind(add,dismod)

## this was extracted with 4 extra leading zeros removed (largest value around .001) around age 20
dismod[,male:=male/10000]
dismod[,female:=female/10000]
dismod <- melt(dismod,id.vars=c("age"),value.name="rhd_death_rate",variable.name="sex")
dismod[,rhd_death_rate:=rhd_death_rate]

## merge and rescale dismod curve so that it fits within reasonable assumption of an death envelope 
d <- merge(d,dismod,by=c("age","sex"),all.x=T)

## load populations
pop <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_pop.RDS"))
pop <- pop[location_name=="African Union"]
d <- merge(d,pop[,c("year","sex","age","pop"),with=F],by=c("year","sex","age"),all.x=T)

## convert rates to counts
d[,envelope_deaths:=envelope*pop]
d[,dismod_deaths:=rhd_death_rate*pop]
d[,envelope_deaths2:=envelope2*pop]
d[,codem_deaths:=`Rheumatic heart disease`*pop]

## calibrate to 75% of CVD envelope in ages 5-19
cal <- copy(d[age %in% c(5:19) & year==2017])
cal[,tot_env2:=sum(envelope_deaths2),by=c("year")]
cal[,tot_dismod:=sum(dismod_deaths),by=c("year")]
cal[,rat_5to19:=tot_env2/tot_dismod*.75]


##############################################################
## create comparisons 
##############################################################

## collapse to age groups so that the comparisons are easier
coll <- copy(d[year==2017,list(dismod_deaths=sum(dismod_deaths),codem_deaths=sum(codem_deaths),envelope_deaths=sum(envelope_deaths),envelope_deaths2=sum(envelope_deaths2),
                               pop=sum(pop)),by=c("age_group_id","age_group_name","sex","year")])
sum(coll[year==2017]$dismod_deaths*cal$rat_5to19[1])
coll[,adj_deaths:=dismod_deaths*cal$rat_5to19[1]]
coll[,age_group_factor:=factor(age_group_name,unique(coll$age_group_name))]
coll[,test:=adj_deaths/envelope_deaths2]
coll[,adj_deaths_plus:=adj_deaths] ## create version using adjusted dismod pattern but with codem pattern after age 49
coll[age_group_id >= 14,adj_deaths_plus:=codem_deaths]
coll <- melt(coll,id.vars=c("age_group_name","age_group_id","age_group_factor","sex","year","pop"))
coll[,rate:=value/pop*100000]


gg <- ggplot(data=coll[year==2017 & age_group_id %in% c(1:14) & variable %in% c("adj_deaths","envelope_deaths2","codem_deaths")],aes(x=age_group_factor,y=value,group=variable,color=variable)) + 
  geom_line(size=1.1) + #scale_y_continuous(trans="log",breaks=c(1,10,100,1000,10000,100000)) +
  facet_wrap(~sex) + theme_bw() + scale_color_discrete("",labels=c("GBD CoD Estimates","CVD Envelope","Adjusted DisMod Deaths")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + ylab("Deaths")
print(gg)

gg <- ggplot(data=coll[year==2017 & variable %in% c("adj_deaths","adj_deaths_plus","codem_deaths")],aes(x=age_group_factor,y=value,group=variable,color=variable)) + 
  geom_line(size=1.1) + #scale_y_continuous(trans="log",breaks=c(1,10,100,1000,10000,100000)) +
  facet_wrap(~sex) + theme_bw() + scale_color_discrete("",labels=c("GBD CoD Estimates","Adjusted DisMod Deaths","Adjusted DisMod Deaths+Old Age")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + ylab("Deaths")
print(gg)

sum(coll[variable=="adj_deaths"]$value)
sum(coll[variable=="adj_deaths_plus"]$value)

gg <- ggplot(data=coll[year==2017 & age_group_id %in% c(1:14) & variable %in% c("adj_deaths","envelope_deaths2","codem_deaths")],aes(x=age_group_factor,y=rate,group=variable,color=variable)) + 
  geom_line(size=1.1) + #scale_y_continuous(trans="log",breaks=c(1,10,100,1000,10000,100000)) +
  facet_wrap(~sex) + theme_bw() + scale_color_discrete("",labels=c("GBD CoD Estimates","CVD Envelope","Adjusted DisMod Deaths")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + ylab("Death Rate (per 100,000)")
print(gg)

gg <- ggplot(data=coll[year==2017 & age_group_id %in% c(1:14) & variable %in% c("adj_deaths","envelope_deaths2","codem_deaths")],aes(x=age_group_factor,y=rate,group=variable,color=variable)) +
  geom_line(size=1.1) + scale_y_continuous(trans="log",breaks=c(1,10,100,1000)) +
  facet_wrap(~sex) + theme_bw() + scale_color_discrete("",labels=c("GBD CoD Estimates","CVD Envelope","Adjusted DisMod Deaths")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + ylab("Death Rate (per 100,000)")
print(gg)


gg <- ggplot(data=coll[year==2017 & variable %in% c("adj_deaths","adj_deaths_plus","codem_deaths")],aes(x=age_group_factor,y=rate,group=variable,color=variable)) + 
  geom_line(size=1.1) + #scale_y_continuous(trans="log",breaks=c(1,10,100,1000,10000,100000)) +
  facet_wrap(~sex) + theme_bw() + scale_color_discrete("",labels=c("GBD CoD Estimates","Adjusted DisMod Deaths","Adjusted DisMod Deaths+Old Age")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + ylab("Death Rate (per 100,000)")
print(gg)


gg <- ggplot(data=coll[year==2017 & variable %in% c("adj_deaths","adj_deaths_plus","codem_deaths")],aes(x=age_group_factor,y=rate,group=variable,color=variable)) + 
  geom_line(size=1.1) + scale_y_continuous(trans="log",breaks=c(1,10,100,1000,10000,100000)) +
  facet_wrap(~sex) + theme_bw() + scale_color_discrete("",labels=c("GBD CoD Estimates","Adjusted DisMod Deaths","Adjusted DisMod Deaths+Old Age")) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + ylab("Death Rate (per 100,000)")
print(gg)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

pdf(paste0(outdir,"/results/figures/appendix/death_calibration.pdf"),width=8,height=6)

toplot <- copy(coll[year==2017 & variable %in% c("adj_deaths","adj_deaths_plus","codem_deaths")])
toplot[,varfact:=factor(variable,levels=c("adj_deaths","codem_deaths","adj_deaths_plus"),labels=c("Scaled DisMod Deaths","CODEm Deaths","Deaths Assumed in This Analysis"))]
toplot[,ltype:="solid"]
toplot[variable=="adj_deaths_plus",ltype:="dashed"]
toplot[,ltype:=factor(ltype,levels=c("solid","dashed"))]

cols <- c(gg_color_hue(2),"black")

gg1 <- ggplot(data=toplot,aes(x=age_group_factor,y=rate,group=varfact,color=varfact,linetype=ltype)) + 
  geom_line(size=1.1) + scale_y_continuous(trans="log",breaks=c(1,10,100,1000,10000,100000)) + scale_linetype(guide = 'none') +
  facet_wrap(~sex) + theme_bw() + scale_color_manual("",values=cols) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + ylab("Death Rate (per 100,000)") + theme(legend.position="none") + xlab("Age Group")
print(gg1)

gg2 <- ggplot(data=toplot,aes(x=age_group_factor,y=value,group=varfact,color=varfact,linetype=ltype)) + scale_linetype(guide = 'none') +
  geom_line(size=1.1) + #scale_y_continuous(trans="log",breaks=c(1,10,100,1000,10000,100000)) +
  facet_wrap(~sex) + theme_bw() + scale_color_manual("",values=cols) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + ylab("Deaths") + theme(legend.position="bottom") + xlab("Age Group")
print(gg2)


lay <- rbind(c(1,1,1),
             c(1,1,1),
             c(1,1,1),
             c(1,1,1),
             c(1,1,1),
             c(2,2,2),
             c(2,2,2),
             c(2,2,2),
             c(2,2,2),
             c(2,2,2))

grid.arrange(gg1,gg2,layout_matrix=lay)


dev.off()


coll <- coll[variable %in% c("adj_deaths_plus","codem_deaths")]

coll <- dcast.data.table(coll,age_group_name+age_group_id+sex+year+pop~variable,value.var="rate")
coll[,scalar:=adj_deaths_plus/codem_deaths]
coll[age_group_name==0,scalar:=0]

coll


test <- copy(coll)
test[,count:=adj_deaths_plus*pop/100000]
sum(test$count)
test[,count2:=codem_deaths*pop/100000]
sum(test$count2)



##############################
## Now, use these death numbers to get the implied probability of transitioning to severe disease
##############################

gbd <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_inc_prev_death_inputs.RDS"))
gbd <- map_to_names(gbd,codedir=codedir,keep_ids=T,gbd=2017)
gbd <- gbd[!location_name %in% c("Fiji")] ## fiji in dataset for other purposes


## reshape to fill in zeros
gbd <- dcast.data.table(gbd,location_id+sex_id+age_group_id+year+cause_id+location_name+sex+age_group_name+cause_name~measure_name,value.var=c("val","lower","upper"))
gbd[is.na(val_Deaths),val_Deaths:=0]
gbd[is.na(lower_Deaths),lower_Deaths:=0]
gbd[is.na(upper_Deaths),upper_Deaths:=0]
gbd <- melt(gbd,id.vars=c("location_id","sex_id","age_group_id","year","cause_id","location_name","sex","age_group_name","cause_name"))
gbd[,uncert:=str_split_fixed(variable, "_", Inf)[,1]]
gbd[,measure_name:=str_split_fixed(variable, "_", Inf)[,2]]
gbd[,variable:=NULL]
gbd <- dcast.data.table(gbd,location_id+sex_id+age_group_id+year+cause_id+location_name+sex+age_group_name+cause_name+measure_name~uncert,value.var=c("value"))

## no need to keep prev and inc for all causes
gbd <- gbd[!(cause_name %in% ("All causes") & measure_name %in% c("Prevalence","Incidence")) & location_name=="African Union"]
gbd <- dcast.data.table(gbd,sex_id+age_group_id+year+location_name+sex+age_group_name+cause_name~measure_name,value.var="val")

d <- copy(gbd[location_name=="African Union"])
## extend rates to single-year age groups
d <- d[!age_group_id %in% c(2,3,4,22,27)]
d[age_group_id == 28,age_group_name:="0"]
d[age_group_id==28,age_group_id:=1]
d <- rbind(d,d,d,d,d)
d <- d[order(location_name,year,sex,cause_name,age_group_id)]
d[, id := seq_len(.N), by = c("location_name","year","sex","cause_name","age_group_id")]
d <- d[!(age_group_name=="0" & id > 1) & !(age_group_name=="1 to 4" & id > 4)]
d[,age:=id-1+as.numeric(substr(age_group_name,1,2))]
d <- d[!age %in% c(96:99)]


d[cause_name=="Rheumatic heart disease",cause_name:="RHD"]
d[cause_name=="All causes",cause_name:="AC"]
d <- dcast.data.table(d,year+sex+age+age_group_name+location_name+sex_id~cause_name,value.var=c("Prevalence","Incidence","Deaths"))
d[,c("Prevalence_All causes","Incidence_All causes"):=NULL]


rhd_inputs <- readRDS(paste0(codedir,"data/gbd_inputs/prepped_hf_rhd_inputs.rds"))
rhd_inputs <- rhd_inputs[location_name=="African Union"]
d <- merge(d,rhd_inputs[,c("age","sex","year","rhd_hf_prev_rate"),with=F],by=c("age","sex","year"),all.x=T)
d[,prev_mild:=Prevalence_RHD-rhd_hf_prev_rate]


## load populations
pop <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_pop.RDS"))
pop <- pop[location_name=="African Union"]

d <- merge(d,pop[,c("year","sex","age","pop"),with=F],by=c("year","sex","age"),all.x=T)
if (nrow(d[is.na(pop)]) > 0) stop("missing pop")

## merge on scalars
d <- merge(d,coll[,c("age_group_name","sex","scalar"),with=F],by=c("age_group_name","sex"),all.x=T)
if (nrow(d[is.na(scalar)]) > 0) stop("missing scalar")
d[,rhd_death_rate:=Deaths_RHD*scalar]

gg <- ggplot(d[year %in% c(2000,2005,2010,2015,2017)],aes(x=age,y=rhd_death_rate,group=year,color=year)) + geom_line() + facet_wrap(~sex)
print(gg)


## compute risks
d[,cd_mort_risk:=(1-exp(-1*Deaths_AC))*(Deaths_AC-rhd_death_rate )/Deaths_AC] ## all-cause mort risk without RHD
d[,rhd_mort_risk:=.25] ## rhd mort risk among people with RHD (in severe RHD)
d[,with_cause_mort_risk:=cd_mort_risk+rhd_mort_risk] ## overall mortality risk among people with severe RHD
if (nrow(d[with_cause_mort_risk > .99]) > 0) stop("with cause mort risk too high")

d[,RHD_death_count:=rhd_death_rate*pop]
d[,HF_count_implied:=RHD_death_count/rhd_mort_risk] ## back calculate the implied severe cases from the deaths where RHD deaths = RHD death risk*severe cases
d[,count_prev_rhd:=pop*Prevalence_RHD] ## overall prevalence of RHD
d[,death_in_HF_state:=HF_count_implied*with_cause_mort_risk] ## total deaths in HF state calculated from with-

## if there was more or less a steady state, the transition to HF would be similar to the number of people dying
## the GBD estimates of prevalence/incidence very stable
## but also don't expect much variability in a single year
## though the prevalence of RHD remains fairly steady over time in GBD, this ratio of deaths to prevalence changes quite a bit--
## really a function of the separate estimation processes for mortality and epi in GBD (see appendix discussion)
d[,trans_prob_ballpark:=death_in_HF_state/(prev_mild*pop)] 

## at very old ages, prev_mild is much lower than the actual count 
## cap this a 10% risk of severe disease annually
d[trans_prob_ballpark > .1,trans_prob_ballpark:=.1] ## this occurs at very old ages with very little population

## recalculate HF count implied by the mild prevalence and the transition probability
d[,death_in_HF_state:=trans_prob_ballpark*prev_mild*pop]
d[,HF_count_implied:=death_in_HF_state/with_cause_mort_risk]
if (nrow(d[HF_count_implied <0]) > 0) stop("issue")

## calculate the number of severe and mild cases that persist from the previous year (subtracting those who would have died)
## to obtain starting values for the model
d[,cases_severe_persistent_prev_yr_per_pop:=HF_count_implied-HF_count_implied*rhd_mort_risk]
d[,cases_mild_persistent_prev_yr_per_pop:=prev_mild*pop-trans_prob_ballpark*prev_mild*pop]

gg <- ggplot(d[year==2017],aes(x=age,y=cases_mild_persistent_prev_yr_per_pop)) + geom_line() + 
  facet_wrap(~sex)
print(gg)

gg <- ggplot(d[year==2017],aes(x=age,y=trans_prob_ballpark)) + geom_line() + 
  facet_wrap(~sex) + scale_y_continuous(trans="log",breaks=c(0.001,0.005,0.01,0.1,1))
print(gg)

setnames(d,c("trans_prob_ballpark"),c("inc_implied"))
d[,cases_severe_persistent_prev_yr_per_pop:=cases_severe_persistent_prev_yr_per_pop/pop]
d[,cases_mild_persistent_prev_yr_per_pop:=cases_mild_persistent_prev_yr_per_pop/pop]

## extending these to specific countries using the ratio to the RHD prevalence (both numerator and denominator here are per pop, so pop cancels)
d[,cases_severe_persistent_prev_yr_per_prev:=cases_severe_persistent_prev_yr_per_pop/`Prevalence_RHD`]
d[,cases_mild_persistent_prev_yr_per_prev:=cases_mild_persistent_prev_yr_per_pop/`Prevalence_RHD`]


d[year==2017]

outnewmort <- copy(d[year==2017,c("location_name","sex","year","sex_id","age_group_name","age","rhd_death_rate"),with=F])

gg <- ggplot(outnewmort,aes(x=age,y=rhd_death_rate*100000,color=sex,group=sex)) + geom_line() + scale_y_continuous(trans="log")
print(gg)

## if needed
#saveRDS(outnewmort,paste0(codedir,"/data/other_inputs/newmort_DisMod_codem_adj_hybrid.RDS"))

d <- d[,c("location_name","sex","year","sex_id","age_group_name","age","pop","inc_implied","cases_mild_persistent_prev_yr_per_prev","cases_severe_persistent_prev_yr_per_prev"),with=F]
d[age==0,c("inc_implied","cases_mild_persistent_prev_yr_per_prev","cases_severe_persistent_prev_yr_per_prev"):=0]

d <- d[year==2017]

d[cases_mild_persistent_prev_yr_per_prev < 0]
d[inc_implied > .99]


if (nrow(d[inc_implied < 0 | cases_mild_persistent_prev_yr_per_prev < 0 | cases_severe_persistent_prev_yr_per_prev < 0]) > 0) stop("issue")
if (nrow(d[inc_implied > .99 | cases_mild_persistent_prev_yr_per_prev > .9999999 | cases_severe_persistent_prev_yr_per_prev > .9999999]) > 0) stop("issue")

saveRDS(d,paste0(codedir,"/data/other_inputs/implied_hf_incidence_DisMod_codem_adj_hybrid.RDS"))




####################################
## compare to implied transition probability derived from alternative methods
####################################

  
  implied_hf_inc <- readRDS(paste0(codedir,"/data/other_inputs/implied_hf_incidence.RDS"))
  implied_hf_inc <- implied_hf_inc[year==2017]
  implied_hf_inc[sex_id == 1,sex:="male"]
  implied_hf_inc[sex_id == 2,sex:="female"]
  setnames(implied_hf_inc,"inc_implied","gbd_inc_implied")
  
  soweto <- readRDS(paste0(codedir,"/data/other_inputs/soweto_hf_prob_est.RDS"))
  soweto[sex_id == 1,sex:="male"]
  soweto[sex_id == 2,sex:="female"]
  setnames(soweto,"inc_implied","soweto_inc_implied")
  
  implied_hf_inc <- merge(implied_hf_inc,soweto[,c("sex","age","soweto_inc_implied"),with=F],by=c("sex","age"),all.x=T)
  implied_hf_inc[,watkins_inc_implied:=.008]

  
  ## load estimates from this code
  mainmod <- readRDS(paste0(codedir,"/data/other_inputs/implied_hf_incidence_DisMod_codem_adj_hybrid.RDS"))
  mainmod[sex_id == 1,sex:="male"]
  mainmod[sex_id == 2,sex:="female"]
  mainmod <- mainmod[year==2017]
  setnames(mainmod,"inc_implied","mainmod_inc_implied")
  implied_hf_inc <- merge(implied_hf_inc,mainmod[,c("sex","year","age","mainmod_inc_implied"),with=F],by=c("sex","year","age"),all.x=T)
  
  tp <- implied_hf_inc
  tp[,c("cases_mild_persistent_prev_yr_per_pop","cases_severe_persistent_prev_yr_per_pop"):=NULL]
  tp <- melt(tp,id.vars=c("sex","year","age","location_name","sex_id","age_group_name","pop"))
  tp[,variable:=factor(variable,levels=c("mainmod_inc_implied","gbd_inc_implied","soweto_inc_implied","watkins_inc_implied"),
                       labels=c("Main Model","GBD Approach","GBD + Heart of Soweto Approach","Watkins et al. (2016)"))]
  
  
  pdf(paste0(outdir,"/results/figures/appendix/hf_incidence_scenarios.pdf"),width=8,height=4)
  
  
  gg <- ggplot(data=tp[age>0],aes(x=age,y=value*100,group=variable,color=variable)) + 
    geom_line(size=1.1) + theme_bw() + 
    scale_y_continuous("HF Transition Probability (Percent)",breaks=c(.1,1,10,100),trans="log") +
    scale_color_discrete("Model") + facet_wrap(~sex) +
    xlab("Age")+ theme(legend.key.size =  unit(0.4, "in"),legend.position = "bottom")
  
  print(gg)
  
  dev.off()
  

  


