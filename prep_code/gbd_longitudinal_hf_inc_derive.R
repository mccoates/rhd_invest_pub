## Matthew Coates


####################################################
## read in data 
####################################################
rm(list=ls())
library(data.table)
library(ggplot2)
library(scales)

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


gbd <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_inc_prev_death_inputs.RDS"))
gbd <- map_to_names(gbd,codedir=codedir,keep_ids=T,gbd=2017)
gbd <- gbd[!location_name %in% c("Fiji")] ## fiji in dataset for other purposes
gbd <- dcast.data.table(gbd,location_id+sex_id+age_group_id+year+cause_id+location_name+sex+age_group_name+cause_name~measure_name,value.var="val")

d <- copy(gbd[location_name=="African Union"])
## extend rates to single-year age groups
d <- d[!age_group_id %in% c(2,3,4,22,27)]
d[age_group_id == 28,age_group_name:="0"]
d[age_group_id==28,age_group_id:=1]
d <- rbind(d,d,d,d,d)
d <- d[order(location_name,sex,cause_name,age_group_id,year)]
d[, id := seq_len(.N), by = c("location_name","year","sex","cause_name","age_group_id")]
d <- d[!(age_group_name=="0" & id > 1) & !(age_group_name=="1 to 4" & id > 4)]
d[,age:=id-1+as.numeric(substr(age_group_name,1,2))]
d <- d[!age %in% c(96:99)]

## load populations
pop <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_pop.RDS"))
pop <- pop[location_name=="African Union"]
d <- merge(d,pop[,c("year","sex","age","pop"),with=F],by=c("year","sex","age"),all.x=T)


if (any(is.na(d$pop)) | any(is.na(d[cause_name=="Rheumatic heart disease"]$Prevalence))) stop('missing')
d[is.na(Deaths),Deaths:=0]

d <- dcast.data.table(d,age+sex+location_name+year+sex_id+pop~cause_name,value.var=c("Deaths","Incidence","Prevalence"))
setnames(d,c("Deaths_All causes","Deaths_Rheumatic heart disease","Incidence_All causes","Incidence_Rheumatic heart disease","Prevalence_All causes","Prevalence_Rheumatic heart disease"),
         c("mx_all","mx_rhd","incidence_all","incidence_rhd","prevalence_all","prevalence_rhd"))
d[is.na(mx_rhd),mx_rhd:=0]

## merge with more detailed RHD HF info
## severe to stay severe is 1 minus probability of death from severe also minus transition to to post-cardiac surgery if applicable)
rhd_inputs <- readRDS(paste0(codedir,"data/gbd_inputs/prepped_hf_rhd_inputs.rds"))
rhd_inputs <- rhd_inputs[location_name == "African Union"]

d <- merge(d,rhd_inputs[,c("age","location_name","sex","year","rhd_hf_prev_rate"),with=F],by=c("age","location_name","sex","year"),all=T)

d[is.na(rhd_hf_prev_rate)]
d[is.na(pop)]

d[,c("incidence_all","prevalence_all"):=NULL]


## if all RHD deaths were from HF
## then the with-cause mortality would be 
d[,with_cause_mx:=mx_all-mx_rhd+mx_rhd/rhd_hf_prev_rate]
d[,with_cause_qx:=1-exp(-1*with_cause_mx)]
d[is.na(with_cause_mx),with_cause_mx:=0]
d[is.na(with_cause_qx),with_cause_qx:=0]
d[,cases:=pop*with_cause_mx*rhd_hf_prev_rate*(1/with_cause_qx)] 
d[mx_rhd==0,cases:=0]
d[,deaths_mx:=pop*with_cause_mx*rhd_hf_prev_rate]
d[,deaths_qx:=with_cause_qx*cases]
d[,leftover_cases:=cases-deaths_qx]


## now, from the leftover cases from the previous year, calculate the implied incident cases
past <- copy(d[,c("age","location_name","sex","year","sex_id","leftover_cases"),with=F])
past[,year:=year+1]
past[,age:=age+1]
past[age>95,age:=95]
past <- past[,list(prev_year_leftover_cases=sum(leftover_cases)),by=c("age","location_name","sex","year","sex_id")]
d <- merge(d,past,by=c("age","location_name","sex","year","sex_id"),all.x=T)
d[is.na(prev_year_leftover_cases) & age==0,c("prev_year_leftover_cases","prev_year_half_leftover_cases"):=0]
d[,inc_hf_implied:=cases-prev_year_leftover_cases] 
summary(d$inc_hf_implied)
d[inc_hf_implied < 0] ## negatives only at age 55 in men

## the number leaving is HF incidence + cause-deleted mortality 
d[,prev_mild:=prevalence_rhd-rhd_hf_prev_rate]
d[,prev_mild_count:=prev_mild*pop]
regression <- exp((log(.15)+log(.01))/2)
d[,num_leaving_mild:=(mx_all-mx_rhd)*(prev_mild)+inc_hf_implied]
d[age<=19,num_leaving_mild:=(mx_all-mx_rhd)*(prev_mild)+inc_hf_implied+regression*prev_mild] ## under 20, incorporate regression to normal
d[,rhd_inc_count:=incidence_rhd*pop]
d[,inc_mild_frac_prev_mild:=rhd_inc_count/prev_mild_count]

## get rate of leaving (death or transition to HF) mild
d[,rate_leaving_mild:=num_leaving_mild/prev_mild_count]
d[,prob_leaving_mild:=1-exp(-1*rate_leaving_mild)]

## then, using probability, back-calculate some "number" of cases
d[,cases_mild:=(num_leaving_mild)/prob_leaving_mild]

## then, subtract out the RHD incidence to get number held over from previous year
d[,num_mild_persistent_last_year:=cases_mild-rhd_inc_count]

summary(d$num_mild_persistent_last_year)
d[age==0,c("cases_mild","num_mild_persistent_last_year"):=0]

## save this for HoS code
saveRDS(d,paste0(codedir,"/data/other_inputs/full_inc_cases_long_calcs.RDS"))

## for more consistency over ages, pool ages to get the implied incidence rates
out <- copy(d)
out[,prev_mild_count_denom:=cases_mild]
out[,count_persistent_cases_prev:=num_mild_persistent_last_year]
out <- out[,c("age","location_name","sex","year","sex_id","prevalence_rhd","prev_mild_count_denom","count_persistent_cases_prev","prev_year_leftover_cases","inc_hf_implied","pop"),with=F]
out[,agestart:=floor(age/5)*5]
out[age %in% c(1,2,3,4),agestart:=1]
out[,ageend:=floor(age/5)*5+4]
out[age==0,ageend:=0]
out[,age_group_name:=paste0(agestart," to ",ageend)]

## keeping to be able to expand to the full single-year ages again
backout <- copy(out)

out <- out[,list(prev_mild_count_denom=sum(prev_mild_count_denom),count_persistent_cases_prev=sum(count_persistent_cases_prev),prev_year_leftover_cases=sum(prev_year_leftover_cases),
                 inc_hf_implied=sum(inc_hf_implied),
                 pop=sum(pop)),by=c("location_name","sex","year","sex_id","age_group_name")]
out[,inc_implied:=inc_hf_implied/prev_mild_count_denom]
out[prev_mild_count_denom==0,inc_implied:=0]

out[,cases_mild_persistent_prev_yr_per_pop:=count_persistent_cases_prev/pop]
out[,cases_severe_persistent_prev_yr_per_pop:=prev_year_leftover_cases/pop]
out <- out[year %in% c(2001:2017)]

out[,age_group_factor:=factor(age_group_name,levels=c("0 to 0","1 to 4",paste0(seq(5,95,by=5)," to ",seq(9,99,by=5))))]
out[,sex_factor:=factor(sex,levels=c("female","male"),labels=c("Female","Male"))]


sum(out[year==2017]$inc_hf_implied)

out[,pop:=NULL]
out <- merge(out,backout[,c("location_name","sex","year","sex_id","age_group_name","age","prevalence_rhd","pop"),with=F],by=c("location_name","sex","year","sex_id","age_group_name"),all.x=T)

out[,cases_mild_persistent_prev_yr_per_prev:=cases_mild_persistent_prev_yr_per_pop/prevalence_rhd]
out[,cases_severe_persistent_prev_yr_per_prev:=cases_severe_persistent_prev_yr_per_pop/prevalence_rhd]

saveRDS(out[,c("location_name","sex","year","sex_id","age_group_name","age","pop","inc_implied","cases_mild_persistent_prev_yr_per_prev","cases_severe_persistent_prev_yr_per_prev"),with=F],
paste0(codedir,"/data/other_inputs/implied_hf_incidence.RDS"))




