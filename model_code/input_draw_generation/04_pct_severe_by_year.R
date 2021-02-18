## Matthew Coates
## Derive proportion of severe cases that have been severe for X number of years (1-4 and 5+)

############

rm(list=ls())
library(data.table)
library(ggplot2)
library(EnvStats)
library(stringr)
library(gtools)
library(scales)
library(grid)
library(gridExtra)

## set directories
if (Sys.info()[1] == 'Windows') {
  codedir <- paste0("[INSERT OWN DIRECTORY HERE]/rhd_invest_pub/")
  outdir <- paste0("[INSERT OWN DIRECTORY HERE]/rhd_investment_case/")
  drawnum <- 200
} else {
  codedir <- paste0("[INSERT OWN DIRECTORY HERE]/rhd_invest_pub/")
  outdir <- paste0("[INSERT OWN DIRECTORY HERE]/rhd_investment_case/")
  drawnum <- 1000
}
## load functions
source(paste0(codedir,"/functions/swap_location_names.R"))
source(paste0(codedir,"/functions/map_ids_names.R"))
source(paste0(codedir,"/model_code/functions_separate_causes_uncert.R"))

run_num <- 3
specs <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/run_key.xlsx")))
specs <- specs[run_id == run_num]
specs[,runloc:="African Union"]
drawnum <- 4000

base_year <- 2017
proj_yrs <- max(specs$end_year)-base_year+1 
scale_to <- specs$cov_scale_yr
int_start_year <- 2021

## read in AU locations (https://au.int/en/member_states/countryprofiles2)
locs <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/AU_member_states.xlsx")))
locs <- locs[!location_name %in% c("Sahrawi Republic")] ## we don't have burden estimates for Sahrawi Republic, so we will drop

## convert to GBD country names
locs <- swap_locnames(data=locs,out="GBD",version=2017)
locs <- "African Union"


################################
## load input data
################################

## effect sizes
eff <- readRDS(paste0(outdir,"/data/effect_draws.RDS"))

## starting coverage
cov <- readRDS(paste0(outdir,"/data/starting_coverage.RDS"))

## coverage scale-up governed by a few additional pieces of information
params <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/intervention_params_uncert.xlsx")))
params <- params[,c(1:9),with=F]
setnames(params,c("intervention","intervention_id","effect_type","effect","lower","upper","uncert_dist","coverage_start_au","coverage_start_sd"))
params <- params[,c("intervention_id","coverage_start_au"),with=F]

## load target coverage end from specs
addparams <- copy(specs[,c("intervention_end_coverage1","intervention_end_coverage2","intervention_end_coverage4","intervention_end_coverage5",
                           "intervention_end_coverage6","intervention_end_coverage7"),with=F])
addparams <- melt(addparams)
addparams[,intervention_id:=as.numeric(str_sub(variable,-1,-1))]
addparams[,variable:=NULL]
setnames(addparams,"value","coverage_end_au")
params <- merge(params,addparams,by=c("intervention_id"),all=T)
cov <- merge(cov,params,by=c("intervention_id"),all.x=T)

## find coverage difference in AU overall and end coverage, use that as the gain across all countries rather than scaling every country
## up to the same ending coverage
cov[location_name=="African Union",covdiff:=coverage_end_au-starting_coverage]
cov[,covdiff:=max(covdiff,na.rm=T),by=c("intervention_id","draw")]
cov[,ending_coverage:=covdiff+starting_coverage]

if (any(cov$ending_coverage < 0 | cov$ending_coverage > 1)) stop("bad end coverage")

## if the starting coverage is the same as the ending coverage input, make the draws not change
cov[abs(coverage_start_au-coverage_end_au)<.00001,ending_coverage:=starting_coverage]

if (nrow(cov[starting_coverage > ending_coverage]) > 0) stop("coverage end < coverage_start")

cov <- cov[location_name %in% c(locs)]

params <- merge(cov,eff,by=c("intervention_id","draw"),all.x=T)


## epi 
epi <- readRDS(paste0(outdir,"/data/collated_epi_demog_model_inputs.RDS")) 
epi <- map_to_names(epi,codedir=codedir,keep_ids=T,gbd=2017)
epi <- epi[location_name %in% c(locs)]

varstodrop <- apply(as.matrix(expand.grid(a=c("DisModadj","GBD","GBDadj","Watkins","Soweto"),b=c("_inc_implied","_cases_mild_persistent_prev_yr_count","_cases_severe_persistent_prev_yr_count"),stringsAsFactors=F)),
                    MARGIN=1,paste0,collapse="")
if (specs$hf_inc_option=="Soweto") {
  setnames(epi,c("Soweto_inc_implied","Soweto_cases_mild_persistent_prev_yr_count","Soweto_cases_severe_persistent_prev_yr_count"),
           c("inc_implied","cases_mild_persistent_prev_yr_count","cases_severe_persistent_prev_yr_count"))
  epi[,c(varstodrop):=NULL]
} else if (specs$hf_inc_option=="GBD") {
  setnames(epi,c("GBD_inc_implied","GBD_cases_mild_persistent_prev_yr_count","GBD_cases_severe_persistent_prev_yr_count"),
           c("inc_implied","cases_mild_persistent_prev_yr_count","cases_severe_persistent_prev_yr_count"))
  epi[,c(varstodrop):=NULL]
} else if (specs$hf_inc_option=="DisMod_adj") {
  setnames(epi,c("DisModadj_inc_implied","DisModadj_cases_mild_persistent_prev_yr_count","DisModadj_cases_severe_persistent_prev_yr_count"),
           c("inc_implied","cases_mild_persistent_prev_yr_count","cases_severe_persistent_prev_yr_count"))
  epi[,c(varstodrop):=NULL]
} else {
  stop("code not prepared for this specification")
}
epi <- epi[order(location_name,year,sex,age,draw)]


## demographic projections
dem <- copy(epi[,c("location_name","year","sex","age","draw","mx_allcauses","fert","frac"),with=F])

## FOR DEMOG PROJECTION
## replicate demography params to future
dem <- rbindlist(lapply(as.list(c(2017:(base_year-1+proj_yrs))),FUN=function(x) {
  out <- copy(dem)
  out[,year:=x]
}))


fertscale <- readRDS(paste0(codedir,"/data/other_inputs/future_fert_scalars.RDS"))
dem[,age_start:=floor(age/5)*5]
dem <- merge(dem,fertscale,by=c("location_name","year","age_start"),all.x=T)
dem[sex=="female" & age == 33 & is.na(rat)]
dem[age %in% c(10:54),fert:=fert*rat]
dem[sex=="male" | (age < 10 | age > 54),fert:=0]
if (nrow(dem[is.na(fert)]) > 0) stop("missing fert after merge")
dem[,c("age_start","rat"):=NULL]

mortscale <- readRDS(paste0(codedir,"/data/other_inputs/future_mort_scalars.RDS"))
dem[,age_group_start:=floor(age/5)*5]
dem[age %in% c(1:4),age_group_start:=1]
dem <- merge(dem,mortscale,by=c("location_name","year","sex","age_group_start"),all.x=T)
if (any(is.na(dem$ratio))) stop("missing ratio")
dem[,mx_allcauses:=ratio*mx_allcauses]
dem[,c("age_group_start","ratio"):=NULL]

dem <- dem[order(location_name,year,sex,age,draw)]
setnames(dem,c("location_name","year","sex","age","draw","mort","fert","frac"))
rm(mortscale); rm(fertscale); gc()

## pop object used to track population over the projection as well (create)
pop <- copy(epi[,c("location_name","year","sex","age","draw","pop"),with=F])
pop <- pop[order(location_name,year,sex,age,draw)]

params[,coverage_level:=starting_coverage]

params[,impact_scalar:=1-(effect_draw*(coverage_level-starting_coverage))/(1-effect_draw*starting_coverage)]


epi <- epi[order(location_name,sex,age,draw)]
dem <- dem[order(location_name,sex,age,draw,year)]
setnames(epi,"rhd_hf_prev_rate","prev_severe")

base_year <- 2017
int_start_year <- 2021
round <- 1
total_rounds <- 10

set.seed(706043878)
rhd_mortrisk <- inv.logit(rnorm(n=drawnum,mean=logit(.25),sd=(logit(.35)-logit(.2))/(1.96*2)))
summary(rhd_mortrisk)

## with-cause mortality is excess estimated from GBD plus projected cause-deleted mortality
## though with some specifications, this changes below (using rhd_mortrisk instead)
epi[,with_cause_severe_mort:=mx_rhd/prev_severe+dem[year==base_year+round-1]$mort-(frac*dem[year==base_year+round-1]$mort)]
epi[is.na(with_cause_severe_mort),with_cause_severe_mort:=0]
if (any(epi$with_cause_severe_mort < 0)) stop("cannot be below 0")

## then, with the effect of heart failure management
epi[,with_cause_severe_mort_int:=mx_rhd/prev_severe*rep(params[intervention_id == 4]$impact_scalar,nrow(epi)/drawnum)+dem[year==base_year+round-1]$mort-(frac*dem[year==base_year+round-1]$mort)]
epi[is.na(with_cause_severe_mort_int),with_cause_severe_mort_int:=0]

## cause-deleted mortality based on initial cause fraction and changing mortality
epi[,cd_mort:=dem[year==base_year+round-1]$mort-frac*dem[year==base_year+round-1]$mort]
epi[is.na(cd_mort),cd_mort:=0]

## for mortality among people who have had surgery, for now, temporarily using 80% decrease in cause-specific death rate from severe disease
epi[,post_surg_mort:=mx_rhd/prev_severe*(1-params[intervention_id == 5]$effect_draw)+dem[year==base_year+round-1]$mort-frac*dem[year==base_year+round-1]$mort]
epi[is.na(post_surg_mort),post_surg_mort:=0]

## calculate risks from rates
epi[,ac_mort_risk:=1-exp(-1*dem[year==base_year+round-1]$mort)]
epi[,cd_mort_risk:=ac_mort_risk*epi$cd_mort/dem[year==base_year+round-1]$mort] ## apply cause fraction to all-cause mort risk
epi[,post_surg_mort_risk:=1-exp(-1*post_surg_mort)]
epi[,with_cause_severe_mort_risk:=1-exp(-1*with_cause_severe_mort)]
epi[,with_cause_severe_mort_int_risk:=1-exp(-1*with_cause_severe_mort_int)]

## for certain model specifications, we've derived transition probability to severe disease based on an assumption about
## mortality once in that state (rather than derived purely from GBD or from the Heart of Soweto data)--for these, calculate differently
if (specs$hf_inc_option %in% c("GBD_adj","DisMod","DisMod_adj")) {
  epi[,post_surg_mort_risk:=rep(rhd_mortrisk,nrow(epi)/drawnum)*(1-params[intervention_id == 5]$effect_draw)+cd_mort_risk]
  epi[is.na(post_surg_mort_risk),post_surg_mort_risk:=0]
  
  epi[,with_cause_severe_mort_risk:=rep(rhd_mortrisk,nrow(epi)/drawnum)+cd_mort_risk]
  epi[is.na(with_cause_severe_mort_risk),with_cause_severe_mort_risk:=0]
  
  epi[,with_cause_severe_mort_int_risk:=rep(rhd_mortrisk,nrow(epi)/drawnum)*rep(params[intervention_id == 4]$impact_scalar,nrow(epi)/drawnum)+cd_mort_risk]
  epi[is.na(with_cause_severe_mort_int_risk),with_cause_severe_mort_int_risk:=0]
  
  epi[,post_surg_mort:=-1*log(1-post_surg_mort_risk)]
  epi[,with_cause_severe_mort_int:=-1*log(1-with_cause_severe_mort_int_risk)]
  epi[,with_cause_severe_mort:=-1*log(1-with_cause_severe_mort_risk)]
  
}
if (!specs$hf_inc_option %in% c("GBD_adj","GBD","Soweto","DisMod","DisMod_adj")) stop("need to address risk properly--specification not included")
epi[with_cause_severe_mort_risk < 0]
epi[with_cause_severe_mort_risk > .99999999999999999999999]
if (any(epi$with_cause_severe_mort_risk < 0) | any(epi$with_cause_severe_mort_risk > .9999999999999999999999)) stop("cannot be below 0 or above 1")


## implement primordial prevention assumption
primord_aroc <- (log(1)-log(.85))/13
primord_per <- exp(log(1)-primord_aroc*(round-1))

pop[,pop_int:=pop]


for (loopi in c(1:10)) {
  
  ## get transition probs to severe
  trans_mild_severe <- copy(epi[,c("location_name","sex","age","draw","inc_implied"),with=F])
  trans_mild_severe[,inc_implied:=inc_implied*primord_per]
  trans_mild_severe[,inc_implied_int:=inc_implied*rep(params[intervention_id == 7]$impact_scalar,nrow(trans_mild_severe)/drawnum)]
  
  if (any(trans_mild_severe$inc_implied < 0)) stop("negative implied incidence")
  
  incnum <- copy(epi[,c("location_name","sex","age","draw","inc_rhd"),with=F])
  incnum[,year:=2017+round-1]
  setnames(incnum,"inc_rhd","inc_rate")
  
  incnum <- merge(incnum,copy(pop),by=c("location_name","year","sex","age","draw"),all.x=T)
  ## assuming baseline decline in incidence from primordial prevention
  incnum[,inc:=inc_rate*pop*primord_per]
  incnum[,inc_int:=inc_rate*pop_int*primord_per*1]

  ## regression to normal draws
  set.seed(18250238)
  regression_mean <- (log(.15)+log(.01))/2
  regression_se <- (log(.15)-log(.01))/(2*1.96)
  regression_draws <- exp(rnorm(drawnum,regression_mean,regression_se))
  
  
  ## number with mild RHD (in first iteration, use GBD prevalence)
  mild_prev_premort <- copy(epi[,c("location_name","sex","age","draw","cases_mild_persistent_prev_yr_count"),with=F])
  setnames(mild_prev_premort,"cases_mild_persistent_prev_yr_count","mild_prev")
  mild_prev_premort[,mild_prev_int:=mild_prev]
    
  add <- copy(mild_prev_premort[age==0])
  mild_prev_premort[,age:=age+1]
  mild_prev_premort[age==96,age:=95]
  mild_prev_premort <- mild_prev_premort[,list(mild_prev=sum(mild_prev),mild_prev_int=sum(mild_prev_int)),by=c("location_name","sex","age","draw")]
  mild_prev_premort <- rbind(add,mild_prev_premort)
  mild_prev_premort <- mild_prev_premort[order(sex,age,draw)]
  
  mild_prev_premort[,mild_prev:=mild_prev+incnum$inc]
  mild_prev_premort[,mild_prev_int:=mild_prev_int + incnum$inc]
  
  
  ## we have  (1) transition to death, (2) transition to severe, (3) regression, (4) remainder
  ## should be constrained to 100% -- the transition to death, transition to severe, and regression are mutually exclusive
  ## so first do (1-P1)(1-P2)(1-P3) to get remainder, then do 1- that to get the squeezed one
  ## then break the squeezed probability into 
  regression_probs <- copy(mild_prev_premort)
  setnames(regression_probs,c("mild_prev","mild_prev_int"),c("regression_probs","regression_probs_int"))
  regression_probs[,regression_probs:=rep(regression_draws,length(regression_probs)/200)]
  regression_probs[,regression_probs_int:=rep(regression_draws,length(regression_probs)/200)]
  ## regression just under 20
  regression_probs[age >= 20,regression_probs:=0]
  regression_probs[age >= 20,regression_probs_int:=0]
  
  
  mild_scalar <- 1/(trans_mild_severe$inc_implied+epi$cd_mort_risk+regression_probs$regression_probs) ## in general, this is not below 1, so doesn't apply
  epi[,cd_mort_risk_adj:=cd_mort_risk] ## don't want to change this parameter when used in other places
  trans_mild_severe$inc_implied[mild_scalar < 1] <- trans_mild_severe$inc_implied[mild_scalar < 1]*mild_scalar[mild_scalar < 1]
  epi$cd_mort_risk_adj[mild_scalar < 1] <- epi$cd_mort_risk_adj[mild_scalar < 1]*mild_scalar[mild_scalar < 1]
  regression_probs$regression_probs_int[mild_scalar < 1] <- regression_probs$regression_probs_int[mild_scalar < 1]*mild_scalar[mild_scalar < 1]
  trans_mild_severe$inc_implied_int[mild_scalar < 1] <- trans_mild_severe$inc_implied_int[mild_scalar < 1]*mild_scalar[mild_scalar < 1]
  regression_probs$regression_probs_int[mild_scalar < 1] <- regression_probs$regression_probs_int[mild_scalar < 1]*mild_scalar[mild_scalar < 1]
  
  
  ## now these transitions should be fine because they add to 100%
  ## to severe (hf prob)
  tosevere <- copy(mild_prev_premort)
  setnames(tosevere,c("mild_prev","mild_prev_int"),c("tosevere","tosevere_int"))
  tosevere[,tosevere:=tosevere*trans_mild_severe$inc_implied]
  tosevere[,tosevere_int:=tosevere_int*trans_mild_severe$inc_implied_int]
  
  ## to normal (regression)
  toregress <- copy(mild_prev_premort)
  setnames(toregress,c("mild_prev","mild_prev_int"),c("toregress","toregress_int"))
  toregress[,toregress:=toregress*regression_probs$regression_probs]
  toregress[,toregress_int:=toregress_int*regression_probs$regression_probs_int]
  
  ## number with severe RHD (in first iteration, use GBD number)
  ## then use transition probability--these to add are calculated above
  ## then subtract out deaths after
  sev_prev_premort <- copy(epi[,c("location_name","sex","age","draw","cases_severe_persistent_prev_yr_count"),with=F])
  setnames(sev_prev_premort,"cases_severe_persistent_prev_yr_count","sev_prev")
  if (round == 1) sev_prev_premort[,sev_prev_int:=sev_prev]
  
  add <- copy(sev_prev_premort[age==0])
  sev_prev_premort[,age:=age+1]
  sev_prev_premort[age==96,age:=95]
  sev_prev_premort <- sev_prev_premort[,list(sev_prev=sum(sev_prev),sev_prev_int=sum(sev_prev_int)),by=c("location_name","sex","age","draw")]
  sev_prev_premort <- rbind(add,sev_prev_premort)
  sev_prev_premort <- sev_prev_premort[order(sex,age,draw)]
  sev_prev_premort[,sev_prev:=sev_prev + tosevere$tosevere]
  sev_prev_premort[,sev_prev_int:=sev_prev_int + tosevere$tosevere_int]
  
  ## now apply death risk
  if (loopi == 1) {
    res <- copy(sev_prev_premort)
    setnames(res,c("sev_prev","sev_prev_int"),c("sev_prev1","sev_prev_int1"))
    res[,c("sev_prev2","sev_prev3","sev_prev4","sev_prev5","sev_prev_int2","sev_prev_int3","sev_prev_int4","sev_prev_int5"):=0]
  }
  if (loopi > 1) {
    ## transition to the next set of years if from previous loop, and age forward a year
    res[age < 95,age:=age+1]
    res <- res[,lapply(.SD,sum),by=c("location_name","sex","age","draw"),.SDcols=names(res)[grepl("sev_prev",names(res))]]
    add <- copy(res[age==1])
    add[,age:=0]
    add[,c("sev_prev1","sev_prev_int1","sev_prev2","sev_prev3","sev_prev4","sev_prev5","sev_prev_int2","sev_prev_int3","sev_prev_int4","sev_prev_int5"):=0]
    res <- rbind(add,res)
    res <- res[order(sex,age,draw)]
    ## add for 5
    res[,sev_prev5:=sev_prev5+sev_prev4]
    res[,sev_prev_int5:=sev_prev_int5+sev_prev_int4]
    
    for (sevyrs in c(4:2)) {
      res[,paste0("sev_prev",sevyrs):=res[[paste0("sev_prev",(sevyrs-1))]]]
      res[,paste0("sev_prev_int",sevyrs):=res[[paste0("sev_prev_int",(sevyrs-1))]]]
    } 
    
    ## new people for 1
    res[,sev_prev1:=tosevere$tosevere]
    res[,sev_prev_int1:=tosevere$tosevere_int]
    
  }
  ## apply mort risk with intervention to 1-4
  ## apply full mort risk to 5
  for (sevyrs in c(1:5)) {
    if (sevyrs %in% c(1:4)) {
      ## calculate those left over after exposing to death
      res[,paste0("sev_prev",sevyrs):=res[[paste0("sev_prev",sevyrs)]]*(1-epi$with_cause_severe_mort_risk)]
      res[,paste0("sev_prev_int",sevyrs):=res[[paste0("sev_prev_int",sevyrs)]]*(1-epi$with_cause_severe_mort_int_risk)]
    }
    if (sevyrs == 5) {
      ## after 5 years, we say that the intervention no longer has effect
      res[,paste0("sev_prev",sevyrs):=res[[paste0("sev_prev",sevyrs)]]*(1-epi$with_cause_severe_mort_risk)]
      res[,paste0("sev_prev_int",sevyrs):=res[[paste0("sev_prev_int",sevyrs)]]*(1-epi$with_cause_severe_mort_risk)]
    }
  }


  
}

## now calculate fractions across years
res[,c(paste0("sev_prev_int",c(1:5))):=NULL]
res <- melt(res,id.vars=c("location_name","sex","age","draw"))
res[,value:=value/sum(value),by=c("location_name","sex","age","draw")]
res <- dcast.data.table(res,location_name+sex+age+draw~variable,value.var="value")
res[age==0,c("sev_prev1","sev_prev2","sev_prev3","sev_prev4","sev_prev5"):=0]

saveRDS(res,paste0(outdir,"/data/severe_by_year_start.RDS"))


