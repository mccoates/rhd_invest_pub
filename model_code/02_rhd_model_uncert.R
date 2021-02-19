############

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
  rm(list=ls()) ## not clearing workspace on cluster so we can use commandArgs() hack
  codedir <- paste0("[Insert own directory]/rhd_invest_priv/")
  outdir <- paste0("[Insert own directory]/rhd_investment_case/")
  outdirtmp <- outdir
  drawnum <- 50
} else {
  codedir <- paste0("[Insert own directory]/rhd_invest_priv/")
  outdir <- paste0("[Insert own directory]/rhd_investment_case/")
  outdirtmp <- paste0("[Insert own directory]")
  drawnum <- 1000
}
## load functions
source(paste0(codedir,"/functions/swap_location_names.R"))
source(paste0(codedir,"/functions/map_ids_names.R"))
source(paste0(codedir,"/model_code/functions_separate_causes_uncert.R"))


manual <- F
###########################
## Options for what to run
if (manual) {
  run_num <- 34
  specs <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/run_key.xlsx")))
  specs <- specs[run_id == run_num]
} else {
  if (Sys.info()[1] == 'Windows') {
    runs <- dir(paste0(outdir,"/results/model_runs/"))
    run_num <- as.numeric(gsub("\\.csv","",gsub("run","",runs)))
    if (length(run_num) != 1) stop("There is more than one model run active--can't tell which to run")
    specs <- fread(paste0(outdir,"/results/model_runs/run",run_num,".csv"))
  } else {
    run_num <- commandArgs()[2]
    specs <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/run_key.xlsx")))
    specs <- specs[run_id == run_num]
  }
}
###########################
if (Sys.info()[1] == 'Windows' & drawnum != specs$drawnum) specs$drawnum <- drawnum
drawnum <- specs$drawnum

base_year <- 2017
proj_yrs <- max(specs$end_year)-base_year+1 
scale_to <- specs$cov_scale_yr
int_start_year <- 2021

## read in AU locations (https://au.int/en/member_states/countryprofiles2)
locs <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/AU_member_states.xlsx")))
locs <- locs[!location_name %in% c("Sahrawi Republic")] ## we don't have burden estimates for Sahrawi Republic, so we will drop

## convert to GBD country names
locs <- swap_locnames(data=locs,out="GBD",version=2017)
if (specs$runloc == "AU") {
  locs <- "African Union"
  locsshort <- "AU"
} else if (specs$runloc == "regs" & Sys.info()[1] == 'Windows') {
  locs <- unique(locs$au_region)
} else if (specs$runloc=="regs" & Sys.info()[1] != "Windows") {
  locs <- commandArgs()[1]
  locsshort <- locs ## b/c filepaths can't have spaces
  print(locsshort)
  locs <- paste0(locs," Africa")
}  else if (specs$runloc %in% c("DRC","SA")) {
  locsshort <- specs$runloc
  locs <- ifelse(locsshort=="DRC","Democratic Republic of the Congo","South Africa")
  print(locsshort)
} else {
  locs <- locs$location_name
  stop("incorrect specification to run properly")
}
print(locs)


################################
## load input data
################################
## also load the estimates generated for pharyngitis/ARF
phar_arf <- readRDS(paste0(outdirtmp,"/results/model_run_results/",run_num,"/projected_phar_arf_params",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".rds"))
phar_arf <- phar_arf[location_name %in% locs]
phar_arf <- phar_arf[draw %in% c(1:drawnum)]

hist_arf <- readRDS(paste0(outdirtmp,"/results/model_run_results/",run_num,"/arf_history",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".rds"))
hist_arf <- hist_arf[location_name %in% locs]
hist_arf <- hist_arf[draw %in% c(1:drawnum)]

## load history of severe
hist_sev <- readRDS(paste0(outdir,"/data/severe_by_year_start.RDS"))
hist_sev[,location_name:=NULL]
hist_sev <- hist_sev[draw %in% c(1:drawnum)]
#hist_sev <- hist_sev[location_name %in% locs]

## effect sizes
eff <- readRDS(paste0(outdir,"/data/effect_draws.RDS"))
eff <- eff[draw %in% c(1:drawnum)]

## starting coverage
cov <- readRDS(paste0(outdir,"/data/starting_coverage.RDS"))
cov <- cov[draw %in% c(1:drawnum)]

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

## for scenarios with 100% coverage of an intervention in the AU, each country needs to be 100%
cov[coverage_end_au==1,ending_coverage:=1]

## if the starting coverage is the same as the ending coverage input, make the draws not change
cov[abs(coverage_start_au-coverage_end_au)<.00001,ending_coverage:=starting_coverage]

if (any(cov$ending_coverage < 0 | cov$ending_coverage > 1)) stop("bad end coverage")

if (nrow(cov[starting_coverage > ending_coverage]) > 0) stop("coverage end < coverage_start")

cov <- cov[location_name %in% c(locs)]

params <- merge(cov,eff,by=c("intervention_id","draw"),all.x=T)
params <- params[draw %in% c(1:drawnum)]


## epi 
epi <- readRDS(paste0(outdir,"/data/collated_epi_demog_model_inputs.RDS")) 
epi <- map_to_names(epi,codedir=codedir,keep_ids=T,gbd=2017)
epi <- epi[location_name %in% c(locs)]
epi <- epi[draw %in% c(1:drawnum)]

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
## did not save these separately because they will be quite large
## keep veriables here for projecting forward
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


deaths <- list() ## create object to track deaths with no intervention
deaths_int <- list() ## create object to track deaths with interventions
pop[,pop_int:=pop] ## track pop for the scenario of changing coverage
dis <- list() ## disease outputs

#backepi <- copy(epi)

for (i in c(1:proj_yrs)) { ## loop over years projecting forward
  
  cat(paste0("projecting for year ",(base_year+i-1),"\n")); flush.console()
  
  ##############################
  ## run model
  ##############################
  if (i == 1) {
    dis[["Rheumatic heart disease"]] <- rhd(pop=pop,dem=dem,epi=copy(epi),hist_sev=hist_sev,params=copy(params),
                                            results=NA,round=i,total_rounds=proj_yrs,scale_to=scale_to,
                                            int_start_year=int_start_year,base_year=base_year,
                                            hist_arf=hist_arf,phar_arf=phar_arf,specs=specs,drawnum=drawnum)
  } else {
    dis[["Rheumatic heart disease"]] <- rhd(pop=pop,dem=dem,epi=copy(epi),hist_sev=hist_sev,params=copy(params),
                                            results=dis[["Rheumatic heart disease"]],round=i,total_rounds=proj_yrs,
                                            scale_to=scale_to,int_start_year=int_start_year,base_year=base_year,
                                            hist_arf=hist_arf,phar_arf=phar_arf,specs=specs,drawnum=drawnum)
  }

  ##############################
  ## this code initially set up to run different disease models, bring together for demographic projection (thus this format)
  ## run next step of demographic projection, both for intervention/non-intervention
  ##############################
  
  ## get cause-deleted mortality
  ## use cause fractions and apply to projected mortality (because we're using projected all-cause mortality)
  cdmort <- copy(epi[,c("location_name","sex","age","draw","frac"),with=F])
  cdmort <- merge(cdmort,copy(dem[year==(base_year+i-1),c("location_name","sex","age","mort","draw"),with=F]),by=c("location_name","sex","age","draw"),all=T) ## merge with all-cause--this could be sped up without merging if ordering is correct, but staying safe unless we need to optimize for speed
  cdmort[,cdmort:=mort-(mort*frac)] ## subtract to get cause-deleted mortality
  if (any(is.na(cdmort$cdmort) | cdmort$cdmort < 0)) stop("missing or negative values") ## make sure no missingness
  
  if (i == 1) {
    deaths <- rbindlist(copy(dis),fill=T,use.names=T)
    deaths <- deaths[,list(deaths_cause=sum(deaths_cause),deaths_cause_int=sum(deaths_cause_int)),by=c("location_name","year","sex","age","draw")]
    deaths <- merge(deaths,copy(pop),by=c("location_name","year","sex","age","draw"),all.x=T)
    deaths[,deaths_total:=deaths_cause+cdmort$cdmort*pop]
    deaths[,deaths_total_int:=deaths_cause_int+cdmort$cdmort*pop_int]
    
  } else {

    tmp <- rbindlist(copy(dis),fill=T,use.names=T)
    tmp <- tmp[year==(base_year+i-1),list(deaths_cause=sum(deaths_cause),deaths_cause_int=sum(deaths_cause_int)),by=c("location_name","year","sex","age","draw")]
    tmp <- merge(tmp,copy(pop),by=c("location_name","year","sex","age","draw"),all.x=T)
    tmp[,deaths_total:=deaths_cause+cdmort$cdmort*pop]
    tmp[,deaths_total_int:=deaths_cause_int+cdmort$cdmort*pop_int]
    
    deaths <- rbind(deaths,tmp)
    deaths <- deaths[order(location_name,year,sex,age,draw)]
    
  }
  
  sexrat <- 1.03 ## assumed
  bir <- copy(dem[sex=="female" & year == (base_year+i-1)])
  bir[,births:=fert*pop[year==(base_year+i-1) & sex=="female"]$pop]
  bir[,births_int:=fert*pop[year==(base_year+i-1) & sex=="female"]$pop_int]
  bir <- bir[,list(births=sum(births),births_int=sum(births_int)),by=c("location_name","draw")]
  bir[,female:=births/(sexrat+1)]
  bir[,male:=births-female]
  bir[,female_int:=births_int/(sexrat+1)]
  bir[,male_int:=births_int-female_int]
  bir[,c("births","births_int"):=NULL]
  bir <- melt(bir,id.vars=c("location_name","draw"))
  bir[grepl("male",variable),sex:="male"]
  bir[grepl("female",variable),sex:="female"]
  bir[,int:="pop"]
  bir[grepl("_int",variable),int:="pop_int"]
  bir[,variable:=NULL]
  bir <- dcast.data.table(bir,location_name+draw+sex~int,value.var="value")
  bir[,age:=0]
  bir[,year:=base_year+i]
  
  nextpop <- copy(pop[year==base_year+i-1])
  nextpop[,pop:=pop-deaths[year==(base_year+i-1)]$deaths_total]
  nextpop[,pop_int:=pop_int-deaths[year==(base_year+i-1)]$deaths_total_int]
  nextpop[age!=95,age:=age+1]
  nextpop[,year:=year+1]
  nextpop <- rbind(nextpop,bir)
  nextpop <- nextpop[,list(pop=sum(pop),pop_int=sum(pop_int)),by=c("location_name","year","sex","age","draw")]
  nextpop <- nextpop[order(location_name,year,sex,age,draw)]
  
  pop <- rbind(pop,nextpop)
  
  if (any(pop$pop < 0) | any(pop$pop_int < 0)) stop("issue")
  
  
} ## end of loop by year


dis <- dis[["Rheumatic heart disease"]]

## need to clear more memory for things to run depending on specification of draws, locations, etc.
rm(hist_sev); rm(epi); rm(bir); rm(case_uncert); rm(cdmort); rm(cfrac); rm(coverage_draws); rm(implied_hf_inc); rm(gbd)
rm(fertscale); rm(mortscale); rm(nextpop); rm(hist_arf); rm(tmpsims); rm(postsurg_start); rm(cov); rm(dem); rm(eff); rm(params)
rm(simlist); rm(sims); rm(phar_arf); rm(tmp); rm(pop)
gc()


dis[,c("deaths_cause","deaths_cause_int"):=NULL] ## merging on
dis <- merge(dis,deaths,by=c("location_name","year","sex","age","draw"),all.x=T)
rm(deaths); gc()

## if running on cluster, we won't collapse to create AU, but we'll keep that functionality if running test runs locally just in case
if (length(unique(dis$location_name)) > 1) {
  add <- copy(dis[,lapply(.SD,sum,na.rm=T),by=c("sex","age","year","draw"),.SDcols=c(names(dis)[!names(dis) %in% c("location_name","sex","age","year","draw")])])
  add[,location_name:="African Union"]
  dis <- rbind(dis,add)
  rm(add); gc()
}


## sum across 
dis[,total_10yr_history:=rowSums(.SD,na.rm=T),.SDcols = c(names(dis)[grepl("hist_arf",names(dis)) & !grepl("sp",names(dis)) & !grepl("int",names(dis))])]
dis[,total_10yr_history_sp:=rowSums(.SD,na.rm=T),.SDcols = c(names(dis)[grepl("hist_arf",names(dis)) & grepl("sp",names(dis)) & !grepl("int",names(dis))])]
dis[,total_10yr_history_int:=rowSums(.SD,na.rm=T),.SDcols = c(names(dis)[grepl("hist_arf",names(dis)) & !grepl("sp",names(dis)) & grepl("int",names(dis))])]
dis[,total_10yr_history_sp_int:=rowSums(.SD,na.rm=T),.SDcols = c(names(dis)[grepl("hist_arf",names(dis)) & grepl("sp",names(dis)) & grepl("int",names(dis))])]
dis[,count_inc_arf:=count_first_arf+count_recur_arf]
dis[,count_inc_arf_int:=count_first_arf_int+count_recur_arf_int]

## if running on cluster, save draws here so that we can aggregate AU from regions
if (specs$runloc=="regs" & Sys.info()[1] != 'Windows'){
  saveRDS(dis,paste0(outdirtmp,"/results/model_run_results/",run_num,"/model_results_sims",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".rds"))
}

## need ARF numbers for costing for (1) all people on secondary prophylaxis (calculated above)
## new cases (count_inc_arf)
## and new cases on treatment (need to create this somehow from the hist_arf variable in the year of the incident case) (count_inc_arf_totalcare)
dis[,c("count_inc_arf_totalcare","count_inc_arf_totalcare_int"):=0]
for (i in base_year:(base_year+proj_yrs-1)) {
  dis[year==i,count_inc_arf_totalcare:=dis[[paste0("hist_arf_",i,"_sp")]][dis$year==i]]
  dis[year==i,count_inc_arf_totalcare_int:=dis[[paste0("hist_arf_",i,"_sp_int")]][dis$year==i]]
}
dis[is.na(phar_seen),phar_seen:=0]
dis[is.na(phar_seen_int),phar_seen_int:=0]
dis[is.na(phar_treated),phar_treated:=0]
dis[is.na(phar_treated_int),phar_treated_int:=0]
dis[is.na(count_inc_arf),count_inc_arf:=0]
dis[is.na(count_inc_arf_int),count_inc_arf_int:=0]


## output numbers needed for costing--pharyngitis and ARF numbers, also for monetization
## and patient-years with heart failure, population, number of surgeries, patients post-surgery
saveRDS(dis[,c("location_name","year","sex","age","draw","pop","pop_int","total_10yr_history_sp","total_10yr_history_sp_int","count_inc_arf","count_inc_arf_int",
               "count_inc_arf_totalcare","count_inc_arf_totalcare_int",
               "count_first_arf","count_first_arf_int",
               "arf_death","arf_death_int","phar_seen","phar_seen_int",
               "phar_treated","phar_treated_int",
               "sev_prev_pys","sev_prev_pys_int",
               "postsurg_pys","postsurg_pys_int","num_surgs","num_surgs_int",
               "arf_death_first","arf_death_first_int","arf_death_recur","arf_death_recur_int"),with=F],
        paste0(outdirtmp,"/results/model_run_results/",run_num,"/results_for_costing",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".rds"))
gc()


## make dataset smaller--remove hist_arf variables
## save ARF variables that we may want to later plot
dis <- dis[,names(dis)[!grepl("hist_arf",names(dis))],with=F]
dis[,paste0("sev_prev",c(1:5)):=NULL]
dis[,c("sev_prev","sev_prev_int"):=NULL]
dis[,paste0("sev_prev_int",c(1:5)):=NULL]
dis[,c("mild_prev","mild_prev_int","postsurg","postsurg_int","tosevere","tosevere_int","regress","regress_int","sev_prev_start","sev_prev_start_int",
       "count_first_arf","count_first_arf_int","count_recur_arf","count_recur_arf_int"):=NULL]
dis[,c("pct_surg","pct_surg_int","staywell","staywell_int"):=NULL]
gc()


saveRDS(dis[,c("location_name","year","sex","age","draw","deaths_cause","deaths_cause_int","pop","pop_int","deaths_total","deaths_total_int",
               "arf_death_first","arf_death_recur","arf_death_first_int","arf_death_recur_int"),with=F],
        paste0(outdirtmp,"/results/model_run_results/",run_num,"/deaths",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".rds"))

dis[,c("arf_death_first","arf_death_first_int","arf_death_recur",
       "arf_death_recur_int"):=NULL]


## create aggregate cases
dis[,total_prev:=sev_prev_pys+mild_prev_pys+postsurg_pys]
dis[,total_prev_int:=sev_prev_pys_int+mild_prev_pys_int+postsurg_pys_int]
dis[,age:=as.character(age)]

add <- copy(dis[,lapply(.SD,sum,na.rm=T),by=c("location_name","year","age","draw"),.SDcols=names(dis)[!names(dis) %in% c("location_name","year","sex","draw","age")]])
add[,sex:="both"]
dis <- rbind(dis,add)
rm(add); gc()


## age-standardize based on both-sex 2017 population in the AU
as <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_pop.RDS"))
as <- as[location_name=="African Union" & year == 2017]
as <- as[,list(pop=sum(pop)),by=c("location_name","year","age")]
as[,pct_pop:=pop/sum(pop)]
as[,age:=as.numeric(age)]


dis[,age:=as.numeric(age)]
dis <- merge(dis,as[,c("age","pct_pop"),with=F],by=c("age"),all.y=T)
dis <- melt(dis,id.vars=c("location_name","year","sex","age","draw","pop","pop_int","pct_pop"),value.name="count",variable.name="var")
dis[,int:=F]
dis[grepl("int",var),int:=T]
dis[int==T,pop:=pop_int]
dis[,pop_int:=NULL]
dis[,rate:=count/pop]
gc()
dis[,rate:=rate*pct_pop]
add2 <- copy(dis[,list(rate=sum(rate)),by=c("location_name","year","sex","draw","int","var")])
add2[,pop:=NA]
add2[,count:=NA]
add2[,age:="Age-standardized"]
dis[,rate:=count/pop]
dis[,pct_pop:=NULL]
dis <- rbind(dis,add2)
rm(add2); rm(as); gc()

add <- copy(dis[age != "Age-standardized",list(pop=sum(pop),count=sum(count)),by=c("location_name","year","sex","draw","int","var")])
add[,rate:=count/pop]
add[,age:="All Ages"]
dis <- rbind(dis,add)
rm(add); gc()
dis[,year:=as.numeric(year)]
dis[,var:=gsub("_int","",var)]

## there should be NAs for count for age-standardized
## check that there aren't in any others before we allow it to run with na.rm=T
if (any(is.na(dis[age!="Age-standardized"]$count))) stop("NA values")
if (any(is.na(dis[age!="Age-standardized"]$pop))) stop("NA values")



## % reductions in age-standardized rates of RHD deaths, ARF deaths, and total RHD prevalence
outsum <- copy(dis[age=="Age-standardized" & sex == "both" & year == (base_year+proj_yrs-1) & var %in% c("deaths_cause","arf_death","total_prev","incnum")])
outsum <- dcast.data.table(outsum,location_name+var+draw~int,value.var="rate")
outsum[,reduct_as_rate:=`FALSE`-`TRUE`]
outsum[,pct_reduct_as_rate:=(`FALSE`-`TRUE`)/`FALSE`*100]

## total deaths averted, ARF deaths averted, RHD cases averted
outsum2 <- copy(dis[age=="All Ages" & sex == "both" & year %in% c(2021:(base_year+proj_yrs-1)) & var %in% c("deaths_cause","arf_death","incnum")])
add <- copy(outsum2[,list(count=sum(count)),by=c("location_name","sex","age","draw","int","var")])
add[,year:="All"]
add[,c("pop","rate"):=NA]
outsum2 <- rbind(outsum2,add)
outsum2 <- dcast.data.table(outsum2,location_name+var+year+draw~int,value.var="count")
outsum2[,death_reduction:=`FALSE`-`TRUE`]
outsum2[,pct_death_reduction:=(`FALSE`-`TRUE`)/`FALSE`*100]

saveRDS(outsum,paste0(outdirtmp,"/results/model_run_results/",run_num,"/pct_asrate_reduct_sims",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".rds"))
saveRDS(outsum2,paste0(outdirtmp,"/results/model_run_results/",run_num,"/counts_averted_sims",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".rds"))

rm(outsum)
rm(outsum2)
gc()

res <- copy(dis[,list(count_mean=mean(count,na.rm=T),count_lower=quantile(count,probs=c(.025),na.rm=T),count_upper=quantile(count,probs=c(.975),na.rm=T),
                      rate_mean=mean(rate),rate_lower=quantile(rate,probs=c(.025)),rate_upper=quantile(rate,probs=c(.975)),
                      pop_mean=mean(pop,na.rm=T),pop_lower=quantile(pop,probs=c(.025),na.rm=T),pop_upper=quantile(pop,probs=c(.975),na.rm=T)),by=c("location_name","year","sex","age","int","var")])
res <- melt(res,id.vars=c("location_name","year","sex","age","int","var","pop_mean","pop_lower","pop_upper"))
res[,uncert:=str_split_fixed(variable,"_",Inf)[,2]]
res[,metric:=str_split_fixed(variable,"_",Inf)[,1]]
res[,variable:=NULL]


## save res for easy results
saveRDS(res,paste0(outdirtmp,"/results/model_run_results/",run_num,"/full_results",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".rds"))
#res <- readRDS(paste0("/model_run_results/",run_num,"/full_results.rds"))
gc()

res[var %in% c("arf_death","deaths_cause") & sex=="both" & int==F & age == "All Ages" & metric=="count" & year == 2017]


ratediff <- copy(dis[sex=="both" & age %in% c("Age-standardized","All Ages") & var %in% c("total_prev","deaths_cause","incnum"),c("location_name","year","sex","age","draw","int","var","rate"),with=F])
ratediff <- dcast.data.table(ratediff,location_name+year+sex+age+draw+var~int,value.var="rate")
ratediff[,diff:=`TRUE`-`FALSE`]
ratediff[,pct_diff:=(`TRUE`-`FALSE`)/`FALSE`*100]
ratediff <- ratediff[,list(rate_diff_mean=mean(diff),rate_diff_lower=quantile(diff,probs=c(.025)),rate_diff_upper=quantile(diff,probs=c(.975)),
                           rate_pct_diff_mean=mean(pct_diff),rate_pct_diff_lower=quantile(pct_diff,probs=c(.025)),rate_pct_diff_upper=quantile(pct_diff,probs=c(.975))),by=c("location_name","year","sex","age","var")]

saveRDS(ratediff,paste0(outdirtmp,"/results/model_run_results/",run_num,"/ratediff",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".rds"))


test <- copy(res[sex=="both" & year==2030 & age=="Age-standardized" & var=="deaths_cause" & metric=="rate"])
test

test <- copy(res[sex=="both" & year==2030 & age=="All Ages" & var=="total_prev" & metric=="count"])
test

test <- copy(res[sex=="both" & year==2030 & age=="Age-standardized" & var=="deaths_cause" & metric=="rate"])
test


##################################
## Summary plots
##################################
totplot <- copy(res[age %in% c("All Ages","Age-standardized") & var %in% c("mild_prev_pys","sev_prev_pys","postsurg_pys","deaths_cause") & sex %in% c("both") & metric %in% c("count","rate")])
totplot[,var:=factor(var,levels=c("mild_prev_pys","sev_prev_pys","postsurg_pys","deaths_cause"),labels=c("Mild RHD","RHD with HF","Post-surgical RHD","Deaths from RHD"))]
#totplot[,sex:=factor(sex,levels=c("female","male"),labels=c("Female","Male"))]
totplot[,int:=factor(int,levels=c("FALSE","TRUE"),labels=c("No Scale-up of Interventions","Scale-up of Interventions"))]
totplot <- dcast.data.table(totplot,location_name+year+sex+age+int+var+metric~uncert,value.var=c("value"))


pdf(paste0(outdir,"/results/model_run_results/",run_num,"/summary_results_diagnostics_uncert",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".pdf"),width=12,height=8)

for (i in unique(totplot$location_name)) {
  ## make plots of rates of things over time
  gg <- ggplot(data=totplot[age=="All Ages" & metric=="rate" & location_name==i],aes(x=year,y=mean*100000,group=int,color=int,linetype=int)) + 
    geom_line() + theme_bw() + facet_wrap(~var,scales="free") + ggtitle(paste0(i," Crude Rates of Parameters, per 100,000")) + expand_limits(y = 0) +
    ylab("Rate") + scale_x_continuous("Year",breaks=c(2017,2020,2025,2030,2035,2040)) + theme(legend.position="bottom") + 
    geom_ribbon(aes(ymin=lower*100000, ymax=upper*100000,fill=int), alpha=0.1,colour = NA)
  
  print(gg)
  
  gg <- ggplot(data=totplot[age=="Age-standardized" & metric == "rate" & location_name==i],aes(x=year,y=mean*100000,group=int,color=int,linetype=int)) + 
    geom_line() + theme_bw() + facet_wrap(~var,scales="free") + ggtitle(paste0(i," Age-standardized Rates of Parameters, per 100,000")) + expand_limits(y = 0) +
    ylab("Rate") + scale_x_continuous("Year",breaks=c(2017,2020,2025,2030,2035,2040)) + theme(legend.position="bottom") + 
    geom_ribbon(aes(ymin=lower*100000, ymax=upper*100000,fill=int), alpha=0.1,colour = NA)
  
  print(gg)
  
  
  ## make plots of counts of things over time
  gg <- ggplot(data=totplot[age=="All Ages" & metric == "count" & location_name==i],aes(x=year,y=mean/1000,group=int,color=int,linetype=int)) + 
    geom_line() + theme_bw() + facet_wrap(~var,scales="free") + ggtitle(paste0(i," Counts of Parameters, thousands")) + expand_limits(y = 0) +
    ylab("Rate") + scale_x_continuous("Year",breaks=c(2017,2020,2025,2030,2035,2040)) + theme(legend.position="bottom") + 
    geom_ribbon(aes(ymin=lower/1000, ymax=upper/1000,fill=int), alpha=0.1,colour = NA)
  
  print(gg)
}

dev.off()



totplot2 <- copy(res[age %in% c("All Ages","Age-standardized") & var %in% c("total_prev","deaths_cause","incnum") & sex %in% c("both") & metric %in% c("count","rate")])
totplot2[,var:=factor(var,levels=c("incnum","total_prev","deaths_cause"),labels=c("Incident RHD Cases","Total RHD Cases","Deaths from RHD"))]
totplot2[,int:=factor(int,levels=c("FALSE","TRUE"),labels=c("No Scale-up of Interventions","Scale-up of Interventions"))]
totplot2 <- dcast.data.table(totplot2,location_name+year+sex+age+int+var+metric~uncert,value.var=c("value"))

ratediff[,var:=factor(var,levels=c("incnum","total_prev","deaths_cause"),labels=c("Incident RHD Cases","Total RHD Cases","Deaths from RHD"))]


pdf(paste0(outdir,"/results/model_run_results/",run_num,"/summary_results_uncert",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".pdf"),width=8,height=4)

for (i in unique(totplot$location_name)) {
  
  ## make plots of rates of things over time
  gg <- ggplot(data=totplot2[age=="All Ages" & metric == "rate" & location_name==i],aes(x=year,y=mean*100000,group=int,color=int,linetype=int)) + 
    geom_line() + theme_bw() + facet_wrap(~var,scales="free") + #ggtitle("Crude Rates of Parameters, per 100,000") + 
    expand_limits(y = 0) + scale_color_discrete("") + scale_fill_discrete("") + scale_linetype_discrete("") +
    ylab("Crude Rate per 100,000") + scale_x_continuous("Year",breaks=c(2020,2025,2030,2035,2040)) + theme(legend.position="bottom") + 
    geom_ribbon(aes(ymin=lower*100000, ymax=upper*100000,fill=int), alpha=0.1,colour = NA) + ggtitle(paste0(i))
  
  print(gg)
  
  gg1 <- ggplot(data=totplot2[age=="Age-standardized" & metric == "rate" & location_name==i],aes(x=year,y=mean*100000,group=int,color=int,linetype=int)) + 
    geom_line() + theme_bw() + facet_wrap(~var,scales="free") + #ggtitle("Age-standardized Rates of Parameters, per 100,000") + 
    expand_limits(y = 0) + scale_color_discrete("") + scale_fill_discrete("") + scale_linetype_discrete("") +
    ylab("Age-Standardized Rate per 100,000") + scale_x_continuous("Year",breaks=c(2020,2025,2030,2035,2040)) + theme(legend.position="bottom") + 
    geom_ribbon(aes(ymin=lower*100000, ymax=upper*100000,fill=int), alpha=0.1,colour = NA) + ggtitle(paste0(i))
  
  print(gg1)
  
  
  ## make plots of counts of things over time
  gg <- ggplot(data=totplot2[age=="All Ages" & metric == "count" & location_name==i],aes(x=year,y=mean/1000,group=int,color=int,linetype=int)) + 
    geom_line() + theme_bw() + facet_wrap(~var,scales="free") + #ggtitle("Counts of Parameters, thousands") + 
    expand_limits(y = 0) + scale_color_discrete("") + scale_fill_discrete("") + scale_linetype_discrete("") +
    ylab("Number (thousands)") + scale_x_continuous("Year",breaks=c(2020,2025,2030,2035,2040)) + theme(legend.position="bottom") + 
    geom_ribbon(aes(ymin=lower/1000, ymax=upper/1000,fill=int), alpha=0.1,colour = NA) + scale_y_continuous(labels = comma) + ggtitle(paste0(i))
  
  print(gg)

}


dev.off()


pdf(paste0(outdir,"/results/model_run_results/",run_num,"/summary_results_uncert_decline",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".pdf"),width=8,height=6)

for (i in unique(totplot$location_name)) {
  
  plot.new()
  text(x=.5, y=.5, i)  

  
  gg1 <- ggplot(data=totplot2[age=="Age-standardized" & metric == "rate" & year >=2020 & location_name==i],aes(x=year,y=mean*100000,group=int,color=int,linetype=int)) + 
    geom_line() + theme_bw() + facet_wrap(~var,scales="free") + #ggtitle("Age-standardized Rates of Parameters, per 100,000") + 
    expand_limits(y = 0) + scale_color_discrete("") + scale_fill_discrete("") + scale_linetype_discrete("") +
    ylab("Age-Standardized Rate per 100,000") + scale_x_continuous("Year",breaks=c(2020,2025,2030,2035,2040)) + theme(legend.position="bottom") + 
    geom_ribbon(aes(ymin=lower*100000, ymax=upper*100000,fill=int), alpha=0.1,colour = NA)
  
  print(gg1)
  
  gg <- ggplot(data=ratediff[age=="Age-standardized" & year >=2020 & location_name==i],aes(x=year,y=rate_pct_diff_mean)) + 
    geom_line() + theme_bw() + facet_wrap(~var,scales="fixed") + #ggtitle("Age-standardized Rates of Parameters, per 100,000") + 
    expand_limits(y = 0) + scale_color_discrete("") + scale_fill_discrete("") + scale_linetype_discrete("") +
    ylab("Percent Difference in Rate") + scale_x_continuous("Year",breaks=c(2020,2025,2030,2035,2040)) + theme(legend.position="bottom") + 
    geom_ribbon(aes(ymin=rate_pct_diff_lower, ymax=rate_pct_diff_upper), alpha=0.1,colour = NA)
  print(gg)
  
  lay <- rbind(c(1,1,1),
               c(1,1,1),
               c(1,1,1),
               c(1,1,1),
               c(1,1,1),
               c(2,2,2),
               c(2,2,2),
               c(2,2,2),
               c(2,2,2))
  
  grid.arrange(gg1,gg,layout_matrix=lay)
}
  
dev.off()

## look at breakdown of deaths from different groups
totplot <- copy(res[age %in% c("All Ages","Age-standardized") & var %in% c("deaths_cause","deaths_severe_rhd","deaths_postsurg_rhd","deaths_op"
) & sex %in% c("both") & metric %in% c("rate","count")])
totplot[,var:=factor(var,levels=c("deaths_cause","deaths_severe_rhd","deaths_postsurg_rhd","deaths_op"),
                     labels=c("All RHD Deaths","RHD HF Deaths","RHD Deaths Post-surgical","RHD Deaths Surgical"))]
totplot[,int:=factor(int,levels=c("FALSE","TRUE"),labels=c("No Scale-up of Interventions","Scale-up of Interventions"))]
totplot <- dcast.data.table(totplot,location_name+year+sex+age+int+var+metric~uncert,value.var=c("value"))

pdf(paste0(outdir,"/results/model_run_results/",run_num,"/death_breakdown_diagnostics_uncert",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".pdf"),width=12,height=8)

for (i in unique(totplot$location_name)) {
  
  ## make plots of rates of things over time
  gg <- ggplot(data=totplot[age=="All Ages" & metric == "rate" & location_name==i],aes(x=year,y=mean*100000,group=int,color=int,linetype=int)) + 
    geom_line() + theme_bw() + facet_wrap(~var) + ggtitle(paste0(i," Crude Rates of Parameters, per 100,000")) + expand_limits(y = 0) +
    ylab("Rate") + scale_x_continuous("Year",breaks=c(2020,2025,2030,2035,2040)) + theme(legend.position="bottom") + 
    geom_ribbon(aes(ymin=lower*100000, ymax=upper*100000,fill=int), alpha=0.1,colour = NA)
  
  print(gg)
  
  
  ## make plots of rates of things over time
  gg <- ggplot(data=totplot[age=="All Ages" & metric == "rate" & location_name==i],aes(x=year,y=mean*100000,group=int,color=int,linetype=int)) + 
    geom_line() + theme_bw() + facet_wrap(~var,scales="free") + ggtitle(paste0(i," Crude Rates of Parameters, per 100,000")) + expand_limits(y = 0) +
    ylab("Rate") + scale_x_continuous("Year",breaks=c(2020,2025,2030,2035,2040)) + theme(legend.position="bottom") + 
    geom_ribbon(aes(ymin=lower*100000, ymax=upper*100000,fill=int), alpha=0.1,colour = NA)
  
  print(gg)
  
  
  gg <- ggplot(data=totplot[age=="Age-standardized" & metric == "rate" & location_name==i],aes(x=year,y=mean*100000,group=int,color=int,linetype=int)) + 
    geom_line() + theme_bw() + facet_wrap(~var,scales="free") + ggtitle(paste0(i," Age-standardized Rates of Parameters, per 100,000")) + expand_limits(y = 0) +
    ylab("Rate") + scale_x_continuous("Year",breaks=c(2020,2025,2030,2035,2040)) + theme(legend.position="bottom") + 
    geom_ribbon(aes(ymin=lower*100000, ymax=upper*100000,fill=int), alpha=0.1,colour = NA)
  
  print(gg)
  
  
  ## make plots of counts of things over time
  gg <- ggplot(data=totplot[age=="All Ages" & metric == "count" & location_name==i],aes(x=year,y=mean,group=int,color=int,linetype=int)) + 
    geom_line() + theme_bw() + facet_wrap(~var,scales="free") + ggtitle(paste0(i," Counts of Parameters, thousands")) + expand_limits(y = 0) +
    ylab("Count") + scale_x_continuous("Year",breaks=c(2020,2025,2030,2035,2040)) + theme(legend.position="bottom") + 
    geom_ribbon(aes(ymin=lower, ymax=upper,fill=int), alpha=0.1,colour = NA)
  
  
  print(gg)

}

dev.off()



toplot <- copy(res[var=="sev_prev_pys"])
setnames(toplot,c("pop_mean","pop_lower","pop_upper"),c("mean","lower","upper"))
toplot <- toplot[age %in% c(0:99)]
toplot[,age:=as.numeric(age)]


pdf(paste0(outdir,"/results/model_run_results/",run_num,"/pop_breakdown_age_year",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".pdf"),width=12,height=8)

for (i in unique(totplot$location_name)) {
  
  
  gg <- ggplot(data=toplot[sex=="male" & location_name==i],aes(x=age,y=mean/1000,group=int,color=int,linetype=int)) + 
    geom_line() + theme_bw() + facet_wrap(~year) +  ggtitle(paste0(i," Population, Thousands")) + expand_limits(y = 0) +
    ylab("Rate") + scale_x_continuous("Age") + theme(legend.position="bottom") + 
    geom_ribbon(aes(ymin=lower/1000, ymax=upper/1000,fill=int), alpha=0.1,colour = NA)
  
  print(gg)
  
  
  gg <- ggplot(data=toplot[sex=="female" & location_name==i],aes(x=age,y=mean/1000,group=int,color=int,linetype=int)) + 
    geom_line() + theme_bw() + facet_wrap(~year) +  ggtitle(paste0(i," Population, Thousands")) + expand_limits(y = 0) +
    ylab("Rate") + scale_x_continuous("Age") + theme(legend.position="bottom") + 
    geom_ribbon(aes(ymin=lower/1000, ymax=upper/1000,fill=int), alpha=0.1,colour = NA)
  
  print(gg)
  


}

dev.off()







