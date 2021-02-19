## RHD costing
## Matthew Coates



library(data.table)
library(openxlsx)
library(EnvStats)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(stringr)


## set directories
if (Sys.info()[1] == 'Windows') {
  rm(list=ls()) ## not clearing workspace on cluster so we can use commandArgs() hack
  codedir <- paste0("[Insert own directory here]/rhd_invest_priv/")
  outdir <- paste0("[Insert own directory here]/rhd_investment_case/")
  outdirtmp <- outdir
  drawnum <- 50
} else {
  codedir <- paste0("[Insert own directory here]/rhd_invest_priv/")
  outdir <- paste0("[Insert own directory here]/rhd_investment_case/")
  outdirtmp <- paste0("[Insert own directory here]")
  drawnum <- 1000
}
## load functions
source(paste0(codedir,"/functions/swap_location_names.R"))
source(paste0(codedir,"/functions/map_ids_names.R"))
source(paste0(codedir,"/model_code/costing_functions.R"))

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


## params
int_start_year <- 2021
base_year <- 2017
total_rounds <- specs$end_year-base_year+1
scale_to <- specs$cov_scale_yr


###########################

d2 <- readRDS(paste0(outdirtmp,"/results/model_run_results/",run_num,"/results_for_costing",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".rds"))
setnames(d2,c("total_10yr_history_sp","total_10yr_history_sp_int"),c("arf_cases_on_sp","arf_cases_on_sp_int"))
d2[,age:=as.numeric(as.character(age))]
d3 <- copy(d2[,list(arf_cases_on_sp=sum(arf_cases_on_sp,na.rm=T),arf_cases_on_sp_int=sum(arf_cases_on_sp_int,na.rm=T),
                           count_inc_arf=sum(count_inc_arf,na.rm=T),count_inc_arf_int=sum(count_inc_arf_int,na.rm=T),count_inc_arf_totalcare=sum(count_inc_arf_totalcare,na.rm=T),
                           count_inc_arf_totalcare_int=sum(count_inc_arf_totalcare_int,na.rm=T),
                           count_first_arf=sum(count_first_arf,na.rm=T),count_first_arf_int=sum(count_first_arf_int,na.rm=T),
                           arf_death=sum(arf_death,na.rm=T),arf_death_int=sum(arf_death_int,na.rm=T),
                           phar_seen=sum(phar_seen,na.rm=T),phar_seen_int=sum(phar_seen_int,na.rm=T),
                           phar_treated=sum(phar_treated,na.rm=T),phar_treated_int=sum(phar_treated_int,na.rm=T),pop=sum(pop,na.rm=T),pop_int=sum(pop_int,na.rm=T),
                           sev_prev_pys=sum(sev_prev_pys,na.rm=T),sev_prev_pys_int=sum(sev_prev_pys_int,na.rm=T),
                           num_surgs=sum(num_surgs,na.rm=T),num_surgs_int=sum(num_surgs_int,na.rm=T),
                           postsurg_pys=sum(postsurg_pys,na.rm=T),postsurg_pys_int=sum(postsurg_pys_int,na.rm=T)),by=c("location_name","year","draw")])
d3[,sex:="both"]
## CHW assessment (phar_seen) plus an adherence support visit for those actually treated
d3[,total_visits:=phar_seen+phar_treated]
d3[,total_visits_int:=phar_seen_int+phar_treated_int]

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

## if the starting coverage is the same as the ending coverage input, make the draws not change
cov[abs(coverage_start_au-coverage_end_au)<.00001,ending_coverage:=starting_coverage]

## for scenarios with 100% coverage of an intervention in the AU, each country needs to be 100%
cov[coverage_end_au==1,ending_coverage:=1]

if (any(cov$ending_coverage < 0 | cov$ending_coverage > 1)) stop("bad end coverage")

if (nrow(cov[starting_coverage > ending_coverage]) > 0) stop("coverage end < coverage_start")

cov <- cov[location_name %in% c(locs)]

cov <- rbindlist(lapply(base_year:(base_year+total_rounds-1),FUN=function(x){
  ret <- copy(cov)
  ret[,year:=x]
  return(ret)
}))


total_scaleup_rounds <- (scale_to-base_year+1) - (int_start_year-base_year)
cov[,round:=year-base_year+1]
cov[round > (int_start_year-base_year),coverage_level:=(ending_coverage-starting_coverage)/(total_scaleup_rounds)*(round-(int_start_year-base_year))+starting_coverage]
cov[round > (scale_to-base_year+1),coverage_level:=ending_coverage]
cov[round <= (int_start_year-base_year),coverage_level:=starting_coverage]

## did this in nonlinear way--numbers from spreadsheet
## should be consistent with the health impact scale-up as well
hf_facility_scale <- c(0.1,0.1,0.1,0.1,0.19,0.28,0.37,0.46,0.55,0.64,0.73,0.82,0.91,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,rep(1,45))
hf_cov_scale <- c(0.08,0.08,0.08,0.08,0.089,0.107,0.134,0.17,0.215,0.269,0.332,0.404,0.476,0.548,0.611,0.665,0.71,0.746,0.773,0.791,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,rep(0.8,45))
hf_yr <- c(2017:2090)
hf_facility_scale_dt <- data.table(data.frame(hf_facility_scale=hf_facility_scale,hf_cov_scale=hf_cov_scale,year=hf_yr,stringsAsFactors=F))
hf_facility_scale_dt[,pct_progress_cov:=(hf_cov_scale-.08)/(.8-.08)]
hf_facility_scale_dt[,pct_progress_fac:=(hf_facility_scale-.1)/(1-.1)]
cov <- merge(cov,hf_facility_scale_dt,by="year",all.x=T)

cov[intervention_id == 4,coverage_level:=(ending_coverage-starting_coverage)*pct_progress_cov+starting_coverage]
cov[round > (scale_to-base_year+1),coverage_level:=ending_coverage]

## added the option to get rid of the scale-up to see long-run effects of scale-up for a certain period
if (specs$end_scaleup != "No") cov[round+base_year-1 >= as.numeric(specs$end_scaleup),coverage_level:=starting_coverage]

cov[,coverage_increase:=coverage_level-starting_coverage]


# Intervention 1:
# # of pharyngitis cases seen (function of some baseline frequency, increase in # presenting to facility)
# population ages 5-24
c1 <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/other_inputs/intervention_costs.xlsx"),
                                     sheet=1,startRow=3))
c1 <- c1[!is.na(Currency),c(1:10),with=F]
setnames(c1,c("item","toggle","mean","lower","upper","currency","currency_year","cost_type","shared_intervention","country_costed"))

## not using Cuba study for costing anymore
c1 <- c1[!country_costed=="Cuba"]
## use amox rather than penicillin (since oral antibiotic scenario meant to show cheapest options)
c1 <- c1[!item %in% c("Oral penicillin (VK 250 mg tab, 2x per day x 10 days)")]
## use costing from cameroon awareness campaign 
c1 <- c1[!country_costed=="Burkina Faso"]

## drop ones where toggle != 1 so we can do sensitivity analyses
## use settings to toggle off
if (specs$gas_transport_cost=="No") c1[item=="Patient transport cost to HC",toggle:=0]
if (specs$community_based_gas=="Yes") {
  c1[item=="Patient transport cost to HC",toggle:=0]
  c1[item=="1 vial IM penicillin (1.2M IU)",toggle:=0]
  c1[item=="Health center outpatient visit",toggle:=0]
  c1[item=="Oral amoxicillin (500 mg tablet x 6 days)",toggle:=1]
  c1[item=="CHW time",toggle:=1]
  c1[item=="CHW training",toggle:=1]
  c1[item=="CHW gear (phone, backpack, etc.)",toggle:=1]
  c1[item=="CHW administrative costs (manager)",toggle:=1]
  
  c1[item=="Ongoing provider education and mentorship",toggle:=0]
  
}

c1 <- c1[toggle==1]

## convert any that are every X year to annual
c1[grepl("per 5-24 pop",cost_type),var:="pop5-24"]
c1[grepl("per pop over age 15",cost_type),var:="pop15+"]
c1[grepl("per case",cost_type),var:="phar_seen"]
c1[cost_type=="per pop",var:="pop"]
c1[cost_type=="per pop per 3 years",var:="pop"]
c1[cost_type=="per treated case",var:="phar_treated"]
## the per CHW ones need to factor in 1 visit for phar_seen and 2 visits for phar_treated--can separately cost initial visit and follow-up visit
## (260=5 days/week, 52 weeks/yr)
c1[cost_type=="per CHW",mean:=mean/260/specs$chw_patients_perday]
c1[cost_type=="per CHW",lower:=lower/260/specs$chw_patients_perday]
c1[cost_type=="per CHW",upper:=upper/260/specs$chw_patients_perday]
c1[cost_type=="per CHW",var:="total_visits"] ## total visits is adding up phar seen and phar treated (initial assessment + follow-up for adherence support if treating)
c1[cost_type=="per 50 CHW",mean:=mean/260/specs$chw_patients_perday/50]
c1[cost_type=="per 50 CHW",lower:=lower/260/specs$chw_patients_perday/50]
c1[cost_type=="per 50 CHW",upper:=upper/260/specs$chw_patients_perday/50]
c1[cost_type=="per 50 CHW",var:="total_visits"]
c1[cost_type=="per 30 CHW",mean:=mean/260/specs$chw_patients_perday/30]
c1[cost_type=="per 30 CHW",lower:=lower/260/specs$chw_patients_perday/30]
c1[cost_type=="per 30 CHW",upper:=upper/260/specs$chw_patients_perday/30]
c1[cost_type=="per 30 CHW",var:="total_visits"]
c1[grepl("3 years", cost_type),mean:=mean/3]
c1[grepl("3 years", cost_type),lower:=lower/3]
c1[grepl("3 years", cost_type),upper:=upper/3]


if (any(is.na(c1$var))) stop("missing denom")


## convert costs to 2019 USD
c1 <- convert_costs(c1,locs)


## make draws
set.seed(246825)
## make draws for AU, then use ratios to extend to countries or regions
## we want the uncertainty defined by the range for the AU overall--if we draw independently, this will shrink substantially
## so take draws for AU and adjust for countries (alternative is make higher uncertainty for countries, but don't have much to base relative uncertainties on)
c1draws <- cbind(copy(c1[location_name=="African Union",c("item","location_name","mean"),with=F]),matrix(rtri(drawnum*nrow(c1[location_name=="African Union"]),
                                                                                                              mode=c1[location_name=="African Union"]$mean,
                           min=c1[location_name=="African Union"]$lower,max=c1[location_name=="African Union"]$upper),nrow=nrow(c1[location_name=="African Union"])))
setnames(c1draws,"mean","au_mean")
c1 <- merge(c1,c1draws[,names(c1draws)[!names(c1draws) %in% c("location_name")],with=F],by=c("item"),all.x=T)
c1[,rat:=mean/au_mean]
for (i in c(paste0("V",1:drawnum))) {
  c1[,c(paste0(i)):=c1[[paste0(i)]]*rat]
}
c1[,c("au_mean","rat"):=NULL]
rm(c1draws); gc()

c1 <- melt(c1,id.vars=c(names(c1)[!names(c1) %in% paste0("V",c(1:drawnum))]),variable.name="draw",value.name="unit_cost")
c1[,draw:=as.numeric(gsub("V","",draw))]
c1 <- c1[location_name %in% locs]


## get variables we need to merge on
c1p <- copy(d3[,c("location_name","year","sex","draw","pop","pop_int")])
c1p <- melt(c1p,id.vars=c("location_name","year","sex","draw"),value.nam="mult",variable.name="var")
c1p[,int:=F]
c1p[grepl("_int",var),int:=T]
c1p[,var:="pop"]

add <- copy(d3[,c("location_name","year","sex","draw","phar_treated","phar_treated_int","phar_seen","phar_seen_int","total_visits","total_visits_int"),with=F])
add <- melt(add,id.vars=c("location_name","year","sex","draw"),variable.name="var",value.nam="mult")
add[,int:=F]
add[grepl("_int",var),int:=T]
add[,var:=gsub("_int","",var)]

c1p <- rbind(c1p,add)

add <- copy(d2[age >=15,c("location_name","year","sex","draw","pop","pop_int"),with=F])
add <- add[,list(pop=sum(pop,na.rm=T),pop_int=sum(pop_int,na.rm=T)),by=c("location_name","year","draw")]
add <- melt(add,id.vars=c("location_name","year","draw"),value.nam="mult",variable.name="var")
add[,int:=F]
add[grepl("_int",var),int:=T]
add[,var:='pop15+']
add[,sex:="both"]

c1p <- rbind(c1p,add)

## to make merge safer replicate for years and intervention status
c1 <- rbindlist(lapply(base_year:(base_year+total_rounds-1),FUN=function(x){
  ret <- copy(c1)
  ret[,year:=x]
  ret[,int:=F]
  add <- copy(ret)
  add[,int:=T]
  ret <- rbind(ret,add)
  return(ret)
}))


## merge unit costs and populations to multiply by
c1 <- merge(c1,c1p,by=c("location_name","year","draw","var","int"),all.x=T)
if (any(is.na(c1$mult))) stop('missing')


## merge costs and coverage
c1 <- merge(c1,copy(cov[intervention_id == 1,c("location_name","coverage_level","starting_coverage","draw","year"),with=F]),by=c("location_name","year","draw"),all.x=T)

## the variable mult for several components already incorporates coverage (per case and per treated case)
## we just need to use coverage for those that aren't already incorporated
## we're going to assume mass media coverage of whole population-- unit cost was divided by 3 above to represent every 3 years
c1[var %in% c("total_visits","phar_treated","phar_seen"),c("coverage_level","starting_coverage"):=1]
c1[item == "Mass media awareness/education campaign",c("starting_coverage"):=0]
c1[item == "Mass media awareness/education campaign" & int == F,c("starting_coverage","coverage_level"):=0]
c1[int==T & year > 2020 & item == "Mass media awareness/education campaign" & abs(coverage_level-starting_coverage)>0.00001,coverage_level:=1] ## if there is a change in coverage, make it a full cost item
c1[year < 2021 & item == "Mass media awareness/education campaign",coverage_level:=0]

if (specs$end_scaleup != "No") c1[year >= as.numeric(specs$end_scaleup) & item == "Mass media awareness/education campaign",coverage_level:=0]


## cost with no intervention scale-up is unit cost*unit cost denom*baseline coverage
c1[int==F,cost:=unit_cost*mult*starting_coverage]
## cost with intervention scale-up is unit cost*unit cost denom*coverage level
c1[int==T,cost:=unit_cost*mult*coverage_level]




## Intervention 2:
## load cost info
c2 <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/other_inputs/intervention_costs.xlsx"),
                                     sheet=2,startRow=3))
c2 <- c2[!is.na(Currency),c(1:10),with=F]
setnames(c2,c("item","toggle","mean","lower","upper","currency","currency_year","cost_type","shared_intervention","country_costed"))

## only cost ones with toggle on
c2 <- c2[toggle==1]


## convert any that are every X year to annual
c2[grepl("per 3 years",cost_type),mean:=mean/3]
c2[grepl("per 3 years",cost_type),lower:=lower/3]
c2[grepl("per 3 years",cost_type),upper:=upper/3]
c2[grepl("per 5 years",cost_type),mean:=mean/5]
c2[grepl("per 5 years",cost_type),lower:=lower/5]
c2[grepl("per 5 years",cost_type),upper:=upper/5]
c2[grepl("per 300,000 people",cost_type),mean:=mean/300000]
c2[grepl("per 300,000 people",cost_type),lower:=lower/300000]
c2[grepl("per 300,000 people",cost_type),upper:=upper/300000]
c2[grepl("per 300,000 people",cost_type),var:="pop"]
c2[grepl("per 5-24 pop",cost_type),var:="pop5-24"]
c2[cost_type=="per pop",var:="pop"]
c2[grepl("initial cost",cost_type),mean:=mean/10] ## assumed need to re-up after 10 years
c2[grepl("initial cost",cost_type),lower:=lower/10]
c2[grepl("initial cost",cost_type),upper:=upper/10]
c2[cost_type=="per pop per 3 years",var:="pop"] ## 3 years part adjusted above
c2[grepl("per incident ARF",cost_type),var:="count_inc_arf_totalcare"]
c2[grepl("per incident ARF in person without ARF history",cost_type),var:="count_inc_arf_totalcare_first"]
c2[grepl("per month per case of ARF history",cost_type),var:="arf_cases_on_sp"] ## per month part adjusted below (x12)
c2[grepl("per pop over age 15",cost_type),var:="pop15+"]
if (any(is.na(c2$cost_type))) stop('missing cost type')

## prep to run through function that converts costs to 2020 USD
c2 <- convert_costs(c2,locs=locs)

## make draws
set.seed(24683825)
c2draws <- cbind(copy(c2[location_name=="African Union",c("item","location_name","mean"),with=F]),matrix(rtri(drawnum*nrow(c2[location_name=="African Union"]),
                                                                                                              mode=c2[location_name=="African Union"]$mean,
                                                                                                              min=c2[location_name=="African Union"]$lower,max=c2[location_name=="African Union"]$upper),nrow=nrow(c2[location_name=="African Union"])))
setnames(c2draws,"mean","au_mean")
c2 <- merge(c2,c2draws[,names(c2draws)[!names(c2draws) %in% c("location_name")],with=F],by=c("item"),all.x=T)
c2[,rat:=mean/au_mean]
for (i in c(paste0("V",1:drawnum))) {
  c2[,c(paste0(i)):=c2[[paste0(i)]]*rat]
}
c2[,c("au_mean","rat"):=NULL]
c2 <- melt(c2,id.vars=c(names(c2)[!names(c2) %in% paste0("V",c(1:drawnum))]),variable.name="draw",value.name="unit_cost")
c2[,draw:=as.numeric(gsub("V","",draw))]
rm(c2draws); gc()
c2 <- c2[location_name %in% locs]


## get variables we need to merge on
c2p <- copy(d3[,c("location_name","year","sex","draw","pop","pop_int"),with=F])
c2p <- melt(c2p,id.vars=c("location_name","year","sex","draw"),value.nam="mult",variable.name="var")
c2p[,int:=F]
c2p[grepl("_int",var),int:=T]
c2p[,var:="pop"]

add <- copy(d2)
add[,pct_first:=count_first_arf/count_inc_arf]
add[,pct_first_int:=count_first_arf_int/count_inc_arf_int]
add[,count_inc_arf_totalcare_first:=pct_first*count_inc_arf_totalcare]
add[,count_inc_arf_totalcare_first_int:=pct_first_int*count_inc_arf_totalcare_int]
add <- add[,list(arf_cases_on_sp=sum(arf_cases_on_sp,na.rm=T),arf_cases_on_sp_int=sum(arf_cases_on_sp_int,na.rm=T),
                 count_inc_arf_totalcare_first=sum(count_inc_arf_totalcare_first,na.rm=T),count_inc_arf_totalcare_first_int=sum(count_inc_arf_totalcare_first_int,na.rm=T)),
           by=c("location_name","year","draw")]
add[,sex:="both"]
add <- melt(add,id.vars=c("location_name","year","sex","draw"),variable.name="var",value.nam="mult")
add[,int:=F]
add[grepl("_int",var),int:=T]
add[,var:=gsub("_int","",var)]

c2p <- rbind(c2p,add)


add <- copy(d2[age >=15,c("location_name","year","sex","draw","pop","pop_int"),with=F])
add <- add[,list(pop=sum(pop,na.rm=T),pop_int=sum(pop_int,na.rm=T)),by=c("location_name","year","draw")]
add <- melt(add,id.vars=c("location_name","year","draw"),value.nam="mult",variable.name="var")
add[,int:=F]
add[grepl("_int",var),int:=T]
add[,var:='pop15+']
add[,sex:="both"]

c2p <- rbind(c2p,add)

## to make merge safer replicate for years and intervention status
c2 <- rbindlist(lapply(base_year:(base_year+total_rounds-1),FUN=function(x){
  ret <- copy(c2)
  ret[,year:=x]
  ret[,int:=F]
  add <- copy(ret)
  add[,int:=T]
  ret <- rbind(ret,add)
  return(ret)
}))

## merge unit costs and populations to multiply by
c2 <- merge(c2,c2p,by=c("location_name","year","draw","var","int"),all.x=T)
if (any(is.na(c2$mult))) stop('missing')


## merge costs and coverage
c2 <- merge(c2,copy(cov[intervention_id==2,c("location_name","coverage_level","starting_coverage","draw","year"),with=F]),by=c("location_name","year","draw"),all.x=T)

## some equipment should be scaled with facility expansion rather than coverage at the person level
addfaccov <- copy(cov[intervention_id==4])
## correct coverage start to coverage at facilities assumed
addfaccov[,starting_coverage:=starting_coverage*0.1/0.08]
addfaccov[year %in% c(2017:2020),coverage_level:=starting_coverage]
addfaccov[coverage_increase > .00001,coverage_end_draw:=1]
addfaccov[coverage_increase > .00001,coverage_level:=(coverage_end_draw-starting_coverage)*pct_progress_fac+starting_coverage]
setnames(addfaccov,c("coverage_level","starting_coverage"),c("coverage_level_facility","starting_coverage_facility"))
addfaccov <- addfaccov[,c("location_name","coverage_level_facility","starting_coverage_facility","draw","year"),with=F]
c2 <- merge(c2,addfaccov,by=c("location_name","year","draw"),all.x=T)


## mult for several components already incorporates coverage (per case and per treated case)
## we just need to use coverage for those that aren't already incorporated
## get coverages organized
c2[var %in% c("arf_cases_on_sp","count_inc_arf_totalcare"),c("coverage_level","starting_coverage"):=1]
c2[item == "Mass media awareness/education campaign",c("starting_coverage"):=0]
c2[item == "Mass media awareness/education campaign" & int == F,c("starting_coverage","coverage_level"):=0]
c2[int==T & year > 2020 & item == "Mass media awareness/education campaign" & abs(coverage_level-starting_coverage)>0.00001,coverage_level:=1] ## if there is a change in coverage, make it a full cost item
c2[year < 2021 & item == "Mass media awareness/education campaign",coverage_level:=0]

if (specs$end_scaleup != "No") c2[year >= as.numeric(specs$end_scaleup) & item == "Mass media awareness/education campaign",coverage_level:=0]

## adjust for monthly administration of secondary prophylaxis
c2[var=="arf_cases_on_sp",unit_cost:=unit_cost*12]

## cost with no intervention scale-up is unit cost*unit cost denom*baseline coverage
c2[int==F,cost:=unit_cost*mult*starting_coverage]
## cost with intervention scale-up is unit cost*unit cost denom*coverage level
c2[int==T,cost:=unit_cost*mult*coverage_level]

## facility-level costs may be higher than population coverage (some costs scaled earlier for services to be offered)
## cost with no intervention scale-up is unit cost*unit cost denom*baseline coverage
c2[int==F & item %in% c("Ongoing provider education and mentorship",
                        "Baseline training","Other clinic supplies","Ultrasound equipment"),cost:=unit_cost*mult*starting_coverage_facility]
## cost with intervention scale-up is unit cost*unit cost denom*coverage level
c2[int==T & item %in% c("Ongoing provider education and mentorship",
                        "Baseline training","Other clinic supplies","Ultrasound equipment"),cost:=unit_cost*mult*coverage_level_facility]


## Intervention 3:
## Patient-years with heart failure
## Population

## load cost info
c3 <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/other_inputs/intervention_costs.xlsx"),
                                     sheet=3,startRow=3))
c3 <- c3[!is.na(Currency),c(1:10),with=F]
setnames(c3,c("item","toggle","mean","lower","upper","currency","currency_year","cost_type","shared_intervention","country_costed"))

## only cost ones with toggle on for sensitivity
c3 <- c3[toggle==1]

## convert any that are every X year to annual
c3[grepl("per 3 years",cost_type),mean:=mean/3]
c3[grepl("per 3 years",cost_type),lower:=lower/3]
c3[grepl("per 3 years",cost_type),upper:=upper/3]
c3[grepl("per 5 years",cost_type),mean:=mean/5]
c3[grepl("per 5 years",cost_type),lower:=lower/5]
c3[grepl("per 5 years",cost_type),upper:=upper/5]
c3[grepl("initial cost",cost_type),mean:=mean/10]
c3[grepl("initial cost",cost_type),lower:=lower/10]
c3[grepl("initial cost",cost_type),upper:=upper/10]
c3[grepl("per 300,000 people",cost_type),mean:=mean/300000]
c3[grepl("per 300,000 people",cost_type),lower:=lower/300000]
c3[grepl("per 300,000 people",cost_type),upper:=upper/300000]
c3[grepl("per 300,000 people",cost_type),var:="pop"]
c3[grepl("per patient-year",cost_type),var:="sev_prev_pys"]

## prep to run through function that converts costs to 2020 USD
c3 <- convert_costs(c3,locs=locs)

## make draws
set.seed(38930983)
c3draws <- cbind(copy(c3[location_name=="African Union",c("item","location_name","mean"),with=F]),matrix(rtri(drawnum*nrow(c3[location_name=="African Union"]),
                                                                                                              mode=c3[location_name=="African Union"]$mean,
                                                                                                              min=c3[location_name=="African Union"]$lower,max=c3[location_name=="African Union"]$upper),nrow=nrow(c3[location_name=="African Union"])))
setnames(c3draws,"mean","au_mean")
c3 <- merge(c3,c3draws[,names(c3draws)[!names(c3draws) %in% c("location_name")],with=F],by=c("item"),all.x=T)
c3[,rat:=mean/au_mean]
for (i in c(paste0("V",1:drawnum))) {
  c3[,c(paste0(i)):=c3[[paste0(i)]]*rat]
}
c3[,c("au_mean","rat"):=NULL]
c3 <- melt(c3,id.vars=c(names(c3)[!names(c3) %in% paste0("V",c(1:drawnum))]),variable.name="draw",value.name="unit_cost")
c3[,draw:=as.numeric(gsub("V","",draw))]
rm(c3draws); gc()
c3 <- c3[location_name %in% locs]



## get variables we need to merge on
c3p <- copy(d3[,c("location_name","year","sex","draw","pop","pop_int","sev_prev_pys","sev_prev_pys_int"),with=F])
c3p <- melt(c3p,id.vars=c("location_name","year","sex","draw"),value.nam="mult",variable.name="var")
c3p[,int:=F]
c3p[grepl("_int",var),int:=T]
c3p[,var:=gsub("_int","",var)]


## to make merge safer replicate for years and intervention status
c3 <- rbindlist(lapply(base_year:(base_year+total_rounds-1),FUN=function(x){
  ret <- copy(c3)
  ret[,year:=x]
  ret[,int:=F]
  add <- copy(ret)
  add[,int:=T]
  ret <- rbind(ret,add)
  return(ret)
}))

## merge unit costs and populations to multiply by
c3 <- merge(c3,c3p,by=c("location_name","year","draw","var","int"),all.x=T)
if (any(is.na(c3$mult))) stop('missing')

## merge costs and coverage
c3 <- merge(c3,copy(cov[intervention_id==4,c("location_name","coverage_level","starting_coverage","draw","year"),with=F]),by=c("location_name","year","draw"),all.x=T)

c3 <- merge(c3,addfaccov,by=c("location_name","year","draw"),all.x=T)


## we do need to use coverage here
## sev_prev_pys is not severe on treatment, it's just severe overall, so coverage should be used for HF management

## cost with no intervention scale-up is unit cost*unit cost denom*baseline coverage
c3[int==F,cost:=unit_cost*mult*starting_coverage]
## cost with intervention scale-up is unit cost*unit cost denom*coverage level
c3[int==T,cost:=unit_cost*mult*coverage_level]

## cost with no intervention scale-up is unit cost*unit cost denom*baseline coverage
c3[int==F & item %in% c("Ongoing provider education and mentorship",
                        "Baseline training","Other clinic supplies","Ultrasound equipment","INR machine"),cost:=unit_cost*mult*starting_coverage_facility]
## cost with intervention scale-up is unit cost*unit cost denom*coverage level
c3[int==T & item %in% c("Ongoing provider education and mentorship",
                        "Baseline training","Other clinic supplies","Ultrasound equipment","INR machine"),cost:=unit_cost*mult*coverage_level_facility]



## Intervention 4:
## Number of surgeries
## Patient-years post surgery
## Population
## Countries

## load cost info
c4 <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/other_inputs/intervention_costs.xlsx"),
                                     sheet=4,startRow=3))
c4 <- c4[!is.na(Currency),c(1:10),with=F]
setnames(c4,c("item","toggle","mean","lower","upper","currency","currency_year","cost_type","shared_intervention","country_costed"))

## only cost ones with toggle on for sensitivity
c4 <- c4[toggle==1]


## convert any that are every X year to annual
c4[grepl("per 3 years",cost_type),mean:=mean/3]
c4[grepl("per 3 years",cost_type),lower:=lower/3]
c4[grepl("per 3 years",cost_type),upper:=upper/3]
c4[grepl("per 5 years",cost_type),mean:=mean/5]
c4[grepl("per 5 years",cost_type),lower:=lower/5]
c4[grepl("per 5 years",cost_type),upper:=upper/5]
c4[grepl("initial cost",cost_type),mean:=mean/10]
c4[grepl("initial cost",cost_type),lower:=lower/10]
c4[grepl("initial cost",cost_type),upper:=upper/10]
c4[grepl("per 300,000 people",cost_type),mean:=mean/300000]
c4[grepl("per 300,000 people",cost_type),lower:=lower/300000]
c4[grepl("per 300,000 people",cost_type),upper:=upper/300000]
c4[grepl("per 300,000 people",cost_type),var:="pop"]
c4[grepl("per patient-year",cost_type),var:="postsurg_pys"]
c4[grepl("per surgery",cost_type),var:="num_surgs"] 
c4[grepl("per 10 million pop",cost_type),mean:=mean/10000000]
c4[grepl("per 10 million pop",cost_type),lower:=lower/10000000]
c4[grepl("per 10 million pop",cost_type),upper:=upper/10000000]
c4[grepl("per 10 million pop",cost_type),var:="pop"]


## prep to run through function that converts costs to 2020 USD
c4 <- convert_costs(c4,locs=locs)


## make draws
set.seed(4736843)
c4draws <- cbind(copy(c4[location_name=="African Union",c("item","location_name","mean"),with=F]),matrix(rtri(drawnum*nrow(c4[location_name=="African Union"]),
                                                                                                              mode=c4[location_name=="African Union"]$mean,
                                                                                                              min=c4[location_name=="African Union"]$lower,max=c4[location_name=="African Union"]$upper),nrow=nrow(c4[location_name=="African Union"])))
setnames(c4draws,"mean","au_mean")
c4 <- merge(c4,c4draws[,names(c4draws)[!names(c4draws) %in% c("location_name")],with=F],by=c("item"),all.x=T)
c4[,rat:=mean/au_mean]
for (i in c(paste0("V",1:drawnum))) {
  c4[,c(paste0(i)):=c4[[paste0(i)]]*rat]
}
c4[,c("au_mean","rat"):=NULL]
c4 <- melt(c4,id.vars=c(names(c4)[!names(c4) %in% paste0("V",c(1:drawnum))]),variable.name="draw",value.name="unit_cost")
c4[,draw:=as.numeric(gsub("V","",draw))]
rm(c4draws); gc()
c4 <- c4[location_name %in% locs]


## get variables we need to merge on
c4p <- copy(d3[,c("location_name","year","sex","draw","pop","pop_int","num_surgs","num_surgs_int","postsurg_pys","postsurg_pys_int"),with=F])
c4p <- melt(c4p,id.vars=c("location_name","year","sex","draw"),value.nam="mult",variable.name="var")
c4p[,int:=F]
c4p[grepl("_int",var),int:=T]
c4p[,var:=gsub("_int","",var)]


## to make merge safer replicate for years and intervention status
c4 <- rbindlist(lapply(base_year:(base_year+total_rounds-1),FUN=function(x){
  ret <- copy(c4)
  ret[,year:=x]
  ret[,int:=F]
  add <- copy(ret)
  add[,int:=T]
  ret <- rbind(ret,add)
  return(ret)
}))

## merge unit costs and populations to multiply by
c4 <- merge(c4,c4p,by=c("location_name","year","draw","var","int"),all.x=T)
if (any(is.na(c4$mult))) stop('missing')

## merge costs and coverage
c4 <- merge(c4,copy(cov[intervention_id==5,c("location_name","coverage_level","starting_coverage","draw","year"),with=F]),by=c("location_name","year","draw"),all.x=T)

c4 <- merge(c4,addfaccov,by=c("location_name","year","draw"),all.x=T)


## coverage is irrelevant for costs of surgery and post-op care because they're 100% covered in each scenario, just different numbers are done
## but coverage part matters for other costs

## cost with no intervention scale-up is unit cost*unit cost denom*baseline coverage
c4[int==F,cost:=unit_cost*mult*starting_coverage]
c4[int==F & cost_type %in% c("per surgery","per patient-year"),cost:=unit_cost*mult]

## cost with intervention scale-up is unit cost*unit cost denom*coverage level
c4[int==T,cost:=unit_cost*mult*coverage_level]
c4[int==T & cost_type %in% c("per surgery","per patient-year"),cost:=unit_cost*mult]

## cost with no intervention scale-up is unit cost*unit cost denom*baseline coverage
c4[int==F & item %in% c("Ongoing provider education and mentorship",
                        "Baseline training","Other clinic supplies","Ultrasound equipment","INR machine","More advanced ultrasound equipment (national-level)"),cost:=unit_cost*mult*starting_coverage_facility]
## cost with intervention scale-up is unit cost*unit cost denom*coverage level
c4[int==T & item %in% c("Ongoing provider education and mentorship",
                        "Baseline training","Other clinic supplies","Ultrasound equipment","INR machine","More advanced ultrasound equipment (national-level)"),cost:=unit_cost*mult*coverage_level_facility]




## compile and figure out which costs we need to deduplicate
c1[,costed_in:="1"]
c2[,costed_in:="2"]
c3[,costed_in:="3"]
c4[,costed_in:="4"]

totcost <- rbindlist(list(c1,c2,c3,c4),fill=T,use.names=T)

totcost <- totcost[location_name %in% c(locs)]


## There are some items that are components of multiple interventions (such as costs associated with provider education and mentorship that overlap for
## primary and secondary prevention). Because coverage of these interventions is not necessarily scaled up at the same rate but this shared component is required
## for both, if we are doing the costing for both, we should only cost once BUT the cost may be different given the differences in coverage between the two different
## interventions. So, we want to (a) eliminate double counting and (b) cost the intervention appropriately (cost for whichever intervention is being scaled up to a 
## higher degree because that is what will be required to increase coverage)
totcost[,cov_change:=coverage_level-starting_coverage]

## ongoing provider education and mentorship is costed per pop, so we can use whichever has biggest delta in coverage between intervention 1 and 2 for costing the
## ongoing provider education and mentorship. Additionally, we won't cost that component if we're doing the CHW approach and there's no increase in secondary prevention
## since this cost is essentially for mentorship program for nurses at health centers from nurses at district hospitals.
mean_1_cov <- mean(totcost[item=="Ongoing provider education and mentorship" & costed_in == 1]$cov_change)
mean_2_cov <- mean(totcost[item=="Ongoing provider education and mentorship" & costed_in == 2]$cov_change)
if (specs$community_based_gas=="Yes") {
  ## if community-based gas, then get rid of ongoing training between DH/HC
  totcost <- totcost[!(item=="Ongoing provider education and mentorship" & costed_in==1)]
  ## and we can keep costing from the second part
} else if (specs$community_based_gas=="No") {
  ## keep whichever has bigger increase in coverage (since # units is pop-based so the same)
  if (mean_1_cov - mean_2_cov > 0) totcost <- totcost[!(item=="Ongoing provider education and mentorship" & costed_in==2)]
  if (mean_1_cov - mean_2_cov < 0) totcost <- totcost[!(item=="Ongoing provider education and mentorship" & costed_in==1)]
  
  ## if they're the same, then cost under 2
  if (mean_1_cov == mean_2_cov) totcost <- totcost[!(item=="Ongoing provider education and mentorship" & costed_in==1)]
}

## mass media awareness will vary between int 1 and 2 because of random draws, but the multiplier should be the same (per capita cost)
## so double check that coverage is changing in both 1 and 2 (and since we're doing cost of this as 100% or 0% coverage, then the cov change should be the same)
## then just do costing as part of 2
if (totcost[item=="Mass media awareness/education campaign" & int==T & costed_in == 1 & draw == 1 & year == 2021]$cov_change != totcost[item=="Mass media awareness/education campaign" & int==T & costed_in == 2 & draw == 1 & year == 2021]$cov_change) stop("discrepancy")
totcost <- totcost[!(item %in% c("Mass media awareness/education campaign") & costed_in == 1)]

## baseline training, clinic supplies, ultrasound equipment, INR all costed as part of facility expansion coverage
## use from the HF intervention for consistency since SP, HF management, and surg all scaled together
## and facility-level coverage at first-level hospitals is same
totcost <- totcost[!(item %in% c("Baseline training","Other clinic supplies","Ultrasound equipment","INR machine") & costed_in %in% c(2,4))] ## keep from the HF part

unique(totcost[,c("shared_intervention","costed_in"),with=F])
unique(totcost[!is.na(shared_intervention),c("item","costed_in"),with=F])

if (nrow(unique(totcost[!is.na(shared_intervention),c("item","costed_in"),with=F])) > length(unique(totcost[!is.na(shared_intervention),c("item","costed_in"),with=F]$item))) stop("didn't deduplicate correctly")
  
## change the labels of the shared interventions to reflect the correct shared interventions
totcost[item=="Baseline training",shared_intervention:="Intervention 2, 3, & 4"]
totcost[item=="Other clinic supplies",shared_intervention:="Intervention 2, 3, & 4"]
totcost[item=="Ultrasound equipment",shared_intervention:="Intervention 2, 3, & 4"]
totcost[item=="INR machine",shared_intervention:="Intervention 3 & 4"]
if (specs$community_based_gas=="Yes") totcost[item=="Ongoing provider education and mentorship",shared_intervention:=NA]
if (specs$community_based_gas=="No") totcost[item=="Ongoing provider education and mentorship",shared_intervention:="Intervention 1 & 2"]
totcost[item=="Mass media awareness/education campaign",shared_intervention:="Intervention 1 & 2"]
totcost[is.na(shared_intervention),shared_intervention:=paste0("Intervention ",costed_in)]

## add discounted
totcost[,cost_discounted:=cost]
totcost[year >=2020,cost_discounted:=cost*(1/(1+specs$discount_rate))^(year-2020)]

## summarize and save outputs to be used later
totcost <- totcost[,list(cost=sum(cost),cost_discounted=sum(cost_discounted)),by=c("location_name","year","draw","sex","int","shared_intervention")]

## add AU (just for local version for testing)
if (length(unique(totcost$location_name)) > 1) {
  add <- copy(totcost)
  add <- add[,list(cost=sum(cost),cost_discounted=sum(cost_discounted)),by=c("year","draw","sex","int","shared_intervention")]
  add[,location_name:="African Union"]
  totcost <- rbind(totcost,add)
  rm(add); gc()  
}
saveRDS(totcost,paste0(outdirtmp,"/results/model_run_results/",run_num,"/full_cost_sims",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".rds"))

totcost <- dcast.data.table(totcost,location_name+year+draw+sex+shared_intervention~int,value.var=c("cost","cost_discounted"))

totcost[,cost_diff:=cost_TRUE-cost_FALSE]
totcost[,cost_discounted_diff:=cost_discounted_TRUE-cost_discounted_FALSE]


totcost_sum <- copy(totcost[,list(mean=mean(cost_diff),lower=quantile(cost_diff,probs=c(0.025)),upper=quantile(cost_diff,probs=c(0.975)),
                        median=quantile(cost_diff,probs=c(.5))),by=c("location_name","year","sex","shared_intervention")])
totcost_sum
totcost_sum_discounted <- copy(totcost[,list(mean=mean(cost_discounted_diff),lower=quantile(cost_discounted_diff,probs=c(0.025)),upper=quantile(cost_discounted_diff,probs=c(0.975)),
                                   median=quantile(cost_discounted_diff,probs=c(.5))),by=c("location_name","year","sex","shared_intervention")])
totcost_sum_discounted


allyearcost <- copy(totcost[,list(cost_diff=sum(cost_diff),cost_discounted_diff=sum(cost_discounted_diff)),by=c("location_name","draw","shared_intervention")])


totcost <- totcost[,list(cost_diff=sum(cost_diff),cost_discounted_diff=sum(cost_discounted_diff)),by=c("location_name","year","draw")]
saveRDS(totcost,paste0(outdirtmp,"/results/model_run_results/",run_num,"/cost",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".rds"))



totcost_totsum <- copy(totcost[,list(mean_cost=mean(cost_diff),lower_cost=quantile(cost_diff,probs=c(0.025)),upper_cost=quantile(cost_diff,probs=c(0.975)),median_cost=quantile(cost_diff,probs=c(.5)),
                              mean_cost_discounted=mean(cost_discounted_diff),lower_cost_discounted=quantile(cost_discounted_diff,probs=c(0.025)),upper_cost_discounted=quantile(cost_discounted_diff,probs=c(0.975)),
                              median_cost_discounted=quantile(cost_discounted_diff,probs=c(.5))),by=c("location_name","year")])

cumcost <- copy(totcost)
cumcost <- cumcost[,list(cost_diff=sum(cost_diff),cost_discounted_diff=sum(cost_discounted_diff)),by=c("location_name","year","draw")]
cumcost[,cumcost:=cumsum(cost_diff),by=c("draw")]
cumcost[,cumcost_discounted:=cumsum(cost_discounted_diff),by=c("location_name","draw")]

cumcost <- copy(cumcost[,list(mean_cost=mean(cumcost),lower_cost=quantile(cumcost,probs=c(0.025)),upper_cost=quantile(cumcost,probs=c(0.975)),
                              mean_cost_discounted=mean(cumcost_discounted),lower_cost_discounted=quantile(cumcost_discounted,probs=c(0.025)),
                              upper_cost_discounted=quantile(cumcost_discounted,probs=c(0.975))),by=c("location_name","year")])


sums <- copy(totcost_sum)
sums

sums_discounted <- copy(totcost_sum_discounted)
sums_discounted
setnames(sums_discounted,c("mean","lower","upper","median"),c("mean_discounted","lower_discounted","upper_discounted","median_discounted"))

sums <- merge(sums,sums_discounted,by=c("location_name","year","sex","shared_intervention"),all.x=T)

setnames(sums,"shared_intervention","cat")
sums[,cat:=factor(cat,levels=c("Intervention 1","Intervention 2","Intervention 3","Intervention 4","Intervention 1 & 2","Intervention 2, 3, & 4","Intervention 3 & 4"),
                  labels=c("Primary Prevention\n(treatment of sore throat)","Secondary Prophylaxis\n(penicillin for ARF and mild RHD)",
                           "HF Management","Surgery & Post-Operative\nManagement","Shared Costs: Primary and\nSecondary Prophylaxis",
                           "Shared Costs: Equipment and Training\nfor Secondary Prophylaxis, HF, & Surgery","Shared Costs: HF & Surgery"))]


outsum <- copy(sums)
outsum2 <- copy(totcost_totsum)
outsum2[,sex:="both"]
outsum2[,cat:="Total"]
setnames(outsum2,gsub("_cost","",names(outsum2)))
outsum <- rbind(outsum,outsum2)



add <- copy(allyearcost)
add <- add[,list(cost_diff=sum(cost_diff),cost_discounted_diff=sum(cost_discounted_diff)),by=c("location_name","draw")]
add <- add[,list(mean_cost=mean(cost_diff),lower_cost=quantile(cost_diff,probs=c(0.025)),upper_cost=quantile(cost_diff,probs=c(0.975)),median_cost=quantile(cost_diff,probs=c(.5)),
                 mean_cost_discounted=mean(cost_discounted_diff),lower_cost_discounted=quantile(cost_discounted_diff,probs=c(0.025)),upper_cost_discounted=quantile(cost_discounted_diff,probs=c(0.975)),
                 median_cost_discounted=quantile(cost_discounted_diff,probs=c(.5))),by=c("location_name")]
add[,cat:="Total"]
add[,sex:="both"]
add[,year:="All"]

allyearcost[,cat:=factor(shared_intervention,levels=c("Intervention 1","Intervention 2","Intervention 3","Intervention 4","Intervention 1 & 2","Intervention 2, 3, & 4","Intervention 3 & 4"),
                         labels=c("Primary Prevention\n(treatment of sore throat)","Secondary Prophylaxis\n(penicillin for ARF and mild RHD)",
                                  "HF Management","Surgery & Post-Operative\nManagement","Shared Costs: Primary and\nSecondary Prophylaxis",
                                  "Shared Costs: Equipment and Training\nfor Secondary Prophylaxis, HF, & Surgery","Shared Costs: HF & Surgery"))]
allyearcost <- allyearcost[,list(mean_cost=mean(cost_diff),lower_cost=quantile(cost_diff,probs=c(0.025)),upper_cost=quantile(cost_diff,probs=c(0.975)),median_cost=quantile(cost_diff,probs=c(.5)),
                                 mean_cost_discounted=mean(cost_discounted_diff),lower_cost_discounted=quantile(cost_discounted_diff,probs=c(0.025)),upper_cost_discounted=quantile(cost_discounted_diff,probs=c(0.975)),
                                 median_cost_discounted=quantile(cost_discounted_diff,probs=c(.5))),by=c("location_name","cat")]
allyearcost[,sex:="both"]
allyearcost[,year:="All"]
allyearcost <- rbind(allyearcost,add)
setnames(allyearcost,gsub("_cost","",names(allyearcost)))

outsum <- rbind(outsum,allyearcost,use.names=T)


saveRDS(outsum,paste0(paste0(outdir,"/results/model_run_results/",run_num,"/cost_summary",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".rds")))


cols <- brewer.pal(n=8,"Set1")[-6]

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols <- gg_color_hue(7)
cols <- c("#955E42","#0B132B","#1C2541","#3A506B","#9C914F","#748E54")
cols <- c("#E1CA96","#0B132B","#1C2541","#3A506B","#9C914F","#748E54")
cols <- c("#DE9151","#0B132B","#1C2541","#3A506B","#9C914F","#748E54")
cols <- c("#DE9151","#0B132B","#3A506B","steelblue3","#9C914F","#748E54")


test <- copy(sums[cat!="Primary Prevention\n(treatment of sore throat)" & location_name=="African Union"])
test[,shared:=0]
test[grepl("Shared",cat),shared:=1]
test <- test[,list(mean=sum(mean)),by=c("shared")]
test[,pct:=mean/sum(mean)]
test

test <- copy(sums[year==2030 & location_name=="African Union"])
test[,pp:=1]
test[cat!="Primary Prevention\n(treatment of sore throat)",pp:=0]
test <- test[,list(mean=sum(mean)),by=c("pp")]
test[,pct:=mean/sum(mean)]
test

sums[cat=="Shared Costs: HF & Surgery",cat:="Shared Costs: Equipment and Training\nfor Secondary Prophylaxis, HF, & Surgery"]

## may change to geographic regions?
# sums <- sums[location_name=="African Union"]
# totcost_totsum <- totcost_totsum[location_name=="African Union"]


pdf(paste0(outdir,"/results/model_run_results/",run_num,"/costing_stacked_bars",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".pdf"),width=9,height=6)

for (i in unique(sums$location_name)) {
  
  gg <- ggplot(sums[year >=2020 & location_name == i],aes(x=year,y=mean/1000000,group=cat,fill=cat)) + geom_bar(stat="identity",position="stack") +
    theme_bw() + ylab("Cost USD (millions)") + scale_x_continuous("Year",breaks=c(2020:(base_year+total_rounds-1))) + scale_fill_manual("",values=cols) + 
    theme(legend.position="bottom") + guides(fill=guide_legend(nrow=2,byrow=TRUE)) +   scale_y_continuous(labels = comma) + ggtitle(paste0(i))
  print(gg)
  
  
  gg <- ggplot(sums[year >=2020 & cat!="Primary Prevention\n(treatment of sore throat)" & location_name == i],aes(x=year,y=mean/1000000,group=cat,fill=cat)) + geom_bar(stat="identity",position="stack") +
    theme_bw() + ylab("Cost USD (millions)") + scale_x_continuous("Year",breaks=c(2020:(base_year+total_rounds-1))) + scale_fill_manual("",values=cols[-1]) + 
    theme(legend.position="bottom") + guides(fill=guide_legend(nrow=2,byrow=TRUE)) + ggtitle(paste0(i))
  print(gg)
  
  
  gg <- ggplot(sums[year >=2020 & location_name == i],aes(x=year,y=mean_discounted/1000000,group=cat,fill=cat)) + geom_bar(stat="identity",position="stack") +
    theme_bw() + ylab("Cost USD (millions), discounted (3%)") + scale_x_continuous("Year",breaks=c(2020:(base_year+total_rounds-1))) + scale_fill_manual("",values=cols) + 
    theme(legend.position="bottom") + guides(fill=guide_legend(nrow=2,byrow=TRUE)) +   scale_y_continuous(labels = comma) + ggtitle(paste0(i))
  print(gg)
  
  
  gg <- ggplot(sums[year >=2020 & cat!="Primary Prevention\n(treatment of sore throat)" & location_name == i],aes(x=year,y=mean_discounted/1000000,group=cat,fill=cat)) + geom_bar(stat="identity",position="stack") +
    theme_bw() + ylab("Cost USD (millions), discounted (3%)") + scale_x_continuous("Year",breaks=c(2020:(base_year+total_rounds-1))) + scale_fill_manual("",values=cols[-1]) + 
    theme(legend.position="bottom") + guides(fill=guide_legend(nrow=2,byrow=TRUE)) + ggtitle(paste0(i))
  print(gg)  
}

gg <- ggplot(sums[year >=2020],aes(x=year,y=mean/1000000,group=cat,fill=cat)) + geom_bar(stat="identity",position="stack") +
  theme_bw() + ylab("Cost USD (millions)") + scale_x_continuous("Year",breaks=c(2020:(base_year+total_rounds-1))) + scale_fill_manual("",values=cols) + 
  theme(legend.position="bottom") + guides(fill=guide_legend(nrow=2,byrow=TRUE)) +   scale_y_continuous(labels = comma) + 
  facet_wrap(~location_name)
print(gg)

dev.off()



pdf(paste0(outdir,"/results/model_run_results/",run_num,"/costing_totals",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".pdf"),width=6,height=5)

for (i in unique(totcost_totsum$location_name)) {
  
  gg <- ggplot(totcost_totsum[year >=2020 & location_name==i],aes(x=year,y=mean_cost/1000000)) + geom_line() +
    geom_ribbon(aes(ymin = lower_cost/1000000, ymax = upper_cost/1000000),alpha=.2,colour=NA) +
    theme_bw() + ylab("Annual Cost USD (millions)") + scale_x_continuous("Year",breaks=c(2020:(base_year+total_rounds-1))) +
    scale_y_continuous(labels = comma) + ggtitle(paste0(i))
  print(gg)
  
  
  gg <- ggplot(cumcost[year >=2020 & location_name==i],aes(x=year,y=mean_cost/1000000)) + geom_line() +
    geom_ribbon(aes(ymin = lower_cost/1000000, ymax = upper_cost/1000000),alpha=.2,colour=NA) +
    theme_bw() + ylab("Cumulative Cost USD (millions)") + scale_x_continuous("Year",breaks=c(2020:(base_year+total_rounds-1))) +
    scale_y_continuous(labels = comma) + ggtitle(paste0(i))
  print(gg)
  
  gg <- ggplot(totcost_totsum[year >=2020 & location_name==i],aes(x=year,y=mean_cost_discounted/1000000)) + geom_line() +
    geom_ribbon(aes(ymin = lower_cost_discounted/1000000, ymax = upper_cost_discounted/1000000),alpha=.2,colour=NA) +
    theme_bw() + ylab("Annual Cost USD (millions), discounted (3%)") + scale_x_continuous("Year",breaks=c(2020:(base_year+total_rounds-1))) +
    scale_y_continuous(labels = comma) + ggtitle(paste0(i))
  print(gg)
  
  
  gg <- ggplot(cumcost[year >=2020 & location_name==i],aes(x=year,y=mean_cost_discounted/1000000)) + geom_line() +
    geom_ribbon(aes(ymin = lower_cost_discounted/1000000, ymax = upper_cost_discounted/1000000),alpha=.2,colour=NA) +
    theme_bw() + ylab("Cumulative Cost USD (millions), discounted (3%)") + scale_x_continuous("Year",breaks=c(2020:(base_year+total_rounds-1))) +
    scale_y_continuous(labels = comma) + ggtitle(paste0(i))
  print(gg)

}


gg <- ggplot(totcost_totsum[year >=2020],aes(x=year,y=mean_cost/1000000)) + geom_line() +
  geom_ribbon(aes(ymin = lower_cost/1000000, ymax = upper_cost/1000000),alpha=.2,colour=NA) +
  theme_bw() + ylab("Annual Cost USD (millions)") + scale_x_continuous("Year",breaks=c(2020:(base_year+total_rounds-1))) +
  scale_y_continuous(labels = comma) + facet_wrap(~location_name)
print(gg)


gg <- ggplot(cumcost[year >=2020],aes(x=year,y=mean_cost/1000000)) + geom_line() +
  geom_ribbon(aes(ymin = lower_cost/1000000, ymax = upper_cost/1000000),alpha=.2,colour=NA) +
  theme_bw() + ylab("Cumulative Cost USD (millions)") + scale_x_continuous("Year",breaks=c(2020:(base_year+total_rounds-1))) +
  scale_y_continuous(labels = comma) + facet_wrap(~location_name)
print(gg)


dev.off()

