## Matthew Coates
## Compile input data for models
## And make draws to propagate uncertainty


rm(list=ls())
root <- "M:/"
laptop <- F
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
  codedir <- paste0("[INSERT OWN DIRECTORY HERE]/rhd_invest_pub/")
  outdir <- paste0("[INSERT OWN DIRECTORY HERE]/rhd_investment_case/")
  draws <- 200
} else {
  codedir <- paste0("[INSERT OWN DIRECTORY HERE]/rhd_invest_pub/")
  outdir <- paste0("[INSERT OWN DIRECTORY HERE]/rhd_investment_case/")
  draws <- 4000
}

## load functions
source(paste0(codedir,"/functions/swap_location_names.R"))
source(paste0(codedir,"/functions/map_ids_names.R"))

specs <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/run_key.xlsx")))



## read in AU locations (https://au.int/en/member_states/countryprofiles2)
locs <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/AU_member_states.xlsx")))
locs <- locs[!location_name %in% c("Sahrawi Republic")] ## we don't have burden estimates for Sahrawi Republic, so we will drop

## convert to GBD country names
locs <- swap_locnames(data=locs,out="GBD",version=2017)


################################################################
## background mortality inputs
################################################################
gbd <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_inc_prev_death_inputs.RDS"))
gbd <- gbd[!age_group_id %in% c(2,3,4,22,27) & year ==2017]
gbd <- map_to_names(d=gbd,codedir=codedir,keep_ids=T,gbd=2017)
gbd <- gbd[location_name %in% c(locs$location_name,"African Union")]
gbd[,c("age","loc_level"):=NULL]


simlist <- list()
count <- 1
tot <- nrow(gbd)
for (h in unique(gbd$location_name)) {
  for (i in c("Deaths","Prevalence","Incidence")) {
    for (j in c("male","female")) {
      for (k in unique(gbd$age_group_name)) {
        for (l in unique(gbd$cause_name)) {
          for (m in unique(gbd$year)) { ## only one year here
            #cat(paste0(i,j,k,l,m)); flush.console()
            cat(paste0(count," of ",tot," ")); flush.console()
            
            if (i=="Deaths") set.seed(153839)
            if (i %in% c("Prevalence","Incidence")) set.seed(573537)
            
            if ((l=="All causes" & i %in% c("Prevalence","Incidence"))) {
              
            } else {
              
              simlist[[paste0(i,j,k,l,m,h)]] <- data.table(data.frame(measure_name=i,sex=j,age_group_name=k,cause_name=l,year=m,location_name=h))
              
              if (gbd[measure_name==i & sex==j & age_group_name==k & cause_name==l & year == m & location_name==h]$lower < gbd[measure_name==i & sex==j & age_group_name==k & cause_name==l & year == m & location_name==h]$val & 
                  gbd[measure_name==i & sex==j & age_group_name==k & cause_name==l & year == m & location_name==h]$val < gbd[measure_name==i & sex==j & age_group_name==k & cause_name==l & year == m & location_name==h]$upper) {
                tmpsims <- t(rtri(draws,min=gbd[measure_name==i & sex==j & age_group_name==k & cause_name==l & year == m & location_name==h]$lower,
                                  max=gbd[measure_name==i & sex==j & age_group_name==k & cause_name==l & year == m & location_name==h]$upper,
                                  mode=gbd[measure_name==i & sex==j & age_group_name==k & cause_name==l & year == m & location_name==h]$val))
              } else {
                tmpsims <- t(rep(gbd[measure_name==i & sex==j & age_group_name==k & cause_name==l & year == m & location_name==h]$val,draws))
              }
              
              simlist[[paste0(i,j,k,l,m,h)]] <- cbind(simlist[[paste0(i,j,k,l,m,h)]],tmpsims)
              
              count <- count + 1          
              
            }
            
            
          }
        }
      }
    }
  }
}

simlist <- rbindlist(simlist)
gbd <- merge(gbd,simlist,by=c("measure_name","sex","age_group_name","cause_name","year","location_name"),all=T)

## extend rates to single-year age groups
gbd <- gbd[!age_group_id %in% c(2,3,4,22,27)]
gbd[age_group_id == 28,age_group_name:="0"]
gbd[age_group_id==28,age_group_id:=1]
gbd <- rbind(gbd,gbd,gbd,gbd,gbd)
gbd <- gbd[order(location_name,sex,cause_name,measure_name,age_group_id)]
gbd[, id := seq_len(.N), by = c("location_name","sex","cause_name","measure_name","age_group_id")]
gbd <- gbd[!(age_group_name=="0" & id > 1) & !(age_group_name=="1 to 4" & id > 4)]
gbd[,age:=id-1+as.numeric(substr(age_group_name,1,2))]
gbd <- gbd[!age %in% c(96:99)]


## make sims of fertility
fert <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_fert.RDS"))
sims <- list()
for (j in unique(fert$location_name)) {
  for (k in unique(fert$age_group_name)) {
    set.seed(583804)
    sims[[paste0(j,k)]] <- data.table(t(as.data.frame(rtri(draws,min=fert[age_group_name==k & location_name==j]$lower,max=fert[age_group_name==k & location_name==j]$upper,
                                                           mode=fert[age_group_name==k & location_name==j]$val))))
    sims[[paste0(j,k)]][,location_name:=j]
    sims[[paste0(j,k)]][,age_group_name:=k]
    
  }
}
sims <- rbindlist(sims)
fert <- merge(fert,sims,by=c("location_name","age_group_name"),all.x=T)
## extend rates to corresponding single-year ages
fert <- rbind(fert,fert,fert,fert,fert)
fert <- fert[order(location_name,sex,age_group_name)]
fert[,age:=rowid(age_group_name)-1+as.numeric(substr(age_group_name,1,2)),by="location_name"]
fert <- melt(fert,id.vars=c("age","sex","location_id","location_name","sex_id","sex_name","age_group_id","age_group_name","year_id","measure_id","measure_name","metric_name","val","upper","lower"))
setnames(fert,c("variable","value"),c("draw","fert"))
fert[,draw:=as.numeric(gsub("V","",as.character(draw)))]

## load pop
pop <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_pop.RDS"))
pop <- pop[location_name %in% c(locs$location_name,"African Union")]
pop <- pop[year==2017]
## need  draws of starting pop (even if we aren't using uncertainty about starting pop) in order to propagate the effects from our uncertainty in projections
pop <- rbindlist(lapply(as.list(c(1:draws)),FUN=function(x) {
  out <- copy(pop)
  out[,draw:=x]
}))
pop <- pop[,c("location_name","year","sex","age","draw","pop"),with=F]
pop <- pop[order(location_name,year,sex,age,draw)]


## reshape inputs
gbd[,id:=NULL]
gbd <- melt(gbd,id.vars=c("measure_name","measure_id","sex","age_group_name","cause_name","year","location_name","location_id","sex_id","age_group_id","cause_id","lower","upper","val","age"),variable.name="draw")
gbd[,draw:=as.numeric(gsub("V","",draw))]

## REMOVE--JUST FOR WHILE CODING so don't have to rerun if something breaks
#backgbd <- copy(gbd)

gbd <- dcast.data.table(gbd,sex+age+measure_name+year+location_name+location_id+sex_id+draw~cause_name,value.var=c("value"))
gbd <- dcast.data.table(gbd,year+location_name+location_id+sex+sex_id+age+draw~measure_name,value.var=c("All causes","Rheumatic heart disease"))
gbd[,c("All causes_Incidence","All causes_Prevalence"):=NULL]
gbd <- merge(gbd,fert[,c("location_name","sex","age","draw","fert"),with=F],by=c("location_name","sex","age","draw"),all.x=T)
gbd[age < 10 | age > 54,fert:=0]
gbd[sex=="male",fert:=0]
if (nrow(gbd[is.na(fert)]) > 0) stop("missing fert")
gbd <- gbd[order(sex,age,draw)]

if (nrow(gbd[`Rheumatic heart disease_Prevalence` < 0 | `Rheumatic heart disease_Incidence` < 0 | `Rheumatic heart disease_Deaths` < 0 | 
             `Rheumatic heart disease_Prevalence`> 1 | `Rheumatic heart disease_Incidence` > 1]) > 0) stop("epi wrong")

## merge on pop
gbd <- merge(gbd,pop,by=c("location_name","year","sex","age","draw"),all.x=T)
if (nrow(gbd[is.na(pop)]) > 0) stop("missing pop")


## now, load some RHD progression inputs--these vary in sensitivity analysis, so loading multiple versions here
## implied from normal GBD estimates specs$hf_inc_option == "GBD"
implied_hf_inc <- readRDS(paste0(codedir,"/data/other_inputs/implied_hf_incidence.RDS"))
implied_hf_inc <- implied_hf_inc[year==2017]
implied_hf_inc[sex_id == 1,sex:="male"]
implied_hf_inc[sex_id == 2,sex:="female"]
setnames(implied_hf_inc,c("inc_implied","cases_mild_persistent_prev_yr_per_prev","cases_severe_persistent_prev_yr_per_prev"),
         c("GBD_inc_implied","GBD_cases_mild_persistent_prev_yr_per_prev","GBD_cases_severe_persistent_prev_yr_per_prev"))
implied_hf_inc[,c("location_name","age_group_name","pop"):=NULL]


## implied from age pattern based on Heart of Soweto Study combined with GBD specs$hf_inc_option == "Soweto"
implied_hf_inc2 <- readRDS(paste0(codedir,"/data/other_inputs/soweto_hf_prob_est.RDS"))
implied_hf_inc2[,year:=2017]
implied_hf_inc2[sex_id == 1,sex:="male"]
implied_hf_inc2[sex_id == 2,sex:="female"]
setnames(implied_hf_inc2,c("inc_implied","cases_mild_persistent_prev_yr_per_prev","cases_severe_persistent_prev_yr_per_prev"),
         c("Soweto_inc_implied","Soweto_cases_mild_persistent_prev_yr_per_prev","Soweto_cases_severe_persistent_prev_yr_per_prev"))
implied_hf_inc2[,c("location_name","age_group_name","pop"):=NULL]
implied_hf_inc <- merge(implied_hf_inc,implied_hf_inc2,by=c("sex","year","sex_id","age"),all.x=T)

## a consistent 0.8% annual risk (Watkins cost-effectiveness paper) specs$hf_inc_option == "Watkins"
implied_hf_inc2 <- readRDS(paste0(codedir,"/data/other_inputs/soweto_hf_prob_est.RDS"))
implied_hf_inc2[,year:=2017]
implied_hf_inc2[sex_id == 1,sex:="male"]
implied_hf_inc2[sex_id == 2,sex:="female"]
implied_hf_inc2[,inc_implied:=.008]
setnames(implied_hf_inc2,c("inc_implied","cases_mild_persistent_prev_yr_per_prev","cases_severe_persistent_prev_yr_per_prev"),
         c("Watkins_inc_implied","Watkins_cases_mild_persistent_prev_yr_per_prev","Watkins_cases_severe_persistent_prev_yr_per_prev"))
implied_hf_inc2[,c("location_name","age_group_name","pop"):=NULL]
implied_hf_inc <- merge(implied_hf_inc,implied_hf_inc2,by=c("sex","year","sex_id","age"),all.x=T)

## adjusted GBD specs$hf_inc_option == "GBD_adj", adjusted using Fiji data
implied_hf_inc2 <- readRDS(paste0(codedir,"/data/other_inputs/implied_hf_incidence_adjGBD.RDS"))
implied_hf_inc2[sex_id == 1,sex:="male"]
implied_hf_inc2[sex_id == 2,sex:="female"]
setnames(implied_hf_inc2,c("inc_implied","cases_mild_persistent_prev_yr_per_prev","cases_severe_persistent_prev_yr_per_prev"),
         c("GBDadj_inc_implied","GBDadj_cases_mild_persistent_prev_yr_per_prev","GBDadj_cases_severe_persistent_prev_yr_per_prev"))
implied_hf_inc2[,c("location_name","age_group_name","pop"):=NULL]
implied_hf_inc <- merge(implied_hf_inc,implied_hf_inc2,by=c("sex","year","sex_id","age"),all.x=T)

## implied from adjusted DisMod results specs$hf_inc_option == "DisMod" -- main model--combo of DisMod age pattern within envelope and CODEM at older ages
implied_hf_inc2 <- readRDS(paste0(codedir,"/data/other_inputs/implied_hf_incidence_DisMod_codem_adj_hybrid.RDS"))
implied_hf_inc2[sex_id == 1,sex:="male"]
implied_hf_inc2[sex_id == 2,sex:="female"]
implied_hf_inc2 <- implied_hf_inc2[year==2017]
setnames(implied_hf_inc2,c("inc_implied","cases_mild_persistent_prev_yr_per_prev","cases_severe_persistent_prev_yr_per_prev"),
         c("DisModadj_inc_implied","DisModadj_cases_mild_persistent_prev_yr_per_prev","DisModadj_cases_severe_persistent_prev_yr_per_prev"))
implied_hf_inc2[,c("location_name","age_group_name","pop"):=NULL]
implied_hf_inc <- merge(implied_hf_inc,implied_hf_inc2,by=c("sex","year","sex_id","age"),all.x=T)

# gg <- ggplot(implied_hf_inc,aes(x=age,y=inc_implied,group=sex,color=sex)) + geom_line() + scale_y_continuous(trans="log")
# print(gg)
# 1-prod(1-implied_hf_inc[sex=="male" & age %in% c(5:64)]$inc_implied)
# 1-prod(1-implied_hf_inc[sex=="male" & age %in% c(5:74)]$inc_implied)

rhd_inputs <- readRDS(paste0(codedir,"/data/gbd_inputs/prepped_hf_rhd_inputs.rds"))
rhd_inputs <- rhd_inputs[year == 2017]
rhd_inputs <- rhd_inputs[order(sex,age)]  
rhd_inputs <- merge(rhd_inputs,implied_hf_inc,by=c("sex","age","year"),all=T)
rhd_inputs <- rhd_inputs[order(location_name,sex,age)] 
rhd_inputs[,c("rhd_hf_prev_count","cause_name","rei_name"):=NULL]

## now use info from prevalence to get uncertainty range for leftover cases
prev <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_inc_prev_death_inputs.RDS"))
prev <- prev[!age_group_id %in% c(2,3,4,22,27) & year ==2017]
prev <- map_to_names(d=prev,codedir=codedir,keep_ids=T,gbd=2017)
prev <- prev[location_name %in% c(locs$location_name,"African Union")]
prev <- prev[measure_name=="Prevalence"]
prev[,c("age","loc_level"):=NULL]
prev <- prev[!age_group_id %in% c(2,3,4,22,27)]
prev[age_group_id == 28,age_group_name:="0"]
prev[age_group_id==28,age_group_id:=1]
prev <- rbind(prev,prev,prev,prev,prev)
prev <- prev[order(location_name,sex,cause_name,measure_name,age_group_id)]
prev[, id := seq_len(.N), by = c("location_name","sex","cause_name","measure_name","age_group_id")]
prev <- prev[!(age_group_name=="0" & id > 1) & !(age_group_name=="1 to 4" & id > 4)]
prev[,age:=id-1+as.numeric(substr(age_group_name,1,2))]
prev <- prev[!age %in% c(96:99)]
setnames(prev,"val","prev_rhd")

rhd_inputs <- merge(rhd_inputs,prev[,c("location_name","sex","age","year","prev_rhd"),with=F],by=c("location_name","sex","age","year"),all.x=T)
addpop <- copy(pop)
addpop <- addpop[,list(pop=mean(pop)),by=c("location_name","year","sex","age")]
rhd_inputs <- merge(rhd_inputs,addpop[,c("location_name","year","sex","age","pop"),with=F],by=c("location_name","year","sex","age"),all.x=T)
rm(addpop); rm(implied_hf_inc); rm(implied_hf_inc2)
gc()

for (i in c("GBD","Soweto","Watkins","GBDadj","DisModadj")) {
  rhd_inputs[[paste0(i,"_cases_severe_persistent_prev_yr_count")]] <- rhd_inputs[[paste0(i,"_cases_severe_persistent_prev_yr_per_prev")]]*rhd_inputs$prev_rhd*rhd_inputs$pop
  rhd_inputs[[paste0(i,"_cases_mild_persistent_prev_yr_count")]] <- rhd_inputs[[paste0(i,"_cases_mild_persistent_prev_yr_per_prev")]]*rhd_inputs$prev_rhd*rhd_inputs$pop
}

## we want to take draws of severe RHD as fraction of total so that we don't get negatives of mild (which we get by subtracting)
rhd_inputs[,rhd_hf_prev_frac:=rhd_hf_prev_rate/prev_rhd]
rhd_inputs[rhd_hf_prev_frac>=.9999999,rhd_hf_prev_frac:=.9999999]
rhd_inputs[is.na(rhd_hf_prev_frac),rhd_hf_prev_frac:=0]

rhd_inputs <- melt(rhd_inputs,id.vars=c("location_name","sex","sex_id","year","age","pop"))
rhd_inputs <- rhd_inputs[!variable %in% c("sex_id","prev_rhd")]
rhd_inputs <- rhd_inputs[!grepl("per_prev",variable)]
rhd_inputs[,variable:=as.character(variable)]

prev[,rat_low:=lower/prev_rhd]
prev[,rat_high:=upper/prev_rhd]
prev[prev_rhd==0,c("rat_low","rat_high"):=0]

rhd_inputs <- merge(rhd_inputs,prev[,c("age","sex","location_name","year","rat_low","rat_high"),with=F],by=c("age","sex","location_name","year"),all.x=T)
rhd_inputs[variable!="rhd_hf_prev_frac",lower:=rat_low*value]
rhd_inputs[variable!="rhd_hf_prev_frac",upper:=rat_high*value]
rhd_inputs[variable=="rhd_hf_prev_frac",lower:=value*.9]
rhd_inputs[variable=="rhd_hf_prev_frac",upper:=value*1.1]
rhd_inputs[variable=="rhd_hf_prev_frac" & upper > .99999999,upper:=.99999999]
setnames(rhd_inputs,"value","val")
rhd_inputs[grepl("inc_implied",variable),c("lower","upper"):=val] ## for incidence of HF, don't use uncertainty from this method--we're just varying this in sensitivity analyses
rhd_inputs <- rhd_inputs[order(location_name,variable,sex,age)]
rhd_inputs <- rhd_inputs[!variable %in% c("rhd_hf_prev_rate")] ## drawing fraction instead

## make draws
if (nrow(rhd_inputs[val < lower | val > upper]) > 0) stop("incorrect input to rtri")
rhd_inputs[age==0,c("val","lower","upper"):=0]
rhd_inputs <- rhd_inputs[!grepl("GBDadj",variable)] ## not running this version
rhd_inputs <- rhd_inputs[!grepl("Watkins",variable)] ## not running this version
sims <- copy(rhd_inputs[val > lower & upper > val])
nosims <- copy(rhd_inputs[!(val > lower & upper > val)])
if (nrow(nosims) + nrow(sims) != nrow(rhd_inputs)) stop("issue")

## draws
simlist <- list()
count <- 1
tot <- nrow(sims)
sims[grepl("rhd_hf_prev_frac",variable),seed:=184439]
sims[grepl("cases_severe_persistent_prev_yr_count",variable),seed:=153839]
sims[grepl("cases_mild_persistent_prev_yr_count",variable),seed:=580303]
if (any(is.na(sims$seed))) stop("need seed")

for (r in 1:tot) {
  #cat(paste0(i,j,k,l,m)); flush.console()
  cat(paste0(count," of ",tot," ")); flush.console()

  set.seed(sims[r]$seed)
  simlist[[r]] <- rtri(draws,min=sims[r]$lower,
                         max=sims[r]$upper,
                         mode=sims[r]$val)
  count <- count + 1
}

simlist <- do.call("rbind",simlist)

sims <- cbind(sims,simlist)
rm(simlist)
simlist <- matrix(rep(nosims$val,draws),ncol=draws,byrow=F)
nosims <- cbind(nosims,simlist)
sims[,seed:=NULL]
rhd_inputs <- rbind(sims,nosims,use.names=T)
rm(simlist); rm(sims); rm(nosims); gc()


# rhd_inputs <- merge(rhd_inputs,simlist,by=c("variable","sex","age"),all=T)
rhd_inputs[,c("rat_low","rat_high","val","lower","upper","sex_id"):=NULL]
setnames(rhd_inputs,"variable","measure_name")
rhd_inputs <- melt(rhd_inputs,id.vars=c("location_name","year","pop","age","sex","measure_name"))
setnames(rhd_inputs,c("variable","value"),c("draw","val"))
rhd_inputs[,draw:=as.numeric(gsub("V","",as.character(draw)))]
rhd_inputs <- dcast.data.table(rhd_inputs,location_name+year+sex+age+draw+pop~measure_name,value.var="val")


## add post-surgery starting pop input
postsurg_start <- readRDS(paste0(outdir,"/data/estimated_postsurg_input.RDS"))
postsurg_start <- postsurg_start[draw %in% c(1:draws)]
rhd_inputs <- merge(rhd_inputs,postsurg_start,by=c("location_name","sex","age","draw"),all.x=T)

## make sure there aren't problematic draws
for (i in c("GBD","Soweto","DisModadj")) {
  cat(paste0(i)); flush.console()
  if (nrow(rhd_inputs[age < 95][rhd_inputs[age < 95][[paste0(i,"_inc_implied")]] > 1]) > 10) stop("draws above 1 for implied incidence")
  rhd_inputs[[paste0(i,"_inc_implied")]][rhd_inputs[[paste0(i,"_inc_implied")]] > 1] <- .99
  if (nrow(rhd_inputs[rhd_inputs[[paste0(i,"_inc_implied")]] < 0]) > 0) stop("implied incidence less than 0")
  if (nrow(rhd_inputs[rhd_inputs[[paste0(i,"_cases_mild_persistent_prev_yr_count")]] < 0]) > 0) stop("cases < 0")
  if (nrow(rhd_inputs[rhd_inputs[[paste0(i,"_cases_severe_persistent_prev_yr_count")]] < 0]) > 0) stop("cases < 0")
}
if (nrow(rhd_inputs[rhd_hf_prev_frac < 0 | rhd_hf_prev_frac > 1]) > 0) stop("prev implausible < 0")


###########################################################################
## load parameters about intervention effect sizes--coverage done in separate code
###########################################################################
params <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/intervention_params_uncert.xlsx")))
params <- params[,c(1:9),with=F]
setnames(params,c("intervention","intervention_id","effect_type","effect","lower","upper","uncert_dist","coverage_start","coverage_start_sd"))

## for log-normal ones, calculate the sd
params[uncert_dist == "log-normal",sd:=(log(upper)-log(lower))/(2*1.96)]
params[uncert_dist == "log-normal",point:=log(effect)]
params[uncert_dist == "logit-normal",sd:=(logit(upper)-logit(lower))/(2*1.96)]
params[uncert_dist == "logit-normal",point:=logit(effect)]
params <- params[!is.na(effect)]

set.seed(58392398)
params <- cbind(params,(matrix(rnorm(n=nrow(params)*draws,mean=rep(params$point,draws),sd=rep(params$sd,draws)),nrow=nrow(params))))
setnames(params,c(names(params)[c(1:11)],paste0("effect_draw",c(1:draws))))
params <- params[,c("intervention","intervention_id","effect_type","effect","lower","upper","uncert_dist","point","sd",paste0("effect_draw",c(1:draws))),with=F]

params <- melt(params,id.vars=c("intervention","intervention_id","effect_type","effect","lower","upper","uncert_dist","point","sd"),value.name=c("effect_draw"),variable.name=c("draw"))
if (any(is.na(params$effect_draw))) stop("missing")
params[uncert_dist == "log-normal",effect_draw:=exp(effect_draw)]
params[uncert_dist == "logit-normal",effect_draw:=inv.logit(effect_draw)]
params[,draw:=gsub("effect_draw","",draw)]
params[,draw:=as.numeric(draw)]
params <- params[order(intervention_id,draw)]
if (any(is.na(params$draw))) stop("missing draw")


paramsum <- copy(params)
paramsum <- paramsum[,list(effect_mean=mean(effect_draw),effect_lower=quantile(effect_draw,probs=c(.025)),
                           effect_upper=quantile(effect_draw,probs=c(.975))),
                     by=c("intervention_id","intervention")]
paramsum

## make effect draw consistent (make relative risks reductions instead)
params[intervention_id == 1,effect_draw:=1-effect_draw]
params[intervention_id == 1,intervention:="Treatment of pharyngitis"]
params[intervention_id == 1,effect_type:="Reduction"]
params[intervention_id == 2,effect_draw:=1-effect_draw]
params[intervention_id == 2,effect_type:="Reduction"]
params[intervention_id == 7,effect_draw:=1-effect_draw]
params[intervention_id == 7,effect_type:="Reduction"]

if (any(params$effect_type != "Reduction")) stop("not designed for this")

summary(params$effect_draw)
## we know RRs are mostly estimated with log-normal UI, so log-normal is appropriate, but can sometimes generate draws > 1 or < 0
## if minimal, we will simply cap
## otherwise, re-examine, but overall better to use original distribution estimated
if (nrow(params[effect_draw > 1])/nrow(params) > .01) stop("more than 1% of rows > 1, re-assess distributions")
params[effect_draw > 1,effect_draw:=.99]

if (nrow(params[effect_draw <0])/nrow(params) > .01) stop("more than 1% of rows < 0, re-assess distributions")
params[effect_draw < 0,effect_draw:=.01]

## add cause fraction
gbd[,frac:=`Rheumatic heart disease_Deaths`/`All causes_Deaths`]
setnames(gbd,c("All causes_Deaths","Rheumatic heart disease_Deaths","Rheumatic heart disease_Incidence","Rheumatic heart disease_Prevalence"),
         c("mx_allcauses","mx_rhd","inc_rhd","prev_rhd"))

## rhd_inputs, gbd, and params are the inputs prepped here
rhd_inputs[,pop:=NULL]
gbd <- merge(gbd,rhd_inputs,by=c("location_name","year","sex","age","draw"),all=T)
gbd[,rhd_hf_prev_rate:=rhd_hf_prev_frac*prev_rhd]
gbd[,rhd_hf_prev_frac:=NULL]


## create regional aggregates for if we run on regional level instead
gbd <- merge(gbd,locs[,c("location_name","au_region"),with=F],by="location_name",all.x=T)


regs <- copy(gbd[,list(mx_allcauses=weighted.mean(mx_allcauses,w=pop),mx_rhd=weighted.mean(mx_rhd,w=pop),
                       inc_rhd=weighted.mean(inc_rhd,w=pop),prev_rhd=weighted.mean(prev_rhd,w=pop),
                       fert=weighted.mean(fert,w=pop),DisModadj_cases_mild_persistent_prev_yr_count=sum(DisModadj_cases_mild_persistent_prev_yr_count),
                       DisModadj_cases_severe_persistent_prev_yr_count=sum(DisModadj_cases_severe_persistent_prev_yr_count),
                       DisModadj_inc_implied=mean(DisModadj_inc_implied),GBD_cases_mild_persistent_prev_yr_count=sum(GBD_cases_mild_persistent_prev_yr_count),
                       GBD_cases_severe_persistent_prev_yr_count=sum(GBD_cases_severe_persistent_prev_yr_count),
                       GBD_inc_implied=mean(GBD_inc_implied),Soweto_cases_mild_persistent_prev_yr_count=sum(Soweto_cases_mild_persistent_prev_yr_count),
                       Soweto_cases_severe_persistent_prev_yr_count=sum(Soweto_cases_severe_persistent_prev_yr_count),
                       Soweto_inc_implied=mean(Soweto_inc_implied),rhd_hf_prev_rate=weighted.mean(rhd_hf_prev_rate,w=pop),
                       postsurg_start=sum(postsurg_start),pop=sum(pop)),by=c("au_region","year","sex","age","draw","sex_id")])
regs[,frac:=mx_rhd/mx_allcauses]
regs <- regs[!is.na(au_region)]
setnames(regs,c("au_region"),c("location_name"))
gbd[,au_region:=NULL]

gbd <- rbind(gbd,regs,use.names=T,fill=T)



gbd[,c("sex"):=NULL] ## shrink file size



saveRDS(gbd,paste0(outdir,"/data/collated_epi_demog_model_inputs.RDS")) ## about 191 MB
saveRDS(params,paste0(outdir,"/data/effect_draws.RDS"))