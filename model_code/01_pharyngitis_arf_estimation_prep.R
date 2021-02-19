## Prep pharyngitis/ARF estimation

library(data.table)
library(ggplot2)
library(EnvStats)
library(stringr)
library(gtools)
library(scales)
library(grid)
library(gridExtra)

print(commandArgs()) 

## set directories
if (Sys.info()[1] == 'Windows') {
  rm(list=ls()) ## not clearing workspace on cluster so we can use commandArgs() hack
  codedir <- paste0("[Insert own directory]/rhd_invest_priv/")
  outdir <- paste0("[Insert own directory]/rhd_investment_case/")
  outdirtmp <- outdir ## this is just for storing files on the cluster with many draws, not backed up
  drawnum <- 200
} else {
  codedir <- paste0("[Insert own directory]/rhd_invest_priv/")
  outdir <- paste0("[Insert own directory]/rhd_investment_case/")
  outdirtmp <- paste0("[Insert own directory]")
  drawnum <- 1000
}
## load functions
source(paste0(codedir,"/functions/swap_location_names.R"))
source(paste0(codedir,"/functions/map_ids_names.R"))



## option for if running this code as an individual part for troubleshooting
## rather than as part of whole process
manual <- F
calibrating <- F## to calibrate scalars for consistency with GBD RHD incidence
###########################
## Options for what to run
if (manual) {
  run_num <- 30
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
print(run_num)
if (Sys.info()[1] == 'Windows' & drawnum != specs$drawnum) specs$drawnum <- drawnum
drawnum <- specs$drawnum
###########################
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



#########################################
## load main inputs
#########################################
## epi
epi <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_inc_prev_death_inputs.RDS"))
epi <- map_to_names(epi,codedir=codedir,keep_ids=T,gbd=2017)

epireg <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_inc_prev_death_inputs_region.RDS"))
epireg <- map_to_names(epireg,codedir=codedir,keep_ids=T,gbd=2017)
epi[,c("lower","upper","loc_level","location_id"):=NULL]

epi <- rbind(epi,epireg)
epi <- epi[location_name %in% locs]
rm(epireg); gc()

## effect sizes
eff <- readRDS(paste0(outdir,"/data/effect_draws.RDS"))
eff <- eff[draw %in% c(1:drawnum)]

## starting coverage
cov <- readRDS(paste0(outdir,"/data/starting_coverage.RDS"))
cov <- cov[draw %in% c(1:drawnum)]

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
cov[,covdiff:=max(covdiff,na.rm=T),by=c("intervention_id","draw")] ## max just to get AU value here for every loc, since it's the only one being given values
cov[,ending_coverage:=covdiff+starting_coverage]
## for scenarios with 100% coverage of an intervention in the AU, each country needs to be 100%
cov[coverage_end_au==1,ending_coverage:=1]

## if the starting coverage is the same as the ending coverage input, make the draws not change
cov[abs(coverage_start_au-coverage_end_au)<.00001,ending_coverage:=starting_coverage]

if (any(cov$ending_coverage < 0 | cov$ending_coverage > 1)) stop("bad end coverage")
if (nrow(cov[starting_coverage > ending_coverage]) > 0) stop("coverage end < coverage_start")

cov <- cov[location_name %in% c(locs)]
cov <- cov[draw %in% c(1:drawnum)]



#########################################
## these values calibrated to create incidence consistent with AU incidence by running this code for the AU given the particular
## parameters in the inputs sheet, we do not have any reliable information about how these parameters may vary across countries, so
## calibrated overall for AU
phar_arf_scalar <- specs$phar_arf_scalar
rhd_transition_scalar <- specs$rhd_transition_scalar
additional_arf_chance_scalar <- specs$additional_arf_chance_scalar
additional_arf_chance_scalar2 <- specs$additional_arf_chance_scalar2

## if calibrating, can uncomment and experiment with values
## bumping these up makes ratio at end go down
## for base case, using gamma dist for arf risk 1.0282 and 0.7405
# phar_arf_scalar <- 1.0192
# rhd_transition_scalar <- .744 ## up makes male ratio at end go down
# additional_arf_chance_scalar <- 1.237
# additional_arf_chance_scalar2 <- 1


## load AU pops, creating age-specific pharyngitis values based on weighted averages
## not assuming any regional differences here
pop <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_pop.RDS"))
pop <- pop[location_name=="African Union"]
pop <- pop[year==2017]
pop <- pop[,list(pop=sum(pop)),by=c("location_name","year","age")]


## calculate the incidence of the first ARF episode through the GAS pharyngitis pathway
inc_gas_pharyngitis <- data.table(data.frame(age=c(0:95),pop=pop$pop))
rm(pop)
inc_gas_pharyngitis[age==0,gas_prev:=0]
inc_gas_pharyngitis[age==0,phar_peryear:=0] 
inc_gas_pharyngitis[age %in% c(1:4),gas_prev:=specs$gas_prev*seq(from=.5,to=1,by=.5/4)[c(1:4)]]
inc_gas_pharyngitis[age %in% c(5:9),gas_prev:=specs$gas_prev]
inc_gas_pharyngitis[age %in% c(10:15),gas_prev:=specs$gas_prev*seq(from=1,to=.8,by=-1*(1-.8)/5)[c(1:6)]]
inc_gas_pharyngitis[age %in% c(15:20),gas_prev:=specs$gas_prev*seq(from=.8,to=.5,by=-1*(.8-.5)/5)[c(1:6)]]
inc_gas_pharyngitis[age %in% c(20:45),gas_prev:=specs$gas_prev*seq(from=.5,to=.2,by=-1*(.5-.2)/25)[c(1:26)]]
inc_gas_pharyngitis[age %in% c(45:95),gas_prev:=specs$gas_prev*.2]
inc_gas_pharyngitis[age %in% c(1:4),phar_peryear:=as.double(specs$phar_peryear)]
inc_gas_pharyngitis[age %in% c(5:24),phar_peryear:=specs$phar_peryear*seq(from=.85,to=.75,by=-1*(.85-.75)/20)[c(1:20)]] 
inc_gas_pharyngitis[age %in% c(25:70),phar_peryear:=specs$phar_peryear*.75*seq(from=1,to=.66,by=-1*(1-.66)/(70-25))] 
## find scalar to make average phar per year in 5-15 equal to specs
## find scalar to make average gas prev in 5-15 equal to specs
sc <- copy(inc_gas_pharyngitis[age %in% c(5:15)])
sc <- sc[,list(sc_phar_peryear=weighted.mean(phar_peryear,w=pop),sc_gas_prev=weighted.mean(gas_prev,w=phar_peryear*pop)),]
sc[,sc_phar_peryear:=specs$phar_peryear/sc_phar_peryear]
sc[,sc_gas_prev:=specs$gas_prev/sc_gas_prev]
inc_gas_pharyngitis[,phar_peryear:=phar_peryear*sc$sc_phar_peryear]
inc_gas_pharyngitis[,gas_prev:=gas_prev*sc$sc_gas_prev]

## now scalars from calibrating to GBD incidence
inc_gas_pharyngitis[,gas_prev:=gas_prev*phar_arf_scalar]
inc_gas_pharyngitis[,phar_peryear:=phar_peryear*phar_arf_scalar]
inc_gas_pharyngitis[,inc_gas_pharyngitis:=phar_peryear*gas_prev] 

## expand to make draws (not incorporating uncertainty about the above parameters--calibrating to GBD incidence, also varying in
## specific sensitivity analyses)
expander <- data.table(expand.grid(age=c(0:95),draw=c(1:drawnum),year=c(base_year:(base_year+proj_yrs-1))))
inc_gas_pharyngitis <- merge(inc_gas_pharyngitis,expander,by=c("age"),all=T)


set.seed(8939727)
arf_chance <- exp(rnorm(n=drawnum,mean=log(.003),sd=(log(.05)-log(.001))/(2*1.96))) ## (.001-.05), arf given GAS+
arf_chance <- rgamma(n=drawnum,shape=4,rate=(4-1)/.003)

hist(arf_chance)
mean(arf_chance)
quantile(arf_chance,probs=.025)
quantile(arf_chance,probs=.975)

arf_chance_recur <- inv.logit(rnorm(n=drawnum,mean=logit(.5),sd=(logit(.75)-logit(.25))/(2*1.96))) 
## these are adjusted in our calibration to GBD RHD incidence based on the results of the model
arf_chance <- arf_chance*phar_arf_scalar*additional_arf_chance_scalar*additional_arf_chance_scalar2
arf_chance_recur <- arf_chance_recur*phar_arf_scalar*additional_arf_chance_scalar
if (any(arf_chance > .999)) stop("issue")
if (length(arf_chance_recur[arf_chance_recur > .999]) > .1*length(arf_chance)) stop("issue")
arf_chance_recur[arf_chance_recur > .98] <- .98

## merge on arf_chance
inc_gas_pharyngitis <- inc_gas_pharyngitis[order(year,age,draw)]
reprows <- nrow(inc_gas_pharyngitis)/drawnum
inc_gas_pharyngitis[,arf_chance:=rep(arf_chance,reprows)] ## conditional on untreated GAS pharyngitis
## given low reported ARF under age 5, making adjustment to the chance
inc_gas_pharyngitis[age %in% c(1:4),arf_chance:=arf_chance*.75]
inc_gas_pharyngitis[,arf_chance_recur:=rep(arf_chance_recur,reprows)] ## among people who have had ARF before and conditional on untreated GAS pharyngitis
inc_gas_pharyngitis[age %in% c(1:4),arf_chance_recur:=arf_chance_recur*.75]
inc_gas_pharyngitis[age>=45,c("arf_chance","arf_chance_recur"):=0] ## assuming people not at risk at age 45

arf_chance <- arf_chance_recur <- NULL ## make sure we're using the one in the dataset

## create separate male/female estimates
inc_gas_pharyngitis[,sex:="male"]
inc2 <- copy(inc_gas_pharyngitis)
inc2[,sex:="female"]
inc_gas_pharyngitis <- rbind(inc_gas_pharyngitis,inc2)
reprows <- nrow(inc_gas_pharyngitis)/drawnum
rm(inc2); gc()


## effect
## primary
inc_gas_pharyngitis <- merge(inc_gas_pharyngitis,copy(eff[intervention_id == 1,c("draw","effect_draw"),with=F]),by=c("draw"),all.x=T)
setnames(inc_gas_pharyngitis,"effect_draw","effect_draw_primary")
## secondary
inc_gas_pharyngitis <- merge(inc_gas_pharyngitis,copy(eff[intervention_id == 2,c("draw","effect_draw"),with=F]),by=c("draw"),all.x=T)
setnames(inc_gas_pharyngitis,"effect_draw","effect_draw_secondary")


## coverage
## before merging on, so that merge is clean, make inc_gas_pharyngitis for each location
inc_gas_pharyngitis <- rbindlist(lapply(unique(cov$location_name),FUN=function(x){
  out <- copy(inc_gas_pharyngitis)
  out[,location_name:=x]
}))
## primary
inc_gas_pharyngitis <- merge(inc_gas_pharyngitis,copy(cov[intervention_id == 1,c("location_name","draw","starting_coverage","ending_coverage"),with=F]),by=c("location_name","draw"),all.x=T)
setnames(inc_gas_pharyngitis,c("starting_coverage","ending_coverage"),c("coverage_start_draw_primary","coverage_end_draw_primary"))
inc_gas_pharyngitis[age < 5 | age > 15,coverage_end_draw_primary:=coverage_start_draw_primary] ## not scaling up coverage in other ages as part of this effort

## secondary
inc_gas_pharyngitis <- merge(inc_gas_pharyngitis,copy(cov[intervention_id == 2,c("location_name","draw","starting_coverage","ending_coverage"),with=F]),by=c("location_name","draw"),all.x=T)
setnames(inc_gas_pharyngitis,c("starting_coverage","ending_coverage"),c("coverage_start_draw_secondary","coverage_end_draw_secondary"))


## sensitivity and specificity of decision rule to treat pharyngitis
dec_sensitivity <- .92
dec_specificity <- .38

## calculate the incidence of first ARF after a case of GAS pharyngitis using incidence of GAS pharyngitis, coverage of primary prevention, sensitivity of decision rule, chance of ARF after untreated, and chance of ARF after treated
inc_gas_pharyngitis[,inc_first_arf_phar:=inc_gas_pharyngitis*(1-coverage_start_draw_primary+coverage_start_draw_primary*(1-dec_sensitivity))*arf_chance+inc_gas_pharyngitis*coverage_start_draw_primary*dec_sensitivity*(1-effect_draw_primary)*arf_chance]

## find incidence of ARF from recurrence (among people who have had initial, separately for those covered by secondary and not covered by secondary prevention)
## then, use coverage of secondary prevention to get total incidence among people who have had initial ARF
inc_gas_pharyngitis[,inc_recur_arf_phar_nosp:=inc_gas_pharyngitis*(1-coverage_start_draw_primary+coverage_start_draw_primary*(1-dec_sensitivity))*arf_chance_recur+inc_gas_pharyngitis*coverage_start_draw_primary*dec_sensitivity*(1-effect_draw_primary)*arf_chance_recur]
inc_gas_pharyngitis[,inc_recur_arf_phar_sp:=(1-effect_draw_secondary)*(inc_gas_pharyngitis*(1-coverage_start_draw_primary+coverage_start_draw_primary*(1-dec_sensitivity))*arf_chance_recur+inc_gas_pharyngitis*coverage_start_draw_primary*dec_sensitivity*(1-effect_draw_primary)*arf_chance_recur)]
inc_gas_pharyngitis[,inc_recur_arf_phar:=coverage_start_draw_secondary*inc_recur_arf_phar_sp+(1-coverage_start_draw_secondary)*inc_recur_arf_phar_nosp]

## incorporate assumed percent of ARF coming after pharyngitis to get total incidene of first and second ARF (second is among people who have had first)
inc_gas_pharyngitis[,pct_phar:=specs$gas_arf_assumption]
inc_gas_pharyngitis[,inc_first_arf:=1/pct_phar*inc_first_arf_phar]
inc_gas_pharyngitis[,inc_recur_arf:=1/pct_phar*inc_recur_arf_phar]


# 
# ## make plots for appendix -- 
# if (manual) {
# 
#   library(grid)
#   library(gridExtra)
# 
#   pdf(paste0(outdir,"/results/figures/appendix/phar_gas_inputs_age.pdf"),width=8,height=10)
# 
#   testplot <- copy(inc_gas_pharyngitis)
#   testplot <- testplot[,list(phar_peryear=mean(phar_peryear),gas_prev=mean(gas_prev),inc_gas_pharyngitis=mean(inc_gas_pharyngitis),
#                              inc_first_arf_mean=mean(inc_first_arf,na.rm=T),
#                              inc_first_arf_lower=quantile(inc_first_arf,probs=c(.025),na.rm=T),inc_first_arf_upper=quantile(inc_first_arf,probs=c(.975),na.rm=T),
#                              inc_recur_arf_mean=mean(inc_recur_arf,na.rm=T),
#                              inc_recur_arf_lower=quantile(inc_recur_arf,probs=c(.025),na.rm=T),inc_recur_arf_upper=quantile(inc_recur_arf,probs=c(.975),na.rm=T),
#                              arf_chance_mean=mean(arf_chance,na.rm=T),
#                              arf_chance_lower=quantile(arf_chance,probs=c(.025),na.rm=T),arf_chance_upper=quantile(arf_chance,probs=c(.975),na.rm=T)),by=c("age","year","sex")]
# 
#   gg1 <- ggplot(data=testplot[age > 0 & year == 2017],aes(x=age,y=phar_peryear)) + geom_line(size=1.1) + theme_bw() +
#     scale_y_continuous("Pharyngitis cases per year",limits=c(0,3.25)) +
#     scale_x_continuous("Age",limits=c(0,44))
#   print(gg1)
# 
# 
#   gg2 <- ggplot(data=testplot[age > 0 & year == 2017],aes(x=age,y=gas_prev*100)) + geom_line(size=1.1) + theme_bw() +
#     scale_y_continuous("Prevalence of GAS (%)",limits=c(0,20)) +
#     scale_x_continuous("Age",limits=c(0,44))
#   print(gg2)
# 
# 
#   gg3 <- ggplot(data=testplot[age > 0 & year == 2017],aes(x=age,y=inc_gas_pharyngitis)) + geom_line(size=1.1) + theme_bw() +
#     scale_y_continuous("GAS Pharyngitis cases per year",limits=c(0,0.5)) +
#     scale_x_continuous("Age",limits=c(0,44))
#   print(gg3)
# 
#   gg4 <- ggplot(data=testplot[age > 0 & year == 2017],aes(x=age,y=arf_chance_mean*100)) + geom_line(size=1.1) +
#     geom_ribbon(aes(x=age,ymax=arf_chance_upper*100,ymin=arf_chance_lower*100),alpha = 0.3) + theme_bw() +
#     scale_y_continuous("Chance of ARF after\nGAS pharyngitis (%)",limits=c(0,5)) +
#     scale_x_continuous("Age",limits=c(0,44))+ theme(legend.position="bottom")
#   print(gg4)
# 
#   gg5 <- ggplot(data=testplot[age > 0 & year == 2017],aes(x=age,y=inc_first_arf_mean*1000)) + geom_line(size=1.1) +
#     geom_ribbon(aes(x=age,ymax=inc_first_arf_upper*1000,ymin=inc_first_arf_lower*1000),alpha = 0.3) +
#     theme_bw() + scale_y_continuous("Incidence of first ARF (per 1,000)",limits=c(0,7.5)) +
#     scale_x_continuous("Age",limits=c(0,44)) + theme(legend.position="bottom")
#   print(gg5)
# 
#   gg6 <- ggplot(data=testplot[age > 0 & year == 2017],aes(x=age,y=inc_recur_arf_mean*1000)) + geom_line(size=1.1) +
#     geom_ribbon(aes(x=age,ymax=inc_recur_arf_upper*1000,ymin=inc_recur_arf_lower*1000),alpha = 0.3) +
#     theme_bw() + scale_y_continuous("Incidence of recurrent ARF\namong people with\nhistory of ARF (per 1,000)",limits=c(0,400)) +
#     scale_x_continuous("Age",limits=c(0,44)) + theme(legend.position="bottom")
#   print(gg6)
# 
#   grid.arrange(grobs=list(gg1,gg2,gg3,gg4,gg5,gg6),ncol=2)
# 
# 
#   dev.off()
# 
# }


## get projected coverage levels based on scale-up pattern specified
total_scaleup_rounds <- (base_year+(scale_to-base_year+1)-1) - int_start_year + 1
inc_gas_pharyngitis[year >= int_start_year,coverage_level_primary:=(coverage_end_draw_primary-coverage_start_draw_primary)/(total_scaleup_rounds)*(year-int_start_year+1)+coverage_start_draw_primary]
inc_gas_pharyngitis[year < int_start_year,coverage_level_primary:=coverage_start_draw_primary]
inc_gas_pharyngitis[year > scale_to,coverage_level_primary:=coverage_end_draw_primary]
if (specs$end_scaleup != "No") inc_gas_pharyngitis[year >= as.numeric(specs$end_scaleup),coverage_level_primary:=coverage_start_draw_primary]

inc_gas_pharyngitis[year >= int_start_year,coverage_level_secondary:=(coverage_end_draw_secondary-coverage_start_draw_secondary)/(total_scaleup_rounds)*(year-int_start_year+1)+coverage_start_draw_secondary]
inc_gas_pharyngitis[year < int_start_year,coverage_level_secondary:=coverage_start_draw_secondary]
inc_gas_pharyngitis[year > scale_to,coverage_level_secondary:=coverage_end_draw_secondary]
if (specs$end_scaleup != "No") inc_gas_pharyngitis[year >= as.numeric(specs$end_scaleup),coverage_level_secondary:=coverage_start_draw_secondary]

## calculate impact scalar for primary and secondary prevention
inc_gas_pharyngitis[,primary_impact_scalar:=1-(effect_draw_primary*(coverage_level_primary-coverage_start_draw_primary)*dec_sensitivity)/(1-effect_draw_primary*coverage_start_draw_primary*dec_sensitivity)]
inc_gas_pharyngitis[,secondary_impact_scalar:=1-(effect_draw_secondary*(coverage_level_secondary-coverage_start_draw_secondary))/(1-effect_draw_secondary*coverage_start_draw_secondary)]

## now get the effect on first ARF overall based on the % from pharyngitis
inc_gas_pharyngitis[,inc_first_arf_int:=inc_first_arf*pct_phar*primary_impact_scalar+inc_first_arf*(1-pct_phar)]
inc_gas_pharyngitis[,inc_recur_arf_int:=secondary_impact_scalar*(inc_recur_arf*pct_phar*primary_impact_scalar+inc_recur_arf*(1-pct_phar))]
inc_gas_pharyngitis[,overall_impact_first_arf_scalar:=inc_first_arf_int/inc_first_arf]
inc_gas_pharyngitis[is.na(overall_impact_first_arf_scalar) & inc_first_arf == 0,overall_impact_first_arf_scalar:=1]
inc_gas_pharyngitis[,overall_impact_recur_arf_scalar:=inc_recur_arf_int/inc_recur_arf]
inc_gas_pharyngitis[is.na(overall_impact_recur_arf_scalar) & inc_recur_arf == 0,overall_impact_recur_arf_scalar:=1]


## now, there is theoretically some population that has already had an ARF episode but doesn't yet have RHD
## to approximate that population as a starting point, we run a simple model from 2000 to 2017 
## while doing this, compare estimates of RHD incidence to GBD to figure out how this model compares and calibrate
## load populations
## load pop
p <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_pop.RDS"))
p <- p[location_name %in% c(locs)]
p <- p[year %in% c(2000:2017)]
## need 200 draws of starting pop (even if we aren't using uncertainty about starting pop) in order to propagate the effects from our uncertainty in projections
p <- rbindlist(lapply(as.list(c(1:drawnum)),FUN=function(x) {
  out <- copy(p)
  out[,draw:=x]
}))
p <- p[,c("location_name","year","sex","age","draw","pop"),with=F]
p <- p[order(location_name,year,sex,age,draw)]

## now we have pops, merge on the ARF info
p <- merge(p[,c("location_name","sex","age","year","draw","pop"),with=F],copy(inc_gas_pharyngitis[year==2017,c("location_name","age","draw","sex","inc_first_arf","inc_recur_arf"),with=F]),by=c("location_name","age","sex","draw"),all.x=T)

## now, simulate people with ARF
## we have incidence of first ARF, the other parameters are probability of death from ARF, background probability of death, 
## probability of transition to RHD with ARF (first case), probability of recurrent case after in remission, probability of RHD with recurrence,
## probability of death with recurrence
set.seed(80984502)
death_first_arf <- exp(rnorm(n=drawnum,mean=log(.01),sd=(log(.02)-log(.005))/(2*1.96)))
death_recur_arf <- death_first_arf*exp(rnorm(n=drawnum,mean=log(2),sd=.2))
rhd_transition_female <- exp(rnorm(n=drawnum,mean=log(.36),sd=(log(.4788)-log(.2412))/(2*1.96)))
rhd_transition_recur_female <- inv.logit(rnorm(n=drawnum,mean=logit(.36*2),sd=(logit(.4788*2)-logit(.2412*2))/(2*1.96)))

rhd_transition_male <- rhd_transition_female*rhd_transition_scalar
rhd_transition_recur_male <- rhd_transition_recur_female*rhd_transition_scalar

## need background prob of death, load 2000-2017 here since we need longitudinal
epi <- dcast.data.table(epi,sex_id+age_group_id+year+cause_id+location_name+sex+age_group_name+cause_name~measure_name,value.var="val")
## extend rates to single-year age groups
epi <- epi[!age_group_id %in% c(2,3,4,22,27)]
epi[age_group_id == 28,age_group_name:="0"]
epi[age_group_id==28,age_group_id:=1]
epi <- rbind(epi,epi,epi,epi,epi)
epi <- epi[order(location_name,sex,cause_name,age_group_id,year)]
epi[, id := seq_len(.N), by = c("location_name","year","sex","cause_name","age_group_id")]
epi <- epi[!(age_group_name=="0" & id > 1) & !(age_group_name=="1 to 4" & id > 4)]
epi[,age:=id-1+as.numeric(substr(age_group_name,1,2))]
epi <- epi[!age %in% c(96:99)]
epi <- dcast.data.table(epi,sex_id+year+location_name+sex+age~cause_name,value.var=c("Deaths","Incidence","Prevalence"))
epi[,qx:=1-exp(-1*`Deaths_All causes`)]
epi[,cd_qx:=(1-exp(-1*(`Deaths_All causes`)))*(`Deaths_All causes`-`Deaths_Rheumatic heart disease`)/`Deaths_All causes`]

p <- p[order(location_name,sex,age,year,draw)]
p[,death_first_arf:=rep(death_first_arf,nrow(p)/drawnum)]
p[,death_recur_arf:=rep(death_recur_arf,nrow(p)/drawnum)]
p[sex=="male",rhd_transition:=rep(rhd_transition_male,nrow(p)/drawnum/2)]
p[sex=="male",rhd_transition_recur:=rep(rhd_transition_recur_male,nrow(p)/drawnum/2)]
p[sex=="female",rhd_transition:=rep(rhd_transition_female,nrow(p)/drawnum/2)]
p[sex=="female",rhd_transition_recur:=rep(rhd_transition_recur_female,nrow(p)/drawnum/2)]


p <- merge(p,epi[,c("location_name","year","age","sex","cd_qx"),with=F],by=c("location_name","year","age","sex"),all.x=T)

## run simulation from 2000 to 2017 to track people with history of ARF in past 10 years
old <- list()
for (i in c(2000:2017)) {
  
  cat(paste0("running year ",i)); flush.console()
  
  tmp <- copy(p[year==i])
  
  if (i == 2000) {
    tmp[age < 45,first_arf:=inc_first_arf*pop]
    tmp[,to_rhd_first:=rhd_transition*first_arf]
    tmp[,to_death_first:=first_arf*(cd_qx+death_first_arf)]
    tmp[,to_remission_first:=first_arf-to_death_first-to_rhd_first]
    tmp[is.na(to_remission_first),to_remission_first:=0]
    tmp[[paste0("hist_arf_",i)]] <- tmp$to_remission_first
    old[[paste0(i)]] <- copy(tmp[,c("location_name","year","age","sex","draw","first_arf","to_rhd_first","to_death_first","to_remission_first",paste0("hist_arf_",i)),with=F])
    
  } else {
    
    tocopy <- copy(old[[paste0(i-1)]])
    tocopy[age < 95,age:=age+1]
    tocopy <- tocopy[,lapply(.SD,sum),by=c("location_name","age","sex","draw"),.SDcols=c(names(tocopy)[grepl("hist_arf",names(tocopy))])]
    tmp <- merge(tmp,tocopy,by=c("location_name","age","sex","draw"),all.x=T)
    
    ## determine which are people with history still in the past 10 years or are < 20 with history
    ## if more than 9 years have passed since 2000
    if ((i-2000) > 9) {
      ## among those with a case from 2000 through ten years prior to current year,
      ## if they are greater than age 20, then no longer count them (those with 10 years + history under 20 still count as at risk)
      for (j in 2000:(i-10)) {
        tmp[[paste0("hist_arf_",j)]][tmp$age > 20] <- 0 ## if it's been more than 10 years, then make everyone older than 20 come off secondary prevention
      }
    }
    
    ## get total considered at risk
    tmp[,total_10yr_history:=rowSums(.SD,na.rm=T),.SDcols = c(names(tmp)[grepl("hist_arf",names(tmp))])]
    
    ## first arf can only happen among those without history
    tmp[age < 45,first_arf:=inc_first_arf*(pop-total_10yr_history)]
    ## subsequent happens among those with history
    tmp[age < 45,recur_arf:=inc_recur_arf*(total_10yr_history)]
    tmp[,to_rhd_first:=rhd_transition*first_arf]
    tmp[,to_rhd_recur:=rhd_transition_recur*recur_arf]
    tmp[,to_death_first:=first_arf*(cd_qx+death_first_arf)]
    tmp[,to_death_recur:=recur_arf*(cd_qx+death_recur_arf)]
    tmp[,paste0("hist_arf_",i):=first_arf-to_death_first-to_rhd_first]
    tmp[[paste0("hist_arf_",i)]][is.na(tmp[[paste0("hist_arf_",i)]])] <- 0
    if (any(tmp[[paste0("hist_arf_",i)]] < 0)) stop("negatives issue")
    ## the number with a history of ARF who don't have RHD is the history from previous year, plus those who have a new ARF and survive/don't transition to RHD,
    ## minus (1) those who have recurrence and die, (2) those who have recurrence and transition to RHD, (3) those who don't have recurrence and die of background mortality
    ## the rest return to hist_arf by default here
    ## now, for all of the older years, we need to adjust for those lost to death and RHD from recurrence as well as normal mortality
    tmp[,frac_lost_death_recur:=to_death_recur/total_10yr_history]
    tmp[,frac_lost_rhd_recur:=to_rhd_recur/total_10yr_history]
    tmp[,frac_lost_normal_death:=cd_qx*(total_10yr_history-recur_arf)/total_10yr_history]
    tmp[,total_frac_lost:=frac_lost_death_recur+frac_lost_rhd_recur+frac_lost_normal_death]
    tmp[total_10yr_history==0 & is.na(total_frac_lost),total_frac_lost:=0]
    summary(tmp$total_frac_lost)
    if (any(tmp$total_frac_lost[!is.na(tmp$total_frac_lost)] > 1)) stop("impossible")
    
    vars_toadj <- names(tmp)[grepl("hist_arf",names(tmp)) & !names(tmp) %in% paste0("hist_arf_",i)]
    for (j in vars_toadj) {
      ## subtract out those dying from recurrence, those lost to RHD from recurrence, and those without recurrence but dying from other causes
      tmp[[j]] <- tmp[[j]]-tmp[[j]]*(tmp$total_frac_lost)
    }
    
    old[[paste0(i)]] <- copy(tmp[,c("location_name","year","age","sex","draw","first_arf","to_rhd_first","to_death_first",
                                    "to_rhd_recur","to_death_recur",names(tmp)[grepl("hist_arf",names(tmp))]),with=F])
    
  }
  
}

old <- rbindlist(old,fill=T,use.names = T)
old[,to_rhd:=to_rhd_first+to_rhd_recur]
old[,total_10yr_history:=rowSums(.SD,na.rm=T),.SDcols = c(names(tmp)[grepl("hist_arf",names(tmp))])]

rm(tmp); gc()
if (!calibrating) rm(epi); gc()

old <- merge(old,p[,c("location_name","year","age","sex","draw","pop"),with=F],by=c("location_name","year","age","sex","draw"),all.x=T)

## then we can run a GAS + ARF model forward that uses incidence of first ARF, keeps tracking this population that is "recurring" 
## and also creates estimates of RHD incidence--we can get relative reduction based on scale-up or not of interventions
## to apply to the full model that just starts with RHD incidence--this occurs in the 02 modeling code

## save the projected info for running things
if (Sys.info()[1]=="Windows") {
  saveRDS(inc_gas_pharyngitis,paste0(outdirtmp,"/results/model_run_results/",run_num,"/projected_phar_arf_params.rds"))
  saveRDS(old[year==2016],paste0(outdirtmp,"/results/model_run_results/",run_num,"/arf_history.rds"))
} else {
  saveRDS(inc_gas_pharyngitis,paste0(outdirtmp,"/results/model_run_results/",run_num,"/projected_phar_arf_params_",locsshort,".rds"))
  saveRDS(old[year==2016],paste0(outdirtmp,"/results/model_run_results/",run_num,"/arf_history_",locsshort,".rds"))
}


if (calibrating) {
  
    
  ## now, try to compare the RHD incidence estimate to that from GBD in 2017
  phar_arf <- readRDS(paste0(outdirtmp,"/results/model_run_results/",run_num,"/projected_phar_arf_params.rds"))
  hist_arf <- readRDS(paste0(outdirtmp,"/results/model_run_results/",run_num,"/arf_history.rds"))
  phar_arf <- phar_arf[location_name=="African Union"]
  hist_arf <- hist_arf[location_name=="African Union"]
  
  ## coverage level should go up in the year that's specified as the intervention scale-up start year
  total_rounds <- proj_yrs
  #total_scaleup_rounds <- total_rounds - (int_start_year-base_year)
  total_scaleup_rounds <- (base_year+(scale_to-base_year+1)-1) - int_start_year + 1
  
  round <- 1
  if (round > (int_start_year-base_year)) cov[,coverage_level:=(ending_coverage-starting_coverage)/(total_scaleup_rounds)*(round-(int_start_year-base_year))+starting_coverage]
  if (round <= (int_start_year-base_year)) cov[,coverage_level:=starting_coverage]
  
  
  hist_arf <- hist_arf[order(sex,age,draw)]
  hist_arf <- hist_arf[,c("sex","age","draw",paste0("hist_arf_",c((base_year-17):(base_year-1)))),with=F]
  hist_arf[,total_10yr_history:=rowSums(.SD,na.rm=T),.SDcols = c(names(hist_arf)[grepl("hist_arf",names(hist_arf))])]
  hist_arf_int <- copy(hist_arf) ## since intervention hasn't started yet
  repnum <- nrow(hist_arf)/drawnum
  cov <- cov[order(intervention_id,draw)]
  
  ## those receiving secondary prevention
  for (j in c(names(hist_arf)[grepl("hist_arf",names(hist_arf))],"total_10yr_history")) {
    hist_arf[,paste0(j,"_sp"):=hist_arf[[paste0(j)]]*rep(cov[intervention_id == 2]$starting_coverage,repnum)]
    hist_arf_int[,paste0(j,"_sp"):=hist_arf_int[[paste0(j)]]*rep(cov[intervention_id == 2]$starting_coverage,repnum)]
  }
  setnames(hist_arf_int,c("sex","age","draw",paste0(names(hist_arf_int)[4:ncol(hist_arf_int)],"_int")))
  hist_arf <- merge(hist_arf,hist_arf_int,by=c("sex","age","draw"))
  
  ## implement primordial prevention assumption
  ## primordial assumed (find annual rate of change based on the specified change between 2017 and 2030 given in specs)
  primord_aroc <- (log(1)-log(1-specs$gas_arf_primordial))/13
  primord_per <- exp(log(1)-primord_aroc*(round-1))
  
  ## now data on people with history of ARF in last 10 years are loaded, including those on secondary prevention, do simulation of pharyngitis for this year
  ## use pharyngitis incidence from phar_arf, use impact calculation given scale-up
  phar <- copy(phar_arf[year==base_year+round-1,c("age","sex","draw","phar_peryear","inc_gas_pharyngitis","inc_first_arf","inc_first_arf_int","inc_recur_arf","inc_recur_arf_int","pct_phar","inc_recur_arf_phar_nosp",
                                                  "coverage_start_draw_primary","coverage_level_primary","coverage_start_draw_secondary","coverage_level_secondary","effect_draw_secondary"),with=F])
  phar <- merge(phar,p[year==base_year+round-1,c("sex","age","draw","pop"),with=F],by=c("age","sex","draw"),all=T)
  phar[,pop_int:=pop]
  phar <- phar[order(sex,age,draw)]
  phar[,phar_peryear:=phar_peryear*primord_per^(1/3)]
  phar[,inc_gas_pharyngitis:=inc_gas_pharyngitis*primord_per^(2/3)] ## assuming phar per year and % gas decline (though not sure that % gas would...)
  phar[,inc_first_arf:=inc_first_arf*primord_per] ## this assumes effect from phar per year and % gas and arf chance
  phar[,inc_first_arf_int:=inc_first_arf_int*primord_per]
  ## first, calculate number of pharyngitis cases seen by health system
  phar[,phar_seen:=phar_peryear*pop*coverage_start_draw_primary]
  phar[,phar_seen_int:=phar_peryear*pop_int*coverage_level_primary]
  phar[,gas_phar_seen:=inc_gas_pharyngitis*pop*coverage_start_draw_primary]
  phar[,gas_phar_seen_int:=inc_gas_pharyngitis*pop_int*coverage_level_primary] ## the coverage_level_primary should be the same as the start draw for certain ages from previous code
  ## then calculate the number of pharyngitis cases treated
  dec_sensitivity <- .92
  dec_specificity <- .38
  phar[,phar_treated:=gas_phar_seen*dec_sensitivity+(phar_seen-gas_phar_seen)*(1-dec_specificity)]
  phar[,phar_treated_int:=gas_phar_seen_int*dec_sensitivity+(phar_seen_int-gas_phar_seen_int)*(1-dec_specificity)]

  
  set.seed(80984502)
  death_first_arf <- exp(rnorm(n=drawnum,mean=log(.01),sd=(log(.02)-log(.005))/(2*1.96)))
  death_recur_arf <- death_first_arf*exp(rnorm(n=drawnum,mean=log(2),sd=.2))
  rhd_transition_female <- exp(rnorm(n=drawnum,mean=log(.36),sd=(log(.4788)-log(.2412))/(2*1.96)))
  rhd_transition_recur_female <- exp(rnorm(n=drawnum,mean=log(.36*2),sd=(log(.4788*2)-log(.2412*2))/(2*1.96)))
  
  rhd_transition_male <- rhd_transition_female*rhd_transition_scalar
  rhd_transition_recur_male <- rhd_transition_recur_female*rhd_transition_scalar
  
  phar <- phar[order(sex,age,draw)]
  phar[,death_first_arf:=rep(death_first_arf,nrow(phar)/drawnum)]
  phar[,death_recur_arf:=rep(death_recur_arf,nrow(phar)/drawnum)]
  phar[sex=="male",rhd_transition:=rep(rhd_transition_male,nrow(phar)/drawnum/2)]
  phar[sex=="male",rhd_transition_recur:=rep(rhd_transition_recur_male,nrow(phar)/drawnum/2)]
  phar[sex=="female",rhd_transition:=rep(rhd_transition_female,nrow(phar)/drawnum/2)]
  phar[sex=="female",rhd_transition_recur:=rep(rhd_transition_recur_female,nrow(phar)/drawnum/2)]
  
  ## now for recurrence, use history of ARF data (hist_arf and hist_arf_int)
  hist_arf[age < 95,age:=age+1]
  hist_arf <- hist_arf[,lapply(.SD,sum),by=c("sex","age","draw"),.SDcols=c(names(hist_arf)[grepl("hist_arf",names(hist_arf))])]
  ## determine which are people with history still in the past 10 years-- turn off in the 10th year, so if i minus old year is >= 10
  for (j in 2000:(base_year+round-1-10)) {
    hist_arf[[paste0("hist_arf_",j)]][hist_arf$age > 20] <- 0 ## if it's been more than 10 years, then make everyone older than 20 come off secondary prevention
    hist_arf[[paste0("hist_arf_",j,"_sp")]][hist_arf$age > 20] <- 0 ## if it's been more than 10 years, then make everyone older than 20 come off secondary prevention
    hist_arf[[paste0("hist_arf_",j,"_int")]][hist_arf$age > 20] <- 0 ## if it's been more than 10 years, then make everyone older than 20 come off secondary prevention
    hist_arf[[paste0("hist_arf_",j,"_sp_int")]][hist_arf$age > 20] <- 0 ## if it's been more than 10 years, then make everyone older than 20 come off secondary prevention
  }
  
  hist_arf[,total_10yr_history:=rowSums(.SD,na.rm=T),.SDcols = c(names(hist_arf)[grepl("hist_arf",names(hist_arf)) & !grepl("sp",names(hist_arf)) & !grepl("int",names(hist_arf))])]
  hist_arf[,total_10yr_history_sp:=rowSums(.SD,na.rm=T),.SDcols = c(names(hist_arf)[grepl("hist_arf",names(hist_arf)) & grepl("sp",names(hist_arf)) & !grepl("int",names(hist_arf))])]
  hist_arf[,total_10yr_history_int:=rowSums(.SD,na.rm=T),.SDcols = c(names(hist_arf)[grepl("hist_arf",names(hist_arf)) & !grepl("sp",names(hist_arf)) & grepl("int",names(hist_arf))])]
  hist_arf[,total_10yr_history_sp_int:=rowSums(.SD,na.rm=T),.SDcols = c(names(hist_arf)[grepl("hist_arf",names(hist_arf)) & grepl("sp",names(hist_arf)) & grepl("int",names(hist_arf))])]
  
  
  phar <- merge(phar,hist_arf,by=c("sex","age","draw"),all=T)
  
  ## count first ARF
  phar[age < 45,count_first_arf:=inc_first_arf*(pop-total_10yr_history)]
  phar[age < 45,count_first_arf_int:=inc_first_arf_int*(pop_int-total_10yr_history)]
  ## first ARF to RHD
  phar[,to_rhd_first:=rhd_transition*count_first_arf]
  phar[,to_rhd_first_int:=rhd_transition*count_first_arf_int]
  ## first ARF to death
  phar[,to_death_first:=count_first_arf*(epi$cd_mort_risk+death_first_arf)]
  phar[,to_death_first_int:=count_first_arf_int*(epi$cd_mort_risk+death_first_arf)]
  phar[,to_death_first_arf:=count_first_arf*(death_first_arf)]
  phar[,to_death_first_arf_int:=count_first_arf_int*(death_first_arf)]
  ## first ARF to remission
  phar[,to_remission_first:=count_first_arf-to_death_first-to_rhd_first]
  phar[,to_remission_first_int:=count_first_arf_int-to_death_first_int-to_rhd_first_int]
  ## recurrent ARF count -- cannot simply use the incidence of recurrence here though...?
  ## issue here being that before we calculated the impact factor with no coverage above age 20
  ## but coverage has to be dynamic because of the 10 year history rule
  ## so calculate incidence of recurrent ARF here-- can't deal with this now--will resolve later
  phar[age < 45,count_recur_arf:=inc_recur_arf*(total_10yr_history)]
  phar[age < 45,count_recur_arf_int:=inc_recur_arf_int*(total_10yr_history_int)]
  ## recurrent ARF to RHD
  phar[,to_rhd_recur:=rhd_transition_recur*count_recur_arf]
  phar[,to_rhd_recur_int:=rhd_transition_recur*count_recur_arf_int]
  ## recurrent ARF to death
  phar[,to_death_recur:=count_recur_arf*(epi$cd_mort_risk+death_recur_arf)]
  phar[,to_death_recur_int:=count_recur_arf_int*(epi$cd_mort_risk+death_recur_arf)]
  phar[,to_death_recur_arf:=count_recur_arf*(death_recur_arf)]
  phar[,to_death_recur_arf_int:=count_recur_arf_int*(death_recur_arf)]
  
  
  ## make new hist arf variables
  ## first normal
  phar[,paste0("hist_arf_",(base_year+round-1)):=count_first_arf-to_death_first-to_rhd_first]
  phar[[paste0("hist_arf_",(base_year+round-1))]][is.na(phar[[paste0("hist_arf_",(base_year+round-1))]])] <- 0
  ## then covered by sp
  phar[,paste0("hist_arf_",(base_year+round-1),"_sp"):=phar[[paste0("hist_arf_",(base_year+round-1))]]*coverage_start_draw_secondary]
  ## then normal int
  phar[,paste0("hist_arf_",(base_year+round-1),"_int"):=count_first_arf_int-to_death_first_int-to_rhd_first_int]
  phar[[paste0("hist_arf_",(base_year+round-1),"_int")]][is.na(phar[[paste0("hist_arf_",(base_year+round-1),"_int")]])] <- 0
  ## then covered by sp int
  phar[,paste0("hist_arf_",(base_year+round-1),"_sp_int"):=phar[[paste0("hist_arf_",(base_year+round-1),"_int")]]*coverage_level_secondary]
  
  if (any(phar[[paste0("hist_arf_",(base_year+round-1))]] < 0)) stop("negatives issue")
  if (any(phar[[paste0("hist_arf_",(base_year+round-1),"_int")]] < 0)) stop("negatives issue")
  
  ## the number with a history of ARF who don't have RHD is the history from previous year, plus those who have a new ARF and survive/don't transition to RHD,
  ## minus (1) those who have recurrence and die, (2) those who have recurrence and transition to RHD, (3) those who don't have recurrence and die of background mortality
  ## the rest return to hist_arf by default here 
  phar[,frac_lost_death_recur:=to_death_recur/total_10yr_history]
  phar[,frac_lost_rhd_recur:=to_rhd_recur/total_10yr_history]
  phar[,frac_lost_normal_death:=epi$cd_mort_risk*(total_10yr_history-count_recur_arf)/total_10yr_history]
  phar[,total_frac_lost:=frac_lost_death_recur+frac_lost_rhd_recur+frac_lost_normal_death]
  phar[total_10yr_history==0 & is.na(total_frac_lost),total_frac_lost:=0]
  summary(phar$total_frac_lost)
  if (any(phar$total_frac_lost[!is.na(phar$total_frac_lost)] > 1)) stop("impossible")
  
  phar[,frac_lost_death_recur_int:=to_death_recur_int/total_10yr_history_int]
  phar[,frac_lost_rhd_recur_int:=to_rhd_recur_int/total_10yr_history_int]
  phar[,frac_lost_normal_death_int:=epi$cd_mort_risk*(total_10yr_history_int-count_recur_arf_int)/total_10yr_history_int]
  phar[,total_frac_lost_int:=frac_lost_death_recur_int+frac_lost_rhd_recur_int+frac_lost_normal_death_int]
  phar[total_10yr_history_int==0 & is.na(total_frac_lost_int),total_frac_lost_int:=0]
  summary(phar$total_frac_lost_int)
  if (any(phar$total_frac_lost_int[!is.na(phar$total_frac_lost_int)] > 1)) stop("impossible")
  
  ## loop over cohorts here, subtract out the fraction lost, but take less out from those on secondary prophylaxis
  ## because people on secondary prophylaxis should have fewer recurrences  (use 1-effect)
  phar[,total_frac_lost_nonsp:=total_frac_lost/(total_10yr_history_sp/total_10yr_history*(1-effect_draw_secondary)+(1-total_10yr_history_sp/total_10yr_history))]
  phar[,total_frac_lost_nonsp_int:=total_frac_lost_int/(total_10yr_history_sp_int/total_10yr_history_int*(1-effect_draw_secondary)+(1-total_10yr_history_sp_int/total_10yr_history_int))]
  phar[,total_frac_lost_sp:=total_frac_lost_nonsp*(1-effect_draw_secondary)]
  phar[,total_frac_lost_sp_int:=total_frac_lost_nonsp_int*(1-effect_draw_secondary)]
  
  ## subtract out those lost from history of ARF
  vars_toadj <- names(phar)[grepl("hist_arf",names(phar)) & !names(phar) %in% paste0("hist_arf_",base_year+round-1) & !grepl("sp",names(phar)) & !grepl("int",names(phar))]
  for (j in vars_toadj) {
    ## subtract out those dying from recurrence, those lost to RHD from recurrence, and those without recurrence but dying from other causes
    phar[[j]] <- phar[[j]]-phar[[j]]*(phar$total_frac_lost)
    phar[[paste0(j,"_int")]] <- phar[[paste0(j,"_int")]]-phar[[paste0(j,"_int")]]*(phar$total_frac_lost_int)
    phar[[paste0(j,"_sp")]] <- phar[[paste0(j,"_sp")]]-phar[[paste0(j,"_sp")]]*(phar$total_frac_lost_sp)
    phar[[paste0(j,"_sp_int")]] <- phar[[paste0(j,"_sp_int")]]-phar[[paste0(j,"_sp_int")]]*(phar$total_frac_lost_sp_int)
    
  }
  
  if (any(unlist(phar[,names(phar)[grepl("hist_arf",names(phar))],with=F])[!is.na(unlist(phar[,names(phar)[grepl("hist_arf",names(phar))],with=F]))] < 0)) stop("negative")
  
  ## calculate impact on number transitioning to RHD (as %) to get impact on 
  phar[,to_rhd:=to_rhd_first+to_rhd_recur]
  phar[,to_rhd_int:=to_rhd_first_int+to_rhd_recur_int]
  phar[,scalar_rhd_inc_int:=to_rhd_int/to_rhd]
  phar[age > 44,scalar_rhd_inc_int:=1]
  phar[age == 0,scalar_rhd_inc_int:=1]
  phar[,arf_death:=to_death_recur_arf+to_death_first_arf] ## make sure these just ARF deaths, not deaths among people who got ARF from all causes
  phar[,arf_death_int:=to_death_recur_arf_int+to_death_first_arf_int]
  phar[is.na(arf_death),arf_death:=0]
  phar[is.na(arf_death_int),arf_death_int:=0]
  

  outphar <- copy(phar[,c("sex","age","draw","phar_seen", "phar_seen_int", "phar_treated", "phar_treated_int","count_first_arf","count_first_arf_int",
                          "count_recur_arf","count_recur_arf_int","arf_death","arf_death_int",names(phar)[grepl("hist_arf",names(phar))]),with=F])
  
  if (any(is.na(phar$scalar_rhd_inc_int))) stop("can't have missing values for the effect scalar")
  
  ## calculate impact on incidence numbers
  incnum <- readRDS(paste0(outdir,"/data/collated_epi_demog_model_inputs.RDS")) 
  incnum <- map_to_names(incnum,codedir=codedir,keep_ids=T,gbd=2017)
  incnum <- incnum[location_name=="African Union"]
  incnum <- incnum[draw %in% c(1:drawnum),c("location_name","sex","age","draw","inc_rhd"),with=F]
  incnum[,year:=2017+round-1]
  setnames(incnum,"inc_rhd","inc_rate")
  
  incnum <- merge(incnum,copy(p[,c("location_name","year","sex","age","draw","pop"),with=F]),by=c("location_name","year","sex","age","draw"),all.x=T)
  incnum[,pop_int:=pop]
  ## assuming some baseline decline in incidence from primordial prevention
  incnum[,inc:=inc_rate*pop*(1-specs$gas_arf_primordial*((round-1)/(total_rounds-1)))]
  incnum[,inc_int:=inc_rate*pop_int*(1-specs$gas_arf_primordial*((round-1)/(total_rounds-1)))*phar$scalar_rhd_inc_int]

  summary(phar$scalar_rhd_inc_int)
  
  ## now, compare incidence implied by ARF model with that from GBD
  comp <- copy(phar[,c("sex","age","draw","to_rhd","to_rhd_int"),with=F])
  comp[is.na(to_rhd),to_rhd:=0]
  comp[is.na(to_rhd_int),to_rhd_int:=0]
  comp <- merge(comp,incnum,by=c("sex","age","draw"),all=T)
  comp[,incrate:=inc/pop*100000]
  comp[,to_rhd_rate:=to_rhd/pop*100000]
  
  cmean <- copy(comp[,list(inc=mean(inc),to_rhd=mean(to_rhd),incrate=mean(incrate),to_rhd_rate=mean(to_rhd_rate)),by=c("sex","age")])
  scalars <- copy(cmean)
  cmean <- melt(cmean,id.vars=c("sex","age"),value.name="incidence",variable.name="version")
  cmean[,type:="Count"]
  cmean[grepl("rate",version),type:="Rate"]
  cmean[grepl("inc",version),version:="GBD"]
  cmean[grepl("to_rhd",version),version:="ARF model"]
  cmean <- dcast.data.table(cmean,sex+age+version~type,value.var="incidence")
  
  scalars[,age_group:=floor(age/5)*5]
  scalars <- scalars[,list(inc=sum(inc),to_rhd=sum(to_rhd)),by=c("sex","age_group")]
  scalars[,scalar:=inc/to_rhd]
  scalars <- scalars[,list(inc=sum(inc),to_rhd=sum(to_rhd)),by=c("sex")]
  scalars[,scalar:=inc/to_rhd]
  
  cmean <- merge(cmean,scalars[,c("sex","scalar"),with=F],by="sex",all.x=T)
  cmean[,Count_scaled:=Count]
  cmean[,Rate_scaled:=Rate]
  cmean[version=="ARF model",Count_scaled:=Count*scalar]
  cmean[version=="ARF model",Rate_scaled:=Rate*scalar]
  
  
  
  gg <- ggplot(data=cmean,aes(x=age,y=Count,color=version,group=version)) + geom_line() + theme_bw() +
    facet_wrap(~sex)
  print(gg)
  

  # make plots for appendix
  # pdf(paste0(outdir,"/results/figures/appendix/phar_gas_inputs_age_scen",run_num,".pdf"),width=8,height=4)
  # 
  # gg <- ggplot(data=cmean,aes(x=age,y=Rate,color=version,group=version)) + geom_line() + theme_bw() +
  #   facet_wrap(~sex) + ylab("Rate (per 100,000)") + scale_color_discrete("Estimate") + xlab("Age")
  # print(gg)
  # 
  # gg <- ggplot(data=cmean,aes(x=age,y=Rate_scaled,color=version,group=version)) + geom_line() + theme_bw() +
  #   facet_wrap(~sex) + ylab("Rate (per 100,000)") + scale_color_discrete("Estimate") + xlab("Age")
  # print(gg)
  # 
  # dev.off()

  
  print(scalars)
}


