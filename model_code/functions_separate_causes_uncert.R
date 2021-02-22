

testing <- F
# ## for testing
if (testing) {
  
  round <- 1
  results <- NA
  total_rounds <- 14
  base_year <- 2017
  int_start_year <- 2021
  scale_to <- 2030
  drawnum <- 200
  
  
  round <- 2
  results <- NA
  total_rounds <- 29
  base_year <- 2017
  int_start_year <- 2021
  scale_to <- 2030
  drawnum <- 200
  
  round <- 5
  results <- dis[["Rheumatic heart disease"]]
  total_rounds <- 29
  base_year <- 2017
  int_start_year <- 2021
  scale_to <- 2030
  drawnum <- 200
  
}



rhd <- function(pop=NA,dem=NA,epi=NA,hist_sev=NA,
                results=NA,round=NA,
                total_rounds=NA,params=NA,base_year=NA,int_start_year=NA,scale_to=NA,
                hist_arf=NA,phar_arf=NA,specs=NA,drawnum=NA) {
  if (is.na(round)) stop("need to know which round this is running through")
  if (round > 1 & length(results)==1) stop("if round > 1, we need previous set of results to compile full results over time")
  if (is.na(total_rounds)) stop("need total rounds")
  if (round > total_rounds) stop("round can't be higher than total rounds")
  if (is.na(params)) stop("need input parameters")
  if (is.na(int_start_year)) stop("need to specify year to start intervention scale-up")
  if (is.na(hist_arf)) stop("need to specify people starting with history of ARF")
  if (is.na(phar_arf)) stop("need input params for the pharyngitis/ARF numbers")
  if (is.na(specs)) stop("need specs")
  if (is.na(hist_sev)) stop("need history of severe RHD")
  if (is.na(scale_to)) stop("need year scaling to")
  if (specs$hf_treat_yrs > 4) stop("not built for that")
  
  rhd_transition_scalar <- specs$rhd_transition_scalar ## drawn here using same seed as previous step
  if (!specs$hf_inc_option %in% c("GBD","Soweto","DisMod_adj")) stop("not set up for this")
  
  
  setnames(epi,c("rhd_hf_prev_rate","prev_rhd"),c("prev_severe","prev"))
  epi[,prev_mild:=prev-prev_severe]
  epi <- epi[order(location_name,sex,age,draw)]
  dem <- dem[order(year,location_name,sex,age,draw)]
  if (nrow(dem)!=(nrow(epi)*length(unique(dem$year)))) stop("issue with alignment")
  params <- params[order(intervention_id,location_name,draw)]
  if (nrow(epi)*length(unique(params$intervention_id))!=(nrow(params)*length(unique(epi$age))*length(unique(epi$sex)))) stop("issue with alignment")
  
  
  ## coverage level should go up in the year that's specified as the intervention scale-up start year
  total_scaleup_rounds <- (scale_to-base_year+1) - (int_start_year-base_year)
  
  if (round > (int_start_year-base_year)) params[,coverage_level:=(ending_coverage-starting_coverage)/(total_scaleup_rounds)*(round-(int_start_year-base_year))+starting_coverage]
  if (round > (scale_to-base_year+1)) params[,coverage_level:=ending_coverage]
  if (round <= (int_start_year-base_year)) params[,coverage_level:=starting_coverage]
  
  ## the scale-up for HF management for facility-level versus population coverage
  hf_facility_scale <- c(0.1,0.1,0.1,0.1,0.19,0.28,0.37,0.46,0.55,0.64,0.73,0.82,0.91,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,rep(1,45))
  hf_cov_scale <- c(0.08,0.08,0.08,0.08,0.089,0.107,0.134,0.17,0.215,0.269,0.332,0.404,0.476,0.548,0.611,0.665,0.71,0.746,0.773,0.791,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8,rep(0.8,45))
  hf_yr <- c(2017:2090)
  hf_facility_scale_dt <- data.table(data.frame(hf_facility_scale=hf_facility_scale,hf_cov_scale=hf_cov_scale,year=hf_yr,stringsAsFactors=F))
  hf_facility_scale_dt[,pct_progress_cov:=(hf_cov_scale-.08)/(.8-.08)]
  hf_facility_scale_dt[,pct_progress_fac:=(hf_facility_scale-.1)/(1-.1)]
  hf_facility_scale_dt <- hf_facility_scale_dt[year==base_year+round-1]
  
  params[intervention_id == 4,coverage_level:=(ending_coverage-starting_coverage)*hf_facility_scale_dt$pct_progress_cov+starting_coverage]

  ## now if end_scaleup exists, we should make the coverage go back to starting levels
  if (specs$end_scaleup != "No" & as.numeric(specs$end_scaleup) <= base_year + round - 1) params[,coverage_level:=starting_coverage]
  
  params[,impact_scalar:=1-(effect_draw*(coverage_level-starting_coverage))/(1-effect_draw*starting_coverage)]
  
  params <- dcast.data.table(params,location_name+draw~intervention_id,value.var=c("starting_coverage","coverage_level","effect_draw","impact_scalar"))
  epi <- merge(epi,params,by=c("location_name","draw"),all.x=T)
  epi <- epi[order(location_name,sex,age,draw)]
  
  ## unless doing strictly GBD-derived estimates, we're using 25% annual risk of mortality among people in severe group
  set.seed(706043878)
  rhd_mortrisk <- inv.logit(rnorm(n=drawnum,mean=logit(.25),sd=(logit(.35)-logit(.2))/(1.96*2)))
  summary(rhd_mortrisk)
  
  ## with-cause mortality is excess estimated from GBD plus projected cause-deleted mortality
  ## though with some specifications, this changes below (using rhd_mortrisk instead)
  epi[,with_cause_severe_mort:=mx_rhd/prev_severe+dem[year==base_year+round-1]$mort-(frac*dem[year==base_year+round-1]$mort)]
  epi[is.na(with_cause_severe_mort),with_cause_severe_mort:=0]
  if (any(epi$with_cause_severe_mort < 0)) stop("cannot be below 0")
  
  ## then, with the effect of heart failure management
  epi <- epi[order(location_name,sex,age,draw)]
  dem <- dem[order(year,location_name,sex,age,draw)]
  epi[,with_cause_severe_mort_int:=mx_rhd/prev_severe*impact_scalar_4+dem[year==base_year+round-1]$mort-(frac*dem[year==base_year+round-1]$mort)]
  epi[is.na(with_cause_severe_mort_int),with_cause_severe_mort_int:=0]
  
  ## cause-deleted mortality based on initial cause fraction and changing mortality
  epi[,cd_mort:=dem[year==base_year+round-1]$mort-frac*dem[year==base_year+round-1]$mort]
  epi[is.na(cd_mort),cd_mort:=0]
  
  ## for mortality among people who have had surgery, for now, temporarily using 80% decrease in cause-specific death rate from severe disease
  epi[,post_surg_mort:=mx_rhd/prev_severe*(1-effect_draw_5)+dem[year==base_year+round-1]$mort-frac*dem[year==base_year+round-1]$mort]
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
    epi[,post_surg_mort_risk:=rep(rhd_mortrisk,nrow(epi)/drawnum)*(1-effect_draw_5)+cd_mort_risk]
    epi[is.na(post_surg_mort_risk),post_surg_mort_risk:=0]
    
    epi[,with_cause_severe_mort_risk:=rep(rhd_mortrisk,nrow(epi)/drawnum)+cd_mort_risk]
    epi[is.na(with_cause_severe_mort_risk),with_cause_severe_mort_risk:=0]
    
    epi[,with_cause_severe_mort_int_risk:=rep(rhd_mortrisk,nrow(epi)/drawnum)*impact_scalar_4+cd_mort_risk]
    epi[is.na(with_cause_severe_mort_int_risk),with_cause_severe_mort_int_risk:=0]
    
    epi[,post_surg_mort:=-1*log(1-post_surg_mort_risk)]
    epi[,with_cause_severe_mort_int:=-1*log(1-with_cause_severe_mort_int_risk)]
    epi[,with_cause_severe_mort:=-1*log(1-with_cause_severe_mort_risk)]
    
  }
  if (!specs$hf_inc_option %in% c("GBD_adj","GBD","Soweto","DisMod","DisMod_adj")) stop("need to address risk properly--specification not included")
  epi[with_cause_severe_mort_risk < 0]
  epi[with_cause_severe_mort_risk > .99999999999999999999999]
  if (any(epi$with_cause_severe_mort_risk < 0) | any(epi$with_cause_severe_mort_risk > .9999999999999999999999)) stop("cannot be below 0 or above 1")
  

  ## FIRST STEP NOW IS GAS + ARF--NEEDED TO GET EFFECT SIZE ON RHD INCIDENCE
  ## load in number of people with history of ARF (pre-RHD) on/off secondary prophylaxis
  if (round == 1) { 
    hist_arf <- hist_arf[order(location_name,sex,age,draw)]
    hist_arf <- hist_arf[,c("location_name","sex","age","draw",paste0("hist_arf_",c((base_year-17):(base_year-1)))),with=F]
    hist_arf[,total_10yr_history:=rowSums(.SD,na.rm=T),.SDcols = c(names(hist_arf)[grepl("hist_arf",names(hist_arf))])]
    hist_arf <- hist_arf[order(location_name,sex,age,draw)]
    hist_arf_int <- copy(hist_arf) ## since intervention hasn't started yet
    if (nrow(epi)!=nrow(hist_arf)) stop("issue")
    epi <- epi[order(location_name,sex,age,draw)]
    for (j in c(names(hist_arf)[grepl("hist_arf",names(hist_arf))],"total_10yr_history")) {
      hist_arf[,paste0(j,"_sp"):=hist_arf[[paste0(j)]]*epi$starting_coverage_2]
      hist_arf_int[,paste0(j,"_sp"):=hist_arf_int[[paste0(j)]]*epi$starting_coverage_2]
    }
    setnames(hist_arf_int,c("location_name","sex","age","draw",paste0(names(hist_arf_int)[5:ncol(hist_arf_int)],"_int")))
    hist_arf <- merge(hist_arf,hist_arf_int,by=c("location_name","sex","age","draw"))
    
    
  } else {
    hist_arf <- copy(results[year==(base_year+(round-2)),c("location_name","sex","age","draw",names(results)[grepl("hist_arf",names(results))]),with=F])
  }
  
  ## implement primordial prevention assumption
  primord_aroc <- (log(1)-log(.85))/13
  primord_per <- exp(log(1)-primord_aroc*(round-1))
  
  ## now data on people with history of ARF in last 10 years are loaded, including those on secondary prevention, do simulation of pharyngitis for this year
  ## use pharyngitis incidence from phar_arf, use impact calculation given scale-up
  phar <- copy(phar_arf[year==base_year+round-1,c("location_name","age","sex","draw","phar_peryear","inc_gas_pharyngitis","inc_first_arf","inc_first_arf_int","inc_recur_arf","inc_recur_arf_int","pct_phar","inc_recur_arf_phar_nosp",
                                                  "coverage_start_draw_primary","coverage_level_primary","coverage_start_draw_secondary","coverage_level_secondary","effect_draw_secondary"),with=F])
  phar <- merge(phar,pop[year==base_year+round-1,c("location_name","sex","age","draw","pop","pop_int"),with=F],by=c("location_name","age","sex","draw"),all=T)
  phar <- phar[order(location_name,sex,age,draw)]
  phar[,phar_peryear:=phar_peryear*primord_per^(1/3)]
  phar[,inc_gas_pharyngitis:=inc_gas_pharyngitis*primord_per^(2/3)] ## assuming phar per year and % gas decline 
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
  ## now we have pharyngitis seen (cost per "visit") and pharyngitis treated (include injection/pills)
  ## these should be the pharyngitis variables we need to keep for costing (phar_seen, phar_seen_int, phar_treated, phar_treated_int)
  ## next, find incidence of first ARF, then recurrent ARF
  
  set.seed(80984502)
  death_first_arf <- exp(rnorm(n=drawnum,mean=log(.01),sd=(log(.02)-log(.005))/(2*1.96)))
  death_recur_arf <- death_first_arf*exp(rnorm(n=drawnum,mean=log(2),sd=.2))
  rhd_transition_female <- exp(rnorm(n=drawnum,mean=log(.36),sd=(log(.4788)-log(.2412))/(2*1.96)))
  rhd_transition_recur_female <- exp(rnorm(n=drawnum,mean=log(.36*2),sd=(log(.4788*2)-log(.2412*2))/(2*1.96)))
  
  rhd_transition_male <- rhd_transition_female*rhd_transition_scalar
  rhd_transition_recur_male <- rhd_transition_recur_female*rhd_transition_scalar
  
  
  
  phar <- phar[order(location_name,sex,age,draw)]
  phar[,death_first_arf:=rep(death_first_arf,nrow(phar)/drawnum)]
  phar[,death_recur_arf:=rep(death_recur_arf,nrow(phar)/drawnum)]
  phar[sex=="male",rhd_transition:=rep(rhd_transition_male,nrow(phar)/drawnum/2)]
  phar[sex=="male",rhd_transition_recur:=rep(rhd_transition_recur_male,nrow(phar)/drawnum/2)]
  phar[sex=="female",rhd_transition:=rep(rhd_transition_female,nrow(phar)/drawnum/2)]
  phar[sex=="female",rhd_transition_recur:=rep(rhd_transition_recur_female,nrow(phar)/drawnum/2)]
  
  ## now for recurrence, use history of ARF data (hist_arf and hist_arf_int)
  hist_arf[age < 95,age:=age+1]
  hist_arf <- hist_arf[,lapply(.SD,sum),by=c("location_name","sex","age","draw"),.SDcols=c(names(hist_arf)[grepl("hist_arf",names(hist_arf))])]
  
  ## check for negatives
  for (i in c(2000:2021)) {
    if (any(hist_arf[[paste0("hist_arf_",i)]] < 0 & !is.na(hist_arf[[paste0("hist_arf_",i)]]) )) stop("negative")
    if (any(hist_arf[[paste0("hist_arf_",i,"_sp")]] < 0 & !is.na(hist_arf[[paste0("hist_arf_",i,"_sp")]]) )) stop("negative")
    
    if (any(hist_arf[[paste0("hist_arf_",i,"_int")]] < 0 & !is.na(hist_arf[[paste0("hist_arf_",i,"_int")]]) )) stop("negative")
    if (any(hist_arf[[paste0("hist_arf_",i,"_sp_int")]] < 0 & !is.na(hist_arf[[paste0("hist_arf_",i,"_sp_int")]]) )) stop("negative")
  }
  
  
  ## determine which are people with history still in the past 10 years (or under age 20)
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
  
  
  ## check for negatives
  if (any(hist_arf$total_10yr_history < 0 & !is.na(hist_arf$total_10yr_history) )) stop("negative")
  if (any(hist_arf$total_10yr_history_sp < 0 & !is.na(hist_arf$total_10yr_history_sp) )) stop("negative")
  
  if (any(hist_arf$total_10yr_history_int < 0 & !is.na(hist_arf$total_10yr_history_int) )) stop("negative")
  if (any(hist_arf$total_10yr_history_sp_int < 0 & !is.na(hist_arf$total_10yr_history_sp_int) )) stop("negative")

  
  phar <- merge(phar,hist_arf,by=c("location_name","sex","age","draw"),all=T)
  
  if (nrow(phar) != nrow(epi)) stop("alignment issue")
  epi <- epi[order(location_name,sex,age,draw)]
  phar <- phar[order(location_name,sex,age,draw)]
  
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
  
  ## loop over cohorts here, subtract out the fraction lost
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
  setnames(phar,c("to_death_first_arf","to_death_recur_arf","to_death_first_arf_int","to_death_recur_arf_int"),
           c("arf_death_first","arf_death_recur","arf_death_first_int","arf_death_recur_int"))
  phar[is.na(arf_death),c("arf_death","arf_death_first","arf_death_recur"):=0]
  phar[is.na(arf_death_int),c("arf_death_int","arf_death_first_int","arf_death_recur_int"):=0]
  
  ## these should be the pharyngitis variables we need to keep for costing (phar_seen, phar_seen_int, phar_treated, phar_treated_int)
  ## and we need to keep the arf history variables for costing and for the next round of this code
  ## also need to save the incidence of ARF--we can do by new/recurrent just for extra info
  outphar <- copy(phar[,c("location_name","sex","age","draw","phar_seen", "phar_seen_int", "phar_treated", "phar_treated_int","count_first_arf","count_first_arf_int",
                          "count_recur_arf","count_recur_arf_int","arf_death","arf_death_int",
                          "arf_death_first","arf_death_first_int","arf_death_recur","arf_death_recur_int",
                          names(phar)[grepl("hist_arf",names(phar))]),with=F])
  
  if (any(is.na(phar$scalar_rhd_inc_int))) stop("can't have missing values for the effect scalar")

  
  ## transition probability from mild to severe RHD
  trans_mild_severe <- copy(epi[,c("location_name","sex","age","draw","inc_implied","impact_scalar_7"),with=F])
  trans_mild_severe[,inc_implied:=inc_implied*primord_per]
  trans_mild_severe[,inc_implied_int:=inc_implied*impact_scalar_7]
  trans_mild_severe[,impact_scalar_7:=NULL]
  if (any(trans_mild_severe$inc_implied < 0)) stop("negative implied incidence")
  
  incnum <- copy(epi[,c("location_name","sex","age","draw","inc_rhd"),with=F])
  incnum[,year:=2017+round-1]
  setnames(incnum,"inc_rhd","inc_rate")
  
  incnum <- merge(incnum,copy(pop),by=c("location_name","year","sex","age","draw"),all.x=T)
  ## assuming baseline decline in incidence from primordial prevention
  incnum[,inc:=inc_rate*pop*primord_per]
  incnum[,inc_int:=inc_rate*pop_int*primord_per*phar$scalar_rhd_inc_int]
  rm(phar); gc()
  incnum <- incnum[order(year,location_name,sex,age,draw)]
  
  ## regression to normal draws
  set.seed(18250238)
  regression_mean <- (log(.15)+log(.01))/2
  regression_se <- (log(.15)-log(.01))/(2*1.96)
  regression_draws <- exp(rnorm(drawnum,regression_mean,regression_se))
  
  
  ## number with mild RHD (in first iteration, use GBD-derived)
  ## subsequently, use what is stored from previous iteration
  ## but also add incidence on and shift for population aging a year
  ## and subtract those who advance to severe disease
  ## will then expose to mortality
  if (round == 1) {
    mild_prev_premort <- copy(epi[,c("location_name","sex","age","draw","cases_mild_persistent_prev_yr_count"),with=F])
    setnames(mild_prev_premort,"cases_mild_persistent_prev_yr_count","mild_prev")
    mild_prev_premort[,mild_prev_int:=mild_prev]
    
    } else {
    ## if after the first round, just add stored cases persisting from previous year to incidence numbers
    mild_prev_premort <- copy(results[year==(base_year+(round-2)),c("location_name","sex","age","draw","mild_prev","mild_prev_int"),with=F])
    
  }
  add <- copy(mild_prev_premort[age==0])
  mild_prev_premort[,age:=age+1]
  mild_prev_premort[age==96,age:=95]
  mild_prev_premort <- mild_prev_premort[,list(mild_prev=sum(mild_prev),mild_prev_int=sum(mild_prev_int)),by=c("location_name","sex","age","draw")]
  mild_prev_premort <- rbind(add,mild_prev_premort)
  mild_prev_premort <- mild_prev_premort[order(location_name,sex,age,draw)]
  
  mild_prev_premort[,mild_prev:=mild_prev+incnum$inc]
  mild_prev_premort[,mild_prev_int:=mild_prev_int+incnum$inc_int]
  mild_prev_premort[,mild_prev_start:=mild_prev]
  mild_prev_premort[,mild_prev_start_int:=mild_prev_int]
  
  ## we have  (1) transition to death, (2) transition to severe, (3) regression, (4) remainder
  ## should be constrained to 100% -- the transition to death, transition to severe, and regression are mutually exclusive

  regression_probs <- copy(mild_prev_premort)
  setnames(regression_probs,c("mild_prev","mild_prev_int"),c("regression_probs","regression_probs_int"))
  regression_probs[,regression_probs:=rep(regression_draws,length(regression_probs)/drawnum)]
  regression_probs[,regression_probs_int:=rep(regression_draws,length(regression_probs)/drawnum)]
  ## regression just under 20 (per GBD assumption)
  regression_probs[age >= 20,regression_probs:=0]
  regression_probs[age >= 20,regression_probs_int:=0]
  
  ## transition to severe is trans_mild_severe$inc_implied
  ## transition to death is epi$cd_mort_risk
  ## regression is regression_probs$regression_probs
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
  
  ## number with severe RHD (in first iteration, use GBD-derived number)
  ## then use transition probability--these to add are calculated above
  ## then subtract out deaths after
  ## split out by # of years that people have had severe disease
  ## effect of HF treatment is then only applied to those who have been in severe state for fewer years
  sev_prev_premort <- copy(epi[,c("location_name","sex","age","draw","cases_severe_persistent_prev_yr_count"),with=F])
  setnames(sev_prev_premort,"cases_severe_persistent_prev_yr_count","sev_prev")
  if (round == 1) {
    sev_prev_premort[,sev_prev_int:=sev_prev]
    for (sevi in c(1:5)) {
      sev_prev_premort[,paste0("sev_prev",sevi):=sev_prev*hist_sev[[paste0("sev_prev",sevi)]]]
      sev_prev_premort[,paste0("sev_prev_int",sevi):=sev_prev_int*hist_sev[[paste0("sev_prev",sevi)]]]
    }
  }
  
  
  ## if past the first round, replace based on results from previous rounds
  if (round > 1) {
    ## if after the first round, we don't need to multiply by population because the stored results are in counts already--just add stored prevalence numbers to incidence numbers
    sev_prev_premort <- copy(results[year==(base_year+(round-2)),c("location_name","sex","age","draw",paste0("sev_prev",c(1:5)),paste0("sev_prev_int",c(1:5))),with=F])
    
  }
  
  add <- copy(sev_prev_premort[age==0])
  sev_prev_premort[,age:=age+1]
  sev_prev_premort[age==96,age:=95]
  sev_prev_premort <- sev_prev_premort[,lapply(.SD,sum),by=c("location_name","sex","age","draw"),.SDcols=names(sev_prev_premort)[grepl("sev_prev",names(sev_prev_premort))]]
  sev_prev_premort <- rbind(add,sev_prev_premort)
  sev_prev_premort <- sev_prev_premort[order(location_name,sex,age,draw)]
  ## since we aged them forward a year, renumber them
  ## add for 5
  sev_prev_premort[,sev_prev5:=sev_prev5+sev_prev4]
  sev_prev_premort[,sev_prev_int5:=sev_prev_int5+sev_prev_int4]
  
  for (sevyrs in c(4:2)) {
    sev_prev_premort[,paste0("sev_prev",sevyrs):=sev_prev_premort[[paste0("sev_prev",(sevyrs-1))]]]
    sev_prev_premort[,paste0("sev_prev_int",sevyrs):=sev_prev_premort[[paste0("sev_prev_int",(sevyrs-1))]]]
  } 
  
  ## new people for 1
  sev_prev_premort[,sev_prev1:=tosevere$tosevere]
  sev_prev_premort[,sev_prev_int1:=tosevere$tosevere_int]
  
  sev_prev_premort[,sev_prev:=sev_prev1+sev_prev2+sev_prev3+sev_prev4+sev_prev5]
  sev_prev_premort[,sev_prev_int:=sev_prev_int1+sev_prev_int2+sev_prev_int3+sev_prev_int4+sev_prev_int5]
  sev_prev_premort[,sev_prev_start:=sev_prev]
  sev_prev_premort[,sev_prev_start_int:=sev_prev_int]
  
  ## prevalence of post-surgical patients
  if (round == 1) {
    
    postsurg_premort <- copy(sev_prev_premort)
    postsurg_premort[,c("postsurg_premort","postsurg_premort_int"):=0]
    
    if (nrow(epi)!=nrow(postsurg_premort)) stop("issue")
    epi <- epi[order(location_name,sex,age,draw)]
    postsurg_premort <- postsurg_premort[order(location_name,sex,age,draw)]
    postsurg_premort[age %in% c(10:39),postsurg_premort:=sev_prev*epi[age %in% c(10:39)]$starting_coverage_5]
    postsurg_premort[age %in% c(10:39),postsurg_premort_int:=sev_prev_int*epi[age %in% c(10:39)]$starting_coverage_5]
    postsurg_premort[,pct_surg:=postsurg_premort/sev_prev]
    postsurg_premort[,pct_surg_int:=postsurg_premort_int/sev_prev_int]
    postsurg_premort[age==0,c("pct_surg","pct_surg_int"):=0]
    postsurg_premort[,c("sev_prev","sev_prev_int",paste0("sev_prev",c(1:5)),paste0("sev_prev_int",c(1:5))):=NULL]
    
    ## simplifying assumption about who is getting surgery
    sev_prev_premort[,sev_prev:=sev_prev*(1-postsurg_premort$pct_surg)]
    for (sevyrs in c(1:5)) { sev_prev_premort[,paste0("sev_prev",sevyrs):=sev_prev_premort[[paste0("sev_prev",sevyrs)]]*(1-postsurg_premort$pct_surg)]}
    sev_prev_premort[,sev_prev_int:=sev_prev_int*(1-postsurg_premort$pct_surg_int)]
    for (sevyrs in c(1:5)) { sev_prev_premort[,paste0("sev_prev_int",sevyrs):=sev_prev_premort[[paste0("sev_prev_int",sevyrs)]]*(1-postsurg_premort$pct_surg_int)]}
    
    ## record number of surgeries
    num_surg <- copy(postsurg_premort)
    setnames(num_surg,c("postsurg_premort","postsurg_premort_int"),c("num_surgs","num_surgs_int"))
    
    set.seed(5838592)
    death_frac_op <- 1-exp(rnorm(drawnum,mean=log(1-.03),sd=(log(1-.02)-log(1-.04))/(2*1.96)))
    
    deaths_op <- copy(postsurg_premort)
    deaths_op[,deaths_op:=postsurg_premort*rep(death_frac_op,nrow(sev_prev_premort)/drawnum)]
    deaths_op[,deaths_op_int:=postsurg_premort_int*rep(death_frac_op,nrow(sev_prev_premort)/drawnum)]
    deaths_op[,c("postsurg_premort","postsurg_premort_int"):=NULL]
    
    ## subtract operative deaths from new postsurg
    postsurg_premort[,postsurg_premort:=postsurg_premort-deaths_op$deaths_op]
    postsurg_premort[,postsurg_premort_int:=postsurg_premort_int-deaths_op$deaths_op_int]
    
    ## add in assumption about people already living post-surg
    postsurg_premort[,postsurg_premort:=postsurg_premort+epi$postsurg_start]
    postsurg_premort[,postsurg_premort_int:=postsurg_premort_int + epi$postsurg_start]
    
    postsurg_premort[,postsurg_premort_start:=postsurg_premort]
    postsurg_premort[,postsurg_premort_start_int:=postsurg_premort_int]
    
  } else {
    
    
    new_postsurg_premort <- copy(sev_prev_premort)
    new_postsurg_premort[,c("new_postsurg_premort","new_postsurg_premort_int"):=0]
    if (nrow(epi)!=nrow(new_postsurg_premort)) stop("issue")
    epi <- epi[order(location_name,sex,age,draw)]
    new_postsurg_premort <- new_postsurg_premort[order(location_name,sex,age,draw)]
    new_postsurg_premort[age %in% c(10:39),new_postsurg_premort:=sev_prev*epi[age %in% c(10:39)]$starting_coverage_5]
    new_postsurg_premort[age %in% c(10:39),new_postsurg_premort_int:=sev_prev_int*epi[age %in% c(10:39)]$coverage_level_5]
    new_postsurg_premort[,pct_surg:=new_postsurg_premort/sev_prev]
    new_postsurg_premort[,pct_surg_int:=new_postsurg_premort_int/sev_prev_int]
    new_postsurg_premort[age==0,c("pct_surg","pct_surg_int"):=0]
    new_postsurg_premort[,c("sev_prev","sev_prev_int",paste0("sev_prev",c(1:5)),paste0("sev_prev_int",c(1:5))):=NULL]
    
    ## subtract out new surgery from severe
    sev_prev_premort[,sev_prev:=sev_prev*(1-new_postsurg_premort$pct_surg)]
    for (sevyrs in c(1:5)) { sev_prev_premort[,paste0("sev_prev",sevyrs):=sev_prev_premort[[paste0("sev_prev",sevyrs)]]*(1-new_postsurg_premort$pct_surg)]}
    sev_prev_premort[,sev_prev_int:=sev_prev_int*(1-new_postsurg_premort$pct_surg_int)]
    for (sevyrs in c(1:5)) { sev_prev_premort[,paste0("sev_prev_int",sevyrs):=sev_prev_premort[[paste0("sev_prev_int",sevyrs)]]*(1-new_postsurg_premort$pct_surg_int)]}
    
    # ## subtract out new surgery from severe
    num_surg <- copy(new_postsurg_premort)
    setnames(num_surg,c("new_postsurg_premort","new_postsurg_premort_int"),c("num_surgs","num_surgs_int"))
    
    set.seed(5838592)
    death_frac_op <- 1-exp(rnorm(drawnum,mean=log(1-.03),sd=(log(1-.02)-log(1-.04))/(2*1.96)))
    
    deaths_op <- copy(new_postsurg_premort)
    deaths_op[,deaths_op:=new_postsurg_premort*rep(death_frac_op,nrow(sev_prev_premort)/drawnum)]
    deaths_op[,deaths_op_int:=new_postsurg_premort_int*rep(death_frac_op,nrow(sev_prev_premort)/drawnum)]
    deaths_op[,c("new_postsurg_premort","new_postsurg_premort_int"):=NULL]
    
    
    ## subtract operative deaths from new postsurg
    new_postsurg_premort[,new_postsurg_premort:=new_postsurg_premort-deaths_op$deaths_op]
    new_postsurg_premort[,new_postsurg_premort_int:=new_postsurg_premort_int-deaths_op$deaths_op_int]
    
    postsurg_premort <- copy(results[year==(base_year+(round-2)),c("location_name","sex","age","draw","postsurg","postsurg_int"),with=F])
    
    add <- copy(postsurg_premort[age==0])
    postsurg_premort[,age:=age+1]
    postsurg_premort[age==96,age:=95]
    postsurg_premort <- postsurg_premort[,list(postsurg=sum(postsurg),postsurg_int=sum(postsurg_int)),by=c("location_name","sex","age","draw")]
    postsurg_premort <- rbind(add,postsurg_premort)
    postsurg_premort <- postsurg_premort[order(location_name,sex,age,draw)]
    postsurg_premort[,postsurg_premort:=postsurg + new_postsurg_premort$new_postsurg_premort]
    postsurg_premort[,postsurg_premort_int:=postsurg_int + new_postsurg_premort$new_postsurg_premort_int]
    
    postsurg_premort[,postsurg_premort_start:=postsurg_premort]
    postsurg_premort[,postsurg_premort_start_int:=postsurg_premort_int]
    
  }
  
  deaths <- copy(deaths_op)

  ## setlle up mild category
  ## deaths among mild RHD
  deaths[,deaths_mild:=mild_prev_premort$mild_prev*epi$cd_mort_risk_adj]
  deaths[,deaths_mild_int:=mild_prev_premort$mild_prev_int*epi$cd_mort_risk_adj]
  ## subtract out number transitioning to severe
  mild_prev_premort[,mild_prev:=mild_prev-tosevere$tosevere]
  mild_prev_premort[,mild_prev_int:=mild_prev_int-tosevere$tosevere_int]
  ## subtract out number transitioning to normal
  mild_prev_premort[,mild_prev:=mild_prev-toregress$toregress]
  mild_prev_premort[,mild_prev_int:=mild_prev_int-toregress$toregress_int]
  ## deaths subtracted out in "out counts" below  
  
  
  ## HF management effect depends on # of years with disease
  for (svyrs in c(1:(specs$hf_treat_yrs))) {
    deaths[,paste0("deaths_severe",svyrs):=sev_prev_premort[[paste0("sev_prev",svyrs)]]*epi$with_cause_severe_mort_risk]
    deaths[,paste0("deaths_severe_int",svyrs):=sev_prev_premort[[paste0("sev_prev_int",svyrs)]]*epi$with_cause_severe_mort_int_risk]
    
    deaths[,paste0("deaths_severe_rhd",svyrs):=sev_prev_premort[[paste0("sev_prev",svyrs)]]*(epi$with_cause_severe_mort_risk-epi$cd_mort_risk)]
    deaths[,paste0("deaths_severe_rhd_int",svyrs):=sev_prev_premort[[paste0("sev_prev_int",svyrs)]]*(epi$with_cause_severe_mort_int_risk-epi$cd_mort_risk)]
    
  }
  
  for (svyrs in c((specs$hf_treat_yrs+1):5)) {
    deaths[,paste0("deaths_severe",svyrs):=sev_prev_premort[[paste0("sev_prev",svyrs)]]*epi$with_cause_severe_mort_risk]
    deaths[,paste0("deaths_severe_int",svyrs):=sev_prev_premort[[paste0("sev_prev_int",svyrs)]]*epi$with_cause_severe_mort_risk]
    
    deaths[,paste0("deaths_severe_rhd",svyrs):=sev_prev_premort[[paste0("sev_prev",svyrs)]]*(epi$with_cause_severe_mort_risk-epi$cd_mort_risk)]
    deaths[,paste0("deaths_severe_rhd_int",svyrs):=sev_prev_premort[[paste0("sev_prev_int",svyrs)]]*(epi$with_cause_severe_mort_risk-epi$cd_mort_risk)]
    
  }
  
  
  ## get total deaths among severe--subtracted out from individual ones below
  deaths[,deaths_severe:=deaths_severe1+deaths_severe2+deaths_severe3+deaths_severe4+deaths_severe5]
  deaths[,deaths_severe_int:=deaths_severe_int1+deaths_severe_int2+deaths_severe_int3+deaths_severe_int4+deaths_severe_int5]
  
  deaths[,deaths_severe_rhd:=deaths_severe_rhd1+deaths_severe_rhd2+deaths_severe_rhd3+deaths_severe_rhd4+deaths_severe_rhd5]
  deaths[,deaths_severe_rhd_int:=deaths_severe_rhd_int1+deaths_severe_rhd_int2+deaths_severe_rhd_int3+deaths_severe_rhd_int4+deaths_severe_rhd_int5]
  
  ## all deaths from post-surgical group
  deaths[,deaths_postsurg:=postsurg_premort$postsurg_premort*epi$post_surg_mort_risk]
  deaths[,deaths_postsurg_int:=postsurg_premort$postsurg_premort_int*epi$post_surg_mort_risk] ## effect is captured through transition, so same mort risk
  ## deaths from post-surgical group from RHD
  deaths[,deaths_postsurg_rhd:=postsurg_premort$postsurg_premort*(epi$post_surg_mort_risk-epi$cd_mort_risk)]
  deaths[,deaths_postsurg_rhd_int:=postsurg_premort$postsurg_premort_int*(epi$post_surg_mort_risk-epi$cd_mort_risk)] ## effect is captured through transition, so same mort risk
  
  ## subtract out deaths from counts
  out_counts <- copy(sev_prev_premort)
  for (svyrs in c(1:5)) {
    out_counts[,paste0("sev_prev",svyrs):=out_counts[[paste0("sev_prev",svyrs)]]-deaths[[paste0("deaths_severe",svyrs)]]]
    out_counts[,paste0("sev_prev_int",svyrs):=out_counts[[paste0("sev_prev_int",svyrs)]]-deaths[[paste0("deaths_severe_int",svyrs)]]]
  }
  out_counts[,sev_prev:=sev_prev1+sev_prev2+sev_prev3+sev_prev4+sev_prev5] 
  out_counts[,sev_prev_int:=sev_prev_int1+sev_prev_int2+sev_prev_int3+sev_prev_int4+sev_prev_int5]
  out_counts[,sev_prev_pys:=(sev_prev_start+sev_prev)/2]
  out_counts[,sev_prev_pys_int:=(sev_prev_start_int+sev_prev_int)/2]
  out_counts[,c("sev_prev_start","sev_prev_start_int"):=NULL]
  
  deaths[,c(paste0("deaths_severe",c(1:5))):=NULL]
  deaths[,c(paste0("deaths_severe_int",c(1:5))):=NULL]
  deaths[,c(paste0("deaths_severe_rhd",c(1:5))):=NULL]
  deaths[,c(paste0("deaths_severe_rhd_int",c(1:5))):=NULL]
  
  out_counts[,mild_prev:=mild_prev_premort$mild_prev-deaths$deaths_mild]
  out_counts[,mild_prev_int:=mild_prev_premort$mild_prev_int-deaths$deaths_mild_int]
  out_counts[,mild_prev_pys:=(mild_prev+mild_prev_premort$mild_prev_start)/2]
  out_counts[,mild_prev_pys_int:=(mild_prev_int+mild_prev_premort$mild_prev_start_int)/2]
  out_counts[,postsurg:=postsurg_premort$postsurg_premort-deaths$deaths_postsurg]
  out_counts[,postsurg_int:=postsurg_premort$postsurg_premort_int-deaths$deaths_postsurg_int]
  out_counts[,postsurg_pys:=(out_counts$postsurg+postsurg_premort$postsurg_premort_start)/2]
  out_counts[,postsurg_pys_int:=(out_counts$postsurg_int+postsurg_premort$postsurg_premort_start_int)/2]
  
  ## some marginally below 0 from rounding error
  out_counts[mild_prev > -.001 & mild_prev < 0,mild_prev:=0]
  out_counts[mild_prev_int > -.001 & mild_prev_int < 0,mild_prev_int:=0]
  
  ## break if any of these are negative
  if (any(unlist(out_counts[,names(out_counts)[!names(out_counts) %in% c("location_name","sex","age","draw")],with=F]) < 0)) {
    print(out_counts[mild_prev > -0.01 & mild_prev < 0])
    print(out_counts[mild_prev_int > -0.01 & mild_prev_int < 0])
    
    stop("Negative value")
  }
  
  
  ## number that stay well
  ## this number not really used
  out_counts[,staywell:=pop[year==base_year+round-1]$pop-mild_prev-sev_prev-postsurg]
  out_counts[,staywell_int:=pop[year==base_year+round-1]$pop_int-mild_prev_int-sev_prev_int-postsurg_int]
  
  ## save incidence
  out_counts$incnum<-incnum$inc
  out_counts$incnum_int<-incnum$inc_int
  
  ## save regression
  out_counts$regress <- toregress$toregress
  out_counts$regress_int <- toregress$toregress_int
  
  ## save number of surgeries
  out_counts$num_surgs <- num_surg$num_surgs
  out_counts$num_surgs_int <- num_surg$num_surgs_int
  
  ## save number going to severe
  out_counts$tosevere <- tosevere$tosevere
  out_counts$tosevere_int <- tosevere$tosevere_int
  
  
  ## need total deaths from RHD
  deaths[,deaths_cause:=deaths_postsurg_rhd+deaths_severe_rhd+deaths_op]
  deaths[,deaths_cause_int:=deaths_postsurg_rhd_int+deaths_severe_rhd_int+deaths_op_int]
  
  
  ## add variables for year/round
  out_counts[,year:=base_year+round-1]
  deaths[,year:=base_year+round-1]
  
  out_counts <- merge(out_counts,deaths,by=c("location_name","year","sex","age","draw"),all=T)
  out_counts <- out_counts[order(location_name,year,sex,age,draw)]
  
  out_counts <- merge(out_counts,outphar,by=c("location_name","sex","age","draw"),all=T)
  

  ## for the first year, then just save this as results
  if (round == 1) {
    results <- out_counts
  }
  

  ## if after first round running, append on
  if (round > 1) {
    results <- rbind(results,out_counts,use.names=T,fill=T)
  }
  
  return(results)
}












