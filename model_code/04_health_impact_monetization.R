## Monetized health gains

library(data.table)
library(ggplot2)
library(reldist)
library(EnvStats)

## set directories
if (Sys.info()[1] == 'Windows') {
  rm(list=ls()) ## not clearing workspace on cluster so we can use commandArgs() hack
  codedir <- paste0("[Insert own directory]/rhd_invest_priv/")
  outdir <- paste0("[Insert own directory]/rhd_investment_case/")
  outdirtmp <- outdir
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
source(paste0(codedir,"/model_code/costing_functions.R"))


manual <- F
###########################
## Options for what to run
if (manual) {
  run_num <- 23
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
  if (manual == T) {
    locs <- "Western Africa"
    locsshort <- "Western"
  } else {
    locs <- commandArgs()[1]
    locsshort <- locs ## b/c filepaths can't have spaces
    print(locsshort)
    locs <- paste0(locs," Africa")    
  }
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

## input data needed from modeling:
## deaths from both models by age and sex
deaths <- readRDS(paste0(outdirtmp,"/results/model_run_results/",run_num,"/deaths",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".rds"))
deaths <- deaths[draw %in% c(1:drawnum)]
arf_deaths <- readRDS(paste0(outdirtmp,"/results/model_run_results/",run_num,"/results_for_costing",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".rds"))
arf_deaths <- arf_deaths[draw %in% c(1:drawnum)]
arf_deaths <- arf_deaths[year > 2020,c("location_name","year","sex","age","draw","arf_death","arf_death_int"),with=F]


## all-cause death rates can be calculated from total deaths and pop
cost <- readRDS(paste0(outdirtmp,"/results/model_run_results/",run_num,"/cost",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".rds"))
cost <- cost[draw %in% c(1:drawnum)]
cost <- cost[,list(cost=sum(cost_diff),cost_discounted_diff=sum(cost_discounted_diff)),by=c("location_name","draw","year")]
costback <- copy(cost)
cost <- cost[,list(cost=sum(cost),cost_discounted_diff=sum(cost_discounted_diff)),by=c("location_name","draw")]


## Deaths averted
deaths <- deaths[year > 2020]
deaths[,deaths_averted_rhd:=deaths_cause-deaths_cause_int]
deaths[,deaths_averted_all:=deaths_total-deaths_total_int]
deaths[,mx_rhd:=deaths_cause/pop]
deaths[,mx_int_rhd:=deaths_cause_int/pop_int]
deaths[,deaths_averted_implied_rhd:=(mx_rhd-mx_int_rhd)*pop]

sum(deaths$deaths_averted_rhd)/sum(deaths$deaths_averted_all) ## difference due to effect on pop (3-4%)
sum(deaths$deaths_averted_implied_rhd)/sum(deaths$deaths_averted_all) ## compare two ways of calculating deaths averted (very similar)
## using method using mx difference and population

arf_deaths <- merge(arf_deaths,deaths[,c("location_name","year","sex","age","draw","pop","pop_int"),with=F],by=c("location_name","year","sex","age","draw"),all.x=T)
arf_deaths[,mx:=arf_death/pop]
arf_deaths[,mx_int:=arf_death_int/pop_int]
arf_deaths[,arf_deaths_averted_implied:=(mx-mx_int)*pop]

sum(arf_deaths$arf_deaths_averted_implied)/(1000*drawnum)


sum(deaths$deaths_averted_implied_rhd)/(1000*drawnum)
test <- copy(deaths)
test <- test[,list(averted=sum(deaths_averted_implied_rhd)),by=c("draw")]
test <- test[,list(mean=mean(averted),lower=quantile(averted,probs=c(.025)),upper=quantile(averted,probs=c(.975))),]
test/1000

deaths[,deaths_averted_rhd:=deaths_averted_implied_rhd]
arf_deaths[,deaths_averted_arf:=arf_deaths_averted_implied]

test <- copy(deaths)
test <- test[,list(mean=mean(deaths_averted_rhd),lower=quantile(deaths_averted_rhd,probs=c(.025)),
                   upper=quantile(deaths_averted_rhd,probs=c(.975))),by=c("year","sex","age")]

## if we didn't want to include ARF death differences in benefits for sensitivity
if (specs$arf_vlo=="Yes") {
  deaths <- merge(deaths,arf_deaths[,c("location_name","year","sex","age","draw","deaths_averted_arf"),with=F],by=c("location_name","year","sex","age","draw"),all.x=T)
  deaths[,deaths_averted:=deaths_averted_rhd+deaths_averted_arf]
} else {
  deaths[,deaths_averted:=deaths_averted_rhd]
}

## add in cost of difference in ARF costs
arf <- readRDS(paste0(outdirtmp,"/results/model_run_results/",run_num,"/results_for_costing",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".rds"))
arf <- arf[draw %in% c(1:drawnum)]
arf <- arf[,list(count_inc_arf=sum(count_inc_arf,na.rm=T),count_inc_arf_int=sum(count_inc_arf_int,na.rm=T)),by=c("location_name","year","draw")] ## keep all ARF cases
arf[,arfdiff:=count_inc_arf-count_inc_arf_int]
## what percent to assume go to hospital
arf[,arf_diff_adj:=specs$arf_hosp_prop*arfdiff]

## arf hospitalization cost
arfc <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/other_inputs/intervention_costs.xlsx"),
                                       sheet=6,startRow=1))
arfc <- arfc[!is.na(Currency),c(1:10),with=F]
setnames(arfc,c("item","toggle","mean","lower","upper","currency","currency_year","cost_type","shared_intervention","country_costed"))
arfc <- convert_costs(arfc,locs)

## for AU UI to result from aggregation of country-specific UIs, need draws to be heavily correlated across countries
## or for country UIs to be larger--we don't really have empirical evidence here--correlate draws
set.seed(2367)
arfcdraws <- rtri(n=drawnum,mode=arfc[location_name=="African Union"]$mean,max=arfc[location_name=="African Union"]$upper,min=arfc[location_name=="African Union"]$lower)
arfcdraws <- matrix(rep(arfcdraws,nrow(arfc)),byrow=T,nrow=nrow(arfc))
arfc[,rat:=mean/arfc[location_name=="African Union"]$mean]
arfcdraws <- apply(arfcdraws,MARGIN=2,FUN=function(x){
  x*arfc$rat
})
if (!is.matrix(arfcdraws)) arfcdraws <- matrix(arfcdraws,nrow=nrow(arfc))
arfc <- cbind(arfc[,c("location_name"),with=F],arfcdraws)
arfc <- melt(arfc,id.vars=c("location_name"),variable.name="draw",value.name="cost")
arfc[,draw:=as.numeric(gsub("V","",draw))]
arf <- merge(arf,arfc,by=c("location_name","draw"),all.x=T)
arf <- arf[location_name %in% locs]

arf[,vlo:=arf_diff_adj*cost]
arf[,vlo_discounted:=vlo*(1/(1+specs$discount_rate))^(year-2020)]
arf <- arf[,list(vlo=sum(vlo),vlo_discounted=sum(vlo_discounted)),by=c("location_name","draw","year")]
backarf <- copy(arf)
arf <- arf[,list(vlo=sum(vlo),vlo_discounted=sum(vlo_discounted)),by=c("location_name","draw")]

## add AU if it's not justu a run for AU
if (locs != "African Union") {
  add <- copy(arf)
  add <- add[,list(vlo=sum(vlo),vlo_discounted=sum(vlo_discounted)),by=c("draw")]
  add[,location_name:="African Union"]
  arf <- rbind(arf,add)
  rm(add); gc()  
}


deaths[,pop_diff:=pop_int-pop]
vlw <- copy(deaths[,c("location_name","year","sex","age","draw","deaths_averted","pop_diff"),with=F])


vlw1 <- copy(vlw)

gnivals <- fread(paste0(codedir,"/data/other_inputs/gni_for_VSL.csv"))
vslcalc <- copy(gnivals[!location_name %in% c("United States")])
vslcalc <- dcast.data.table(vslcalc,location_name~year,value.var="gni")
setnames(vslcalc,c("2015","2019"),c("gni2015","gni2019"))
us_vsl <- 9400000
vslcalc[,vsl2015:=us_vsl*((gni2015)/(gnivals[year==2015 & location_name=="United States"]$gni))^1.5]
vslcalc[,vsl2019:=vsl2015*((gni2019)/gni2015)^1]

## now we have VSL in 2019, use projected real changes in GNI per capita to adjust VSL in future years
proj <- fread(paste0(codedir,"/data/other_inputs/projected_gdp_gnichange.csv"))
proj[,scalar:=1+pct_change_gni/100]
proj <- proj[order(location_name,year)]
proj[,scalar:=c(1,proj$scalar[1:(nrow(proj)-1)])]
proj[year==2019,scalar:=1]
proj[,cumulative_scalar:=cumprod(scalar),by="location_name"]
proj <- merge(proj,vslcalc,by=c("location_name"),all.x=T)
proj[,gni:=gni2019*cumulative_scalar]
## using an elasticity of 1, the scalar for GNI change is the same as the scalar for VSL change
proj[,vsl:=vsl2019*cumulative_scalar]

if (manual) {
  outproj <- copy(proj[location_name %in% c("African Union","Central Africa","Eastern Africa","Northern Africa","Southern Africa","Western Africa")])
  outproj <- outproj[,c("year","location_name","gdp_pc","gni","vsl"),with=F]
  outproj <- melt(outproj,id.vars=c("location_name","year"))
  outproj[,variable:=factor(variable,levels=c("gdp_pc","gni","vsl"),labels=c("GDP per capita","GNI per capita","VSL"))]
  
  pdf(paste0(outdir,"/results/figures/appendix/projected_gdp_gni_vsl.pdf"),width=8,height=4)
  gg <- ggplot(outproj[year < 2031],aes(x=year,y=value,group=location_name,color=location_name)) + geom_line() + theme_bw() + 
    facet_wrap(~variable,scales="free") + scale_y_continuous(limits=c(0,NA),labels=comma) + scale_color_discrete("Location") + 
    xlab("Year") + ylab("USD (2019)") + theme(legend.position="bottom")
  print(gg)
  gg <- ggplot(outproj,aes(x=year,y=value,group=location_name,color=location_name)) + geom_line() + theme_bw() + 
    facet_wrap(~variable,scales="free") + scale_y_continuous(limits=c(0,NA),labels=comma) + scale_color_discrete("Location") + 
    xlab("Year") + ylab("USD (2019)") + theme(legend.position="bottom")
  print(gg)
  
  dev.off()
}

if (manual) write.csv(proj[location_name %in% c("African Union","Central Africa","Eastern Africa","Northern Africa","Southern Africa","Western Africa")],
                      paste0(outdir,"/results/figures/appendix/projected_gdp_gni_vsl.csv"),row.names=F)




vlw1 <- merge(vlw1,proj[,c("location_name","year","gdp_pc","vsl"),with=F],by=c("location_name","year"),all.x=T)
vlw1[,vlw:=deaths_averted*vsl]
vlw1[,gdp:=gdp_pc*pop_diff]
vlw1[,full_income:=vlw+gdp]

vlw1[,discount:=(1/(1+specs$discount_rate))^(year-2020)]
vlw1[,vlw_discounted:=vlw*discount]
vlw1[,gdp_discounted:=gdp*discount]
vlw1[,full_income_discounted:=vlw_discounted+gdp_discounted]

## save so we can do cumulative ROI over time
backvlw1 <- copy(vlw1)

vlw1 <- vlw1[,list(vlw=sum(vlw),vlw_discounted=sum(vlw_discounted),
                   gdp=sum(gdp),gdp_discounted=sum(gdp_discounted),
                   full_income=sum(full_income),full_income_discounted=sum(full_income_discounted)),by=c("location_name","draw")]
vlw1_sum <- copy(vlw1[,list(mean=mean(full_income),lower=quantile(full_income,probs=c(.025)),upper=quantile(full_income,probs=c(.975)),median=quantile(full_income,probs=c(.5)),
                            mean_discounted=mean(full_income_discounted),lower_discounted=quantile(full_income_discounted,probs=c(.025)),
                            upper_discounted=quantile(full_income_discounted,probs=c(.975)),median_discounted=quantile(full_income_discounted,probs=c(.5))),c("location_name")])
vlw1_sum[,type:="Full-income"]

test <- copy(vlw1)
test[,pct:=vlw/full_income]
mean(test$pct)


## calculate ROI from VLW VSL approach (add in the health care costs)
setnames(arf,c("vlo","vlo_discounted"),c("arf","arf_discounted"))

vlw1_plus <- copy(vlw1)
vlw1_plus <- merge(vlw1_plus,arf,by=c("location_name","draw"))
vlw1_plus[,full_income_plus:=full_income+arf]
vlw1_plus[,full_income_plus_discounted:=full_income_discounted+arf_discounted]

vlw1_plus_sum <- copy(vlw1_plus[,list(mean=mean(full_income_plus),lower=quantile(full_income_plus,probs=c(.025)),upper=quantile(full_income_plus,probs=c(.975)),median=quantile(full_income_plus,probs=c(.5)),
                                      mean_discounted=mean(full_income_plus_discounted),lower_discounted=quantile(full_income_plus_discounted,probs=c(.025)),
                                      upper_discounted=quantile(full_income_plus_discounted,probs=c(.975)),median_discounted=quantile(full_income_plus_discounted,probs=c(.5))),c("location_name")])
vlw1_plus_sum[,type:="Full-income plus"]


roi1 <- merge(vlw1_plus,cost,by=c("location_name","draw"),all=T)


roi1[,roi:=full_income_plus/cost]
roi1[,roi_discounted:=full_income_plus_discounted/cost_discounted_diff]
roi1

roi1_summ <- roi1[,list(mean=mean(roi),lower=quantile(roi,probs=c(.025)),upper=quantile(roi,probs=c(.975)),median=quantile(roi,probs=c(.5)),
                        mean_discounted=mean(roi_discounted),lower_discounted=quantile(roi_discounted,probs=c(.025)),upper_discounted=quantile(roi_discounted,probs=c(.975)),
                        median_discounted=quantile(roi_discounted,probs=c(.5))),c("location_name")]
roi1_summ
roi1_summ[,type:="Full-income ROI"]


netben <- copy(roi1)
netben[,net_benefit:=full_income_plus-cost]
netben[,net_benefit_discounted:=full_income_plus_discounted-cost_discounted_diff]



netben_summ <- netben[,list(mean=mean(net_benefit),lower=quantile(net_benefit,probs=c(.025)),upper=quantile(net_benefit,probs=c(.975)),median=quantile(net_benefit,probs=c(.5)),
                            mean_discounted=mean(net_benefit_discounted),lower_discounted=quantile(net_benefit_discounted,probs=c(.025)),upper_discounted=quantile(net_benefit_discounted,probs=c(.975)),
                            median_discounted=quantile(net_benefit_discounted,probs=c(.5))),c("location_name")]
netben_summ
netben_summ[,type:="Full-income Net Benefit"]

# out <- rbind(vlosum,roi_summ,vlw_summ,vlw1_sum,roi1_summ)
out <- rbind(vlw1_sum,vlw1_plus_sum,roi1_summ,netben_summ)


saveRDS(out,paste0(outdir,"/results/model_run_results/",run_num,"/monetization_summary",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".rds"))
## save draws to be able to make comparisons in draw numbers
if (Sys.info()[1]!="Windows") saveRDS(netben,paste0(outdirtmp,"/results/model_run_results/",run_num,"/monetization_summary_draws",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".rds"))



## keep year-specific info for cumulative ROI
vlw1 <- copy(backvlw1)
arf <- copy(backarf)
cost <- copy(costback)


vlw1 <- vlw1[,list(vlw=sum(vlw),vlw_discounted=sum(vlw_discounted),
                   gdp=sum(gdp),gdp_discounted=sum(gdp_discounted),
                   full_income=sum(full_income),full_income_discounted=sum(full_income_discounted)),by=c("location_name","draw","year")]
vlw1_sum <- copy(vlw1[,list(mean=mean(full_income),lower=quantile(full_income,probs=c(.025)),upper=quantile(full_income,probs=c(.975)),median=quantile(full_income,probs=c(.5)),
                            mean_discounted=mean(full_income_discounted),lower_discounted=quantile(full_income_discounted,probs=c(.025)),
                            upper_discounted=quantile(full_income_discounted,probs=c(.975)),median_discounted=quantile(full_income_discounted,probs=c(.5))),c("location_name","year")])
vlw1_sum[,type:="Full-income"]

## calculate ROI from VLW VSL approach (add in the health care costs)
setnames(arf,c("vlo","vlo_discounted"),c("arf","arf_discounted"))

vlw1_plus <- copy(vlw1)
vlw1_plus <- merge(vlw1_plus,arf,by=c("location_name","draw","year"))
vlw1_plus[,full_income_plus:=full_income+arf]
vlw1_plus[,full_income_plus_discounted:=full_income_discounted+arf_discounted]

vlw1_plus_sum <- copy(vlw1_plus[,list(mean=mean(full_income_plus),lower=quantile(full_income_plus,probs=c(.025)),upper=quantile(full_income_plus,probs=c(.975)),median=quantile(full_income_plus,probs=c(.5)),
                                      mean_discounted=mean(full_income_plus_discounted),lower_discounted=quantile(full_income_plus_discounted,probs=c(.025)),
                                      upper_discounted=quantile(full_income_plus_discounted,probs=c(.975)),median_discounted=quantile(full_income_plus_discounted,probs=c(.5))),c("location_name","year")])
vlw1_plus_sum[,type:="Full-income plus"]


roi1 <- merge(vlw1_plus,cost,by=c("location_name","draw","year"),all=T)

roi1 <- roi1[year >= 2021]
roi1[,cum_full_income_plus:=cumsum(full_income_plus),by=c("location_name","draw")]
roi1[,cum_full_income_plus_discounted:=cumsum(full_income_plus_discounted),by=c("location_name","draw")]
roi1[,cumcost:=cumsum(cost),by=c("location_name","draw")]
roi1[,cumcost_discounted:=cumsum(cost_discounted_diff),by=c("location_name","draw")]
roi1[,cum_roi:=cum_full_income_plus/cumcost]
roi1[,cum_roi_discounted:=cum_full_income_plus_discounted/cumcost_discounted]
roi1



## saveRDS(out,paste0(outdir,"/results/model_run_results/",run_num,"/cum_monetization_summary",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".rds"))
## save draws to be able to make comparisons in draw numbers
if (Sys.info()[1]!="Windows") saveRDS(roi1,paste0(outdirtmp,"/results/model_run_results/",run_num,"/cum_monetization_summary_draws",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".rds"))















