## Monetized health gains as we go

library(data.table)
library(ggplot2)
library(EnvStats)
library(stringr)
library(gtools)
library(scales)
library(grid)
library(gridExtra)
library(RColorBrewer)

print(commandArgs()) 

## set directories
if (Sys.info()[1] == 'Windows') {
  rm(list=ls()) ## not clearing workspace on cluster so we can use commandArgs() hack
  codedir <- paste0("[Insert own directory]/rhd_invest_priv/")
  outdir <- paste0("[Insert own directory]/rhd_investment_case/")
  outdirtmp <- outdir ## this is just for storing files on the cluster with many draws, not backed up
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



## option for if running this code as an individual part for troubleshooting
## rather than as part of whole process
manual <-T
###########################
## Options for what to run
if (manual) {
  run_num <- 3
  specs <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/run_key.xlsx")))
  specs <- specs[run_id == run_num]
} else {
  if (Sys.info()[1] == 'Windows') {
    stop("can't run this on windows")
  } else {
    run_num <- str_split(as.character(commandArgs()[3]),pattern="_",n=Inf,simplify=T)[,2]
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
total_rounds <- specs$end_year-base_year+1 ## this is what it's called in costing code
scale_to <- specs$cov_scale_yr
int_start_year <- 2021

## read in AU locations (https://au.int/en/member_states/countryprofiles2)
locs <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/AU_member_states.xlsx")))
locs <- locs[!location_name %in% c("Sahrawi Republic")] ## we don't have burden estimates for Sahrawi Republic, so we will drop

## convert to GBD country names
locs <- swap_locnames(data=locs,out="GBD",version=2017)
regions <- unique(locs$au_region)


netben <- list()
for (i in regions) {
  cat(paste0("running ",i,"\n")); flush.console()
  locsshort <- gsub(" Africa","",i)
  netben[[paste0(i)]] <- readRDS(paste0(outdirtmp,"/results/model_run_results/",run_num,"/monetization_summary_draws_",locsshort,".rds"))
}
netben <- rbindlist(netben)


netben <- netben[,list(vlw=sum(vlw,na.rm=T),vlw_discounted=sum(vlw_discounted,na.rm=T),gdp=sum(gdp,na.rm=T),
                       gdp_discounted=sum(gdp_discounted,na.rm=T),full_income=sum(full_income,na.rm=T),full_income_discounted=sum(full_income_discounted,na.rm=T),
                       arf=sum(arf,na.rm=T),arf_discounted=sum(arf_discounted,na.rm=T),full_income_plus=sum(full_income_plus,na.rm=T),
                       full_income_plus_discounted=sum(full_income_plus_discounted,na.rm=T),cost=sum(cost,na.rm=T),cost_discounted=sum(cost_discounted_diff,na.rm=T),
                       net_benefit=sum(net_benefit,na.rm=T),net_benefit_discounted=sum(net_benefit_discounted,na.rm=T)),by=c("draw","year")]

netben[,cum_full_income_plus:=cumsum(full_income_plus),by="draw"]
netben[,cum_full_income_plus_discounted:=cumsum(full_income_plus_discounted),by="draw"]
netben[,cumcost:=cumsum()]
netben[,cum_roi:=cum_full_income_plus/cumcost]
netben[,cum_roi_discounted:=cum_full_income_plus_discounted/cumcost_discounted]
netben

roi1_summ <- roi1[,list(mean=mean(cum_roi),lower=quantile(cum_roi,probs=c(.025)),upper=quantile(cum_roi,probs=c(.975)),median=quantile(cum_roi,probs=c(.5)),
                        mean_discounted=mean(cum_roi_discounted),lower_discounted=quantile(cum_roi_discounted,probs=c(.025)),upper_discounted=quantile(cum_roi_discounted,probs=c(.975)),
                        median_discounted=quantile(cum_roi_discounted,probs=c(.5))),by="year"]
roi1_summ
roi1_summ[,type:="Full-income ROI"]


pdf(paste0("[Insert own directory]/rhd_investment_case/results/figures/appendix/benefit_cost_ratio_cumulative.pdf"),width=8,height=4)

gg <- ggplot(data=roi1_summ,aes(x=year,y=mean_discounted)) + 
  geom_line() + theme_bw() + #facet_wrap(~var,scales="fixed") + #ggtitle("Age-standardized Rates of Parameters, per 100,000") + 
  expand_limits(y = 0) + scale_color_discrete("") + scale_fill_discrete("") + scale_linetype_discrete("") +
  ylab("Benefit-Cost Ratio (cumulative)") + scale_x_continuous("Year",breaks=c(2020,2030,2040,2050,2060,2070,2080,2090)) + theme(legend.position="bottom") + 
  geom_ribbon(aes(ymin=lower_discounted, ymax=upper_discounted), alpha=0.1,colour = NA) + scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10))
print(gg)

dev.off()


gg <- ggplot(data=roi1_summ,aes(x=year,y=mean)) + 
  geom_line() + theme_bw() + #facet_wrap(~var,scales="fixed") + #ggtitle("Age-standardized Rates of Parameters, per 100,000") + 
  expand_limits(y = 0) + scale_color_discrete("") + scale_fill_discrete("") + scale_linetype_discrete("") +
  ylab("Benefit-Cost Ratio (cumulative)") + scale_x_continuous("Year",breaks=c(2020,2030,2040,2050,2060,2070,2080,2090)) + theme(legend.position="bottom") + 
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.1,colour = NA)
print(gg)


netben <- copy(roi1)
netben[,net_benefit:=full_income_plus-cost]
netben[,net_benefit_discounted:=full_income_plus_discounted-cost_discounted_diff]



netben_summ <- netben[,list(mean=mean(net_benefit),lower=quantile(net_benefit,probs=c(.025)),upper=quantile(net_benefit,probs=c(.975)),median=quantile(net_benefit,probs=c(.5)),
                            mean_discounted=mean(net_benefit_discounted),lower_discounted=quantile(net_benefit_discounted,probs=c(.025)),upper_discounted=quantile(net_benefit_discounted,probs=c(.975)),
                            median_discounted=quantile(net_benefit_discounted,probs=c(.5))),]
netben_summ
netben_summ[,type:="Full-income Net Benefit"]

# out <- rbind(vlosum,roi_summ,vlw_summ,vlw1_sum,roi1_summ)
out <- rbind(vlw1_sum,roi1_summ,netben_summ)


saveRDS(out,paste0("[Insert own directory]/rhd_investment_case/results/model_run_results_long/",run_num,"/monetization_summary.rds"))



