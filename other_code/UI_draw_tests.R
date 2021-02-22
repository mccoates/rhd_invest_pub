## Matthew Coates
## Compare confidence intervals for sets of draws to determine stability

rm(list=ls())
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
  codedir <- paste0("[Insert own directory]/rhd_invest_priv/")
  outdir <- paste0("[Insert own directory]/rhd_investment_case/")
  outdirtmp <- outdir ## this is just for storing files on the cluster with many draws, not backed up
} else {
  codedir <- paste0("[Insert own directory]/rhd_invest_priv/")
  outdir <- paste0("[Insert own directory]/rhd_investment_case/")
  outdirtmp <- paste0("[Insert own directory]")
}
run_num <- 22

## load some health outcomes
deaths <- readRDS(paste0(outdir,"/results/model_run_results/",run_num,"/deaths.rds"))
deaths <- deaths[year > 2020]
deaths[,mx_rhd:=deaths_cause/pop]
deaths[,mx_int_rhd:=deaths_cause_int/pop_int]
deaths[,deaths_averted_rhd:=(mx_rhd-mx_int_rhd)*pop]
deaths <- deaths[,list(rhd_deaths_averted=sum(deaths_averted_rhd)),by=c("draw")]
deaths[,group:=floor((draw-1)/1000)+1]
deaths[,rhd_deaths_averted:=rhd_deaths_averted/1000]
deaths <- deaths[,list(mean=mean(rhd_deaths_averted),lower=quantile(rhd_deaths_averted,probs=.025),upper=quantile(rhd_deaths_averted,probs=.975)),by=c("group")]
deaths[,out:=paste0(format(round(mean,2),nsmall=2)," (",format(round(lower,2),nsmall=2),"-",format(round(upper,2),nsmall=2),")")]
deaths[,variable:="RHD Deaths Averted (Thousands)"]
deaths <- dcast.data.table(deaths,variable~group,value.var=c("out"))

## load some benefit-cost outcomes
netben <- readRDS(paste0(outdir,"/results/model_run_results/",run_num,"/monetization_summary_draws.rds"))
netben[,group:=floor((draw-1)/1000)+1]
netben <- melt(netben,id.vars=c("draw","group","location_name"))
netben <- netben[,list(mean=mean(value),lower=quantile(value,probs=.025),upper=quantile(value,probs=.975)),by=c("group","variable")]
netben <- netben[variable %in% c("net_benefit_discounted","roi_discounted","cost","full_income_plus")]
netben[variable %in% c("net_benefit_discounted","cost","full_income_plus"),mean:=mean/1000000000]
netben[variable %in% c("net_benefit_discounted","cost","full_income_plus"),lower:=lower/1000000000]
netben[variable %in% c("net_benefit_discounted","cost","full_income_plus"),upper:=upper/1000000000]
netben[,out:=paste0(format(round(mean,2),nsmall=2)," (",format(round(lower,2),nsmall=2),"-",format(round(upper,2),nsmall=2),")")]
netben <- dcast.data.table(netben,variable~group,value.var=c("out"))

netben <- rbind(netben,deaths)

netben[,variable:=factor(variable,levels=c("RHD Deaths Averted (Thousands)","cost","full_income_plus","net_benefit_discounted","roi_discounted"),
                         labels=c("RHD Deaths Averted","Cost (Billions USD)","Health Benefits (Full Income Approach, Billions USD)",
                                  "Net Benefits (Discounted, Billions USD)","Benefit-Cost Ratio (Discounted)"))]
netben <- netben[order(variable)]

write.csv(netben,paste0(outdir,"/results/model_run_results/",run_num,"/UI_test_summary.csv"))





