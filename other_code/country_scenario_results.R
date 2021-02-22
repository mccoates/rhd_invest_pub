
rm(list=ls())
library(data.table)
library(ggplot2)
library(writexl)

## set directories
if (Sys.info()[1] == 'Windows') {
  codedir <- paste0("[Insert own directory]/rhd_invest_priv/")
  outdir <- paste0("[Insert own directory]/rhd_investment_case/")
  outdirtmp <- outdir
} else {
  codedir <- paste0("[Insert own directory]/rhd_invest_priv/")
  outdir <- paste0("[Insert own directory]/rhd_investment_case/")
  outdirtmp <- paste0("[Insert own directory]")
}
## load functions
source(paste0(codedir,"/functions/swap_location_names.R"))
source(paste0(codedir,"/functions/map_ids_names.R"))
source(paste0(codedir,"/model_code/costing_functions.R"))


## load results from runs
#runs <- c(1:21,29)
runs <- c(34:37)

## compare % reduction in deaths from runs
res <- list()
for (run in runs) {
  cat(paste0("compiling run ",run,"\n")); flush.console()
  
  specs <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/run_key.xlsx")))
  specs <- specs[run_id == run]
  locsshort <- specs$runloc
  
  end_year <- specs$end_year
  end_year <- "End Year"
  
  rates <- readRDS(paste0(outdirtmp,"/results/model_run_results/",run,"/pct_asrate_reduct_sims_",locsshort,".rds"))
  deaths <- readRDS(paste0(outdirtmp,"/results/model_run_results/",run,"/deaths_",locsshort,".rds"))
  deaths <- deaths[draw %in% c(1:specs$drawnum)]
  counts <- readRDS(paste0(outdirtmp,"/results/model_run_results/",run,"/counts_averted_sims_",locsshort,".rds"))
  costs <- readRDS(paste0(outdir,"/results/model_run_results/",run,"/cost_summary_",locsshort,".rds"))
  totcosts <- readRDS(paste0(outdirtmp,"/results/model_run_results/",run,"/cost_",locsshort,".rds"))
  roi <- readRDS(paste0(outdir,"/results/model_run_results/",run,"/monetization_summary_",locsshort,".rds"))

  deaths[,arf_death:=arf_death_first+arf_death_recur]
  deaths[,arf_death_int:=arf_death_first_int+arf_death_recur_int]
  
  ## Deaths averted
  deaths[,mx_rhd:=deaths_cause/pop]
  deaths[,mx_int_rhd:=deaths_cause_int/pop_int]
  deaths[,rhd_deaths_averted:=(mx_rhd-mx_int_rhd)*pop]
  deaths[,mx_arf:=arf_death/pop]
  deaths[,mx_arf_int:=arf_death_int/pop_int]
  deaths[,arf_deaths_averted:=(mx_arf-mx_arf_int)*pop]
  deaths[,arf_rhd_deaths_averted:=rhd_deaths_averted+arf_deaths_averted]
  deaths <- deaths[,list(rhd_deaths_averted=sum(rhd_deaths_averted),arf_deaths_averted=sum(arf_deaths_averted),
                         arf_rhd_deaths_averted=sum(arf_rhd_deaths_averted),pop=sum(pop),pop_int=sum(pop_int)),by=c("location_name","year","draw")]
  dthsyr <- copy(deaths[year > 2020])
  
  ## merge on incidence counts averted
  counts <- counts[year!="All" & var == "incnum",c("location_name","year","draw","FALSE","TRUE"),with=F]
  setnames(counts,c("FALSE","TRUE"),c("incnum","incnum_int"))
  counts[,year:=as.numeric(year)]
  deaths <- merge(deaths[year > 2020],counts,by=c("location_name","year","draw"),all.x=T)
  deaths[,incnum:=incnum/pop]
  deaths[,incnum_int:=incnum_int/pop_int]
  deaths[,inc_averted:=(incnum-incnum_int)*pop]
  
  deaths <- deaths[,list(rhd_deaths_averted=sum(rhd_deaths_averted),arf_deaths_averted=sum(arf_deaths_averted),
                         arf_rhd_deaths_averted=sum(arf_rhd_deaths_averted),inc_averted=sum(inc_averted)),by=c("location_name","draw")]
  deaths <- melt(deaths,id.vars=c("location_name","draw"),variable.name="var")
  deaths <- deaths[,list(mean=mean(value),median=quantile(value,probs=c(.5)),lower=quantile(value,probs=c(.025)),
                         upper=quantile(value,probs=c(.975))),by=c("location_name","var")]
  deaths[var=="arf_deaths_averted",var:=paste0("ARF Deaths Averted, 2021-",end_year)]
  deaths[var=="rhd_deaths_averted",var:=paste0("RHD Deaths Averted, 2021-",end_year)]
  deaths[var=="arf_rhd_deaths_averted",var:=paste0("ARF and RHD Deaths Averted, 2021-",end_year)]
  deaths[var=="inc_averted",var:=paste0("New RHD Cases Averted, 2021-",end_year)]
  
  
  
  ## cost per death averted
  costdeath <- totcosts[,list(cost_diff=sum(cost_diff),cost_discounted_diff=sum(cost_discounted_diff)),by=c("location_name","draw","year")]
  dthsyr[,arf_rhd_deaths_averted_discounted:=arf_rhd_deaths_averted*(1/(1+specs$discount_rate))^(year-2020)]
  costdeath <- merge(costdeath,dthsyr,by=c("location_name","draw","year"),all.x=T)
  costdeath <- costdeath[year >= 2021]
  costdeath <- costdeath[,list(cost=sum(cost_diff),cost_discounted=sum(cost_discounted_diff),deaths_averted=sum(arf_rhd_deaths_averted),deaths_averted_discounted=sum(arf_rhd_deaths_averted_discounted)),by=c("location_name","draw")]
  costdeath[,cost_per_death_averted:=cost/deaths_averted]
  costdeath[,cost_per_death_averted_discounted:=cost_discounted/deaths_averted_discounted]
  costdeath <- costdeath[,c("location_name","draw","cost_per_death_averted","cost_per_death_averted_discounted"),with=F]
  costdeath <- melt(costdeath,id.vars=c("location_name","draw"))
  costdeath[variable=="cost_per_death_averted",var:="Cost per Death Averted"]
  costdeath[variable=="cost_per_death_averted_discounted",var:="Cost per Death Averted (Discounted)"]
  costdeath <- costdeath[,list(mean=mean(value),median=quantile(value,probs=c(.5)),lower=quantile(value,probs=c(.025)),
                               upper=quantile(value,probs=c(.975))),by=c("location_name","var")]
  
  
  ## summarize rates
  rates <- rates[,c("location_name","var","draw","reduct_as_rate","pct_reduct_as_rate"),with=F]
  rates[,reduct_as_rate:=reduct_as_rate*100000]
  rates <- melt(rates,id.vars=c("location_name","var","draw"))
  rates[,var:=paste0(var,"_",variable)]
  rates <- rates[,list(mean=mean(value),median=quantile(value,probs=c(.5)),lower=quantile(value,probs=c(.025)),
                       upper=quantile(value,probs=c(.975))),by=c("location_name","var")]
  rates[var=="arf_death_pct_reduct_as_rate",var:=paste0("Percent Reduction in ",end_year," ARF Death Rate")]
  rates[var=="deaths_cause_pct_reduct_as_rate",var:=paste0("Percent Reduction in ",end_year," RHD Death Rate")]
  rates[var=="total_prev_pct_reduct_as_rate",var:=paste0("Percent Reduction in ",end_year," RHD Prevalence")] ## this isn't right--really to get RHD cases averted we should do cumulative incident cases
  rates[var=="incnum_pct_reduct_as_rate",var:=paste0("Percent Reduction in ",end_year," RHD Incidence")]
  rates[var=="arf_death_reduct_as_rate",var:=paste0("Reduction in ",end_year," ARF Death Rate")]
  rates[var=="deaths_cause_reduct_as_rate",var:=paste0("Reduction in ",end_year," RHD Death Rate")]
  rates[var=="total_prev_reduct_as_rate",var:=paste0("Reduction in ",end_year," RHD Prevalence")] ## this isn't right--really to get RHD cases averted we should do cumulative incident cases
  rates[var=="incnum_reduct_as_rate",var:=paste0("Reduction in ",end_year," RHD Incidence")]
  
  
  costs <- costs[cat=="Total" & year == "All"]
  costs_discounted <- copy(costs[,c("location_name","mean_discounted","median_discounted","lower_discounted","upper_discounted"),with=F])
  costs <- costs[,c("location_name","mean","median","lower","upper"),with=F]
  costs[,var:=paste0("Total Cost, 2021-",end_year)]
  costs_discounted[,var:=paste0("Total Cost (Discounted), 2021-",end_year)]
  setnames(costs_discounted,names(costs))
  costs <- rbind(costs,costs_discounted)
  
  roi[type=="VLO",var:=paste0("Value of Lost Economic Output Averted (VLO), 2021-",end_year)]
  roi[type=="VLO ROI",var:=paste0("Return on Investment (VLO/Cost), 2021-",end_year)]
  roi[type=="VLW (VSLY approach)",var:=paste0("Value of Lost Welfare (VSLY) Averted, 2021-",end_year)]
  roi[type=="VLW (VSL approach)",var:=paste0("Value of Lost Welfare (VSL) Averted, 2021-",end_year)]
  roi[type=="VLW (VSL approach) ROI",var:=paste0("Return on Investment (VLW (VSL)/Cost), 2021-",end_year)]
  roi[type=="Full-income",var:=paste0("Full Income Value, 2021-",end_year)]
  roi[type=="Full-income ROI",var:=paste0("Return on Investment, 2021-",end_year)]
  roi[type=="Full-income Net Benefit",var:=paste0("Net Benefit, 2021-",end_year)]
  #roi[type=="Full-income plus",var:=paste0("Full Income Benefit, 2021-",end_year)]
  roi <- roi[type!="Full-income plus"]
  
  roi_discounted <- copy(roi[,c("location_name","var","mean_discounted","median_discounted","lower_discounted","upper_discounted"),with=F])
  roi_discounted[var==paste0("Value of Lost Economic Output Averted (VLO), 2021-",end_year),var:=paste0("Value of Lost Economic Output Averted (VLO) (Discounted), 2021-",end_year)]
  roi_discounted[var==paste0("Return on Investment (VLO/Cost), 2021-",end_year),var:=paste0("Return on Investment (VLO/Cost) (Discounted), 2021-",end_year)]
  roi_discounted[var==paste0("Value of Lost Welfare (VSLY) Averted, 2021-",end_year),var:=paste0("Value of Lost Welfare (VSLY) Averted (Discounted), 2021-",end_year)]
  roi_discounted[var==paste0("Value of Lost Welfare (VSL) Averted, 2021-",end_year),var:=paste0("Value of Lost Welfare (VSL) Averted (Discounted), 2021-",end_year)]
  roi_discounted[var==paste0("Return on Investment (VLW (VSL)/Cost), 2021-",end_year),var:=paste0("Return on Investment (VLW (VSL)/Cost) (Discounted), 2021-",end_year)]
  roi_discounted[var==paste0("Full Income Value, 2021-",end_year),var:=paste0("Full Income Value (Discounted), 2021-",end_year)]
  roi_discounted[var==paste0("Return on Investment, 2021-",end_year),var:=paste0("Return on Investment (Discounted), 2021-",end_year)]
  roi_discounted[var==paste0("Net Benefit, 2021-",end_year),var:=paste0("Net Benefit (Discounted), 2021-",end_year)]
  #roi_discounted[var==paste0("Full Income Benefit, 2021-",end_year),var:=paste0("Full Income Benefit (Discounted), 2021-",end_year)]
  
  roi <- roi[,c("location_name","var","mean","median","lower","upper"),with=F]
  setnames(roi_discounted,names(roi))
  roi <- rbind(roi,roi_discounted)
  
  res[[paste0(run)]] <- rbind(rates,deaths,costs,roi,costdeath)
  res[[paste0(run)]][,run_id:=run]
  
}

res <- rbindlist(res)

specs <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/run_key.xlsx")))
res <- merge(res,specs[,c("run_id","run_name"),with=F],by=c("run_id"),all.x=T)
if (any(is.na(res$run_name))) stop("unmerged id")


res[,placeholder:=1]

## change labels
## this is relic of when names had end year in them
end_year <- "End Year"


# gg <- ggplot(res,aes(x=placeholder,y=mean,color=runfact,group=runfact)) + theme_bw() + coord_flip() + geom_point() + facet_wrap(~var,scales="free")
# print(gg)


res[var %in% c(paste0("ARF Deaths Averted, 2021-",end_year),paste0("RHD Deaths Averted, 2021-",end_year),paste0("New RHD Cases Averted, 2021-",end_year),
               "Cost per Death Averted","Cost per Death Averted (Discounted)",paste0("ARF and RHD Deaths Averted, 2021-",end_year)),mean:=mean/1000]
res[var %in% c(paste0("ARF Deaths Averted, 2021-",end_year),paste0("RHD Deaths Averted, 2021-",end_year),paste0("New RHD Cases Averted, 2021-",end_year),
               "Cost per Death Averted","Cost per Death Averted (Discounted)",paste0("ARF and RHD Deaths Averted, 2021-",end_year)),median:=median/1000]
res[var %in% c(paste0("ARF Deaths Averted, 2021-",end_year),paste0("RHD Deaths Averted, 2021-",end_year),paste0("New RHD Cases Averted, 2021-",end_year),
               "Cost per Death Averted","Cost per Death Averted (Discounted)",paste0("ARF and RHD Deaths Averted, 2021-",end_year)),lower:=lower/1000]
res[var %in% c(paste0("ARF Deaths Averted, 2021-",end_year),paste0("RHD Deaths Averted, 2021-",end_year),paste0("New RHD Cases Averted, 2021-",end_year),
               "Cost per Death Averted","Cost per Death Averted (Discounted)",paste0("ARF and RHD Deaths Averted, 2021-",end_year)),upper:=upper/1000]
res[var %in% c(paste0("ARF Deaths Averted, 2021-",end_year),paste0("RHD Deaths Averted, 2021-",end_year),paste0("New RHD Cases Averted, 2021-",end_year),
               "Cost per Death Averted","Cost per Death Averted (Discounted)",paste0("ARF and RHD Deaths Averted, 2021-",end_year)),var:=paste0(var," (thousands)")]


res[var %in% unique(res$var[grepl("Total Cost",res$var) | grepl("Value",res$var) | grepl("Benefit",res$var)]),var:=paste0(var," (billions)")]
res[var %in% unique(res$var[grepl("Total Cost",res$var) | grepl("Value",res$var) | grepl("Benefit",res$var)]),mean:=mean/1000000000]
res[var %in% unique(res$var[grepl("Total Cost",res$var) | grepl("Value",res$var) | grepl("Benefit",res$var)]),median:=median/1000000000]
res[var %in% unique(res$var[grepl("Total Cost",res$var) | grepl("Value",res$var) | grepl("Benefit",res$var)]),lower:=lower/1000000000]
res[var %in% unique(res$var[grepl("Total Cost",res$var) | grepl("Value",res$var) | grepl("Benefit",res$var)]),upper:=upper/1000000000]

res[,factvar:=factor(var,levels=c(paste0("Percent Reduction in ",end_year," RHD Death Rate"),paste0("Reduction in ",end_year," RHD Death Rate"),
                                  paste0("RHD Deaths Averted, 2021-",end_year," (thousands)"),
                                  paste0("Percent Reduction in ",end_year," ARF Death Rate"),paste0("Reduction in ",end_year," ARF Death Rate"),
                                  paste0("ARF Deaths Averted, 2021-",end_year," (thousands)"),
                                  paste0("Percent Reduction in ",end_year," RHD Prevalence"),paste0("Reduction in ",end_year," RHD Prevalence"),
                                  paste0("Percent Reduction in ",end_year," RHD Incidence"),
                                  paste0("Reduction in ",end_year," RHD Incidence"),
                                  paste0("New RHD Cases Averted, 2021-",end_year," (thousands)"),
                                  paste0("Total Cost, 2021-",end_year," (billions)"),paste0("Total Cost (Discounted), 2021-",end_year," (billions)"),
                                  paste0("Full Income Value, 2021-",end_year," (billions)"), paste0("Full Income Value (Discounted), 2021-",end_year," (billions)"),
                                  paste0("Return on Investment, 2021-",end_year,""),paste0("Return on Investment (Discounted), 2021-",end_year,""),
                                  "Cost per Death Averted (thousands)","Cost per Death Averted (Discounted) (thousands)",
                                  paste0("Net Benefit, 2021-",end_year," (billions)"),paste0("Net Benefit (Discounted), 2021-",end_year," (billions)"),
                                  paste0("ARF and RHD Deaths Averted, 2021-",end_year," (thousands)")))]

if (nrow(res[is.na(factvar)]) > 0) stop("missing factvar")

tabout <- copy(res)
tabout[,mean:=round(mean,1)]
tabout <- dcast.data.table(tabout,run_id+run_name+location_name~factvar,value.var="mean")

tabout2 <- copy(res)
tabout2[,mean:=format(round(mean,1),nsmall=1,trim=T)]
tabout2[,lower:=format(round(lower,1),nsmall=1,trim=T)]
tabout2[,upper:=format(round(upper,1),nsmall=1,trim=T)]
tabout2[,res_ui:=paste0(mean," (",lower,"-",upper,")")]

tabout2 <- dcast.data.table(tabout2,run_id+run_name+location_name~factvar,value.var="res_ui")

tabsout <- list(tabout,tabout2)


## make table to show primary prevention intervention comparisons
pharcomp <- copy(tabout2)
pharcomp <- melt(pharcomp,id.vars=c("run_id","run_name","location_name"))
pharcomp[run_id == 34,run_name:="South Africa, All Interventions"]
pharcomp[run_id == 35,run_name:="DRC, All Interventions"]
pharcomp[run_id == 36,run_name:="South Africa, Primary Prevention Only"]
pharcomp[run_id == 37,run_name:="DRC, Primary Prevention Only"]
pharcomp <- dcast.data.table(pharcomp,variable~run_name,value.var=c('value'))
pharcomp <- pharcomp[!variable %in% c("Total Cost (Discounted), 2021-End Year (billions)","Full Income Value (Discounted), 2021-End Year (billions)",
                                      "Return on Investment, 2021-End Year","Cost per Death Averted (thousands)","Net Benefit, 2021-End Year (billions)",
                                      "ARF and RHD Deaths Averted, 2021-End Year (thousands)","RHD Deaths Averted, 2021-End Year (thousands)",
                                      "ARF Deaths Averted, 2021-End Year (thousands)",
                                      "New RHD Cases Averted, 2021-End Year (thousands)","Full Income Value, 2021-End Year (billions)",
                                      "Total Cost, 2021-End Year (billions)","Net Benefit (Discounted), 2021-End Year (billions)")]
pharcomp <- pharcomp[,c("variable","DRC, Primary Prevention Only","South Africa, Primary Prevention Only","DRC, All Interventions","South Africa, All Interventions"),with=F]

writexl::write_xlsx(pharcomp,paste0(outdir,"/results/tables/appendix/country_scenarios.xlsx"))


