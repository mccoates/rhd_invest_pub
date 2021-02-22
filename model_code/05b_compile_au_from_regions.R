## Matthew Coates
## Compile regional estimates into AU estimates (must be done at draw level)


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
manual <- F
###########################
## Options for what to run
if (manual) {
  run_num <- 6
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


############################
## results from 01 -- return if needed, not necessarily needed
############################


############################
## results from 02 
############################
if (specs$end_year < 1960) {
  dis <- list()
  for (i in regions) {
    cat(paste0("loading ",i,"\n")); flush.console()
    locsshort <- gsub(" Africa","",i)
    dis[[paste0(i)]] <- readRDS(paste0(outdirtmp,"/results/model_run_results/",run_num,"/model_results_sims_",locsshort,".rds"))
  }
  dis <- rbindlist(dis)
  dis <- dis[,lapply(.SD,sum,na.rm=T),by=c("sex","age","year","draw"),.SDcols=c(names(dis)[!names(dis) %in% c("location_name","sex","age","year","draw")])]
  dis[,location_name:="African Union"]
} else {
  dis <- list()
  yrs <- c(2017:specs$end_year)
  grps <- floor(1:length(yrs)/((length(yrs)+1)/4))+1
  for (j in c(1:4)) {
    cat(paste0("yr group ",j,"\n")); flush.console()
    dis[[j]] <- list()
    for (i in regions) {
      cat(paste0("loading ",i,"\n")); flush.console()
      locsshort <- gsub(" Africa","",i)
      dis[[j]][[paste0(i)]] <- readRDS(paste0(outdirtmp,"/results/model_run_results/",run_num,"/model_results_sims_",locsshort,".rds"))
      dis[[j]][[paste0(i)]]  <- dis[[j]][[paste0(i)]] [year %in% yrs[grps==j]]
      gc()
    }
    dis[[j]] <- rbindlist(dis[[j]])
    dis[[j]] <- dis[[j]][,lapply(.SD,sum,na.rm=T),by=c("sex","age","year","draw"),
                         .SDcols=c(names(dis[[j]])[!names(dis[[j]]) %in% c("location_name","sex","age","year","draw")])]
    dis[[j]][,location_name:="African Union"]   
    gc()
  }
  dis <- rbindlist(dis)
  gc()  
}


## need ARF numbers for costing for (1) all people on secondary prophylaxis (calculated above)
## new cases (count_inc_arf)
## and new cases on treatment (need to create this somehow from the hist_arf variable in the year of the incident case) (count_inc_arf_totalcare)
dis[,c("count_inc_arf_totalcare","count_inc_arf_totalcare_int"):=0]
for (i in base_year:(base_year+proj_yrs-1)) {
  dis[year==i,count_inc_arf_totalcare:=dis[[paste0("hist_arf_",i,"_sp")]][dis$year==i]]
  dis[year==i,count_inc_arf_totalcare_int:=dis[[paste0("hist_arf_",i,"_sp_int")]][dis$year==i]]
  
}


## reduce size
dis <- dis[,names(dis)[!grepl("hist_arf",names(dis))],with=F]
dis[,paste0("sev_prev",c(1:5)):=NULL]
dis[,c("sev_prev","sev_prev_int"):=NULL]
dis[,paste0("sev_prev_int",c(1:5)):=NULL]
dis[,c("mild_prev","mild_prev_int","postsurg","postsurg_int","tosevere","tosevere_int","regress","regress_int","sev_prev_start","sev_prev_start_int",
       "count_first_arf","count_first_arf_int","count_recur_arf","count_recur_arf_int"):=NULL]
dis[,c("pct_surg","pct_surg_int","staywell","staywell_int"):=NULL]
dis[is.na(arf_death),arf_death:=0]
dis[is.na(arf_death_int),arf_death_int:=0]
gc()


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

saveRDS(outsum,paste0(outdirtmp,"/results/model_run_results/",run_num,"/pct_asrate_reduct_sims",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_AU")),".rds"))
saveRDS(outsum2,paste0(outdirtmp,"/results/model_run_results/",run_num,"/counts_averted_sims",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_AU")),".rds"))

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
saveRDS(res,paste0(outdirtmp,"/results/model_run_results/",run_num,"/full_results",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_AU")),".rds"))
#res <- readRDS(paste0("/model_run_results/",run_num,"/full_results.rds"))
gc()


ratediff <- copy(dis[sex=="both" & age %in% c("Age-standardized","All Ages") & var %in% c("total_prev","deaths_cause","incnum"),c("location_name","year","sex","age","draw","int","var","rate"),with=F])
ratediff <- dcast.data.table(ratediff,location_name+year+sex+age+draw+var~int,value.var="rate")
ratediff[,diff:=`TRUE`-`FALSE`]
ratediff[,pct_diff:=(`TRUE`-`FALSE`)/`FALSE`*100]
ratediff <- ratediff[,list(rate_diff_mean=mean(diff),rate_diff_lower=quantile(diff,probs=c(.025)),rate_diff_upper=quantile(diff,probs=c(.975)),
                           rate_pct_diff_mean=mean(pct_diff),rate_pct_diff_lower=quantile(pct_diff,probs=c(.025)),rate_pct_diff_upper=quantile(pct_diff,probs=c(.975))),by=c("location_name","year","sex","age","var")]

saveRDS(ratediff,paste0(outdirtmp,"/results/model_run_results/",run_num,"/ratediff_AU.rds"))


test <- copy(res[sex=="both" & year==2030 & age=="Age-standardized" & var=="deaths_cause" & metric=="rate"])
test

test <- copy(res[sex=="both" & year==2030 & age=="All Ages" & var=="total_prev" & metric=="count"])
test

test <- copy(res[sex=="both" & year==2030 & age=="Age-standardized" & var=="deaths_cause" & metric=="rate"])
test

rm(dis); gc()

##################################
## Summary plots
##################################
totplot <- copy(res[age %in% c("All Ages","Age-standardized") & var %in% c("mild_prev_pys","sev_prev_pys","postsurg_pys","deaths_cause") & sex %in% c("both") & metric %in% c("count","rate")])
totplot[,var:=factor(var,levels=c("mild_prev_pys","sev_prev_pys","postsurg_pys","deaths_cause"),labels=c("Mild RHD","RHD with HF","Post-surgical RHD","Deaths from RHD"))]
#totplot[,sex:=factor(sex,levels=c("female","male"),labels=c("Female","Male"))]
totplot[,int:=factor(int,levels=c("FALSE","TRUE"),labels=c("No Scale-up of Interventions","Scale-up of Interventions"))]
totplot <- dcast.data.table(totplot,location_name+year+sex+age+int+var+metric~uncert,value.var=c("value"))


pdf(paste0(outdir,"/results/model_run_results/",run_num,"/summary_results_diagnostics_uncert",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_AU")),".pdf"),width=12,height=8)

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



## TO MAKE THIS ONE, TOTAL CASES NEED TO BE COMBINED AT THE DRAW LEVEL--CREATE THIS CATEGORY EARLIER
totplot2 <- copy(res[age %in% c("All Ages","Age-standardized") & var %in% c("total_prev","deaths_cause","incnum") & sex %in% c("both") & metric %in% c("count","rate")])
totplot2[,var:=factor(var,levels=c("incnum","total_prev","deaths_cause"),labels=c("Incident RHD Cases","Total RHD Cases","Deaths from RHD"))]
totplot2[,int:=factor(int,levels=c("FALSE","TRUE"),labels=c("No Scale-up of Interventions","Scale-up of Interventions"))]
totplot2 <- dcast.data.table(totplot2,location_name+year+sex+age+int+var+metric~uncert,value.var=c("value"))

ratediff[,var:=factor(var,levels=c("incnum","total_prev","deaths_cause"),labels=c("Incident RHD Cases","Total RHD Cases","Deaths from RHD"))]


pdf(paste0(outdir,"/results/model_run_results/",run_num,"/summary_results_uncert",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_AU")),".pdf"),width=8,height=4)

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


pdf(paste0(outdir,"/results/model_run_results/",run_num,"/summary_results_uncert_decline",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_AU")),".pdf"),width=8,height=6)

for (i in unique(totplot$location_name)) {
  
  plot.new()
  text(x=.5, y=.5, i)  
  
  
  gg1 <- ggplot(data=totplot2[age=="Age-standardized" & metric == "rate" & year >=2020 & location_name==i],aes(x=year,y=mean*100000,group=int,color=int,linetype=int)) + 
    geom_line() + theme_bw() + facet_wrap(~var,scales="free") + #ggtitle("Age-standardized Rates of Parameters, per 100,000") + 
    expand_limits(y = 0) + scale_color_discrete("") + scale_fill_discrete("") + scale_linetype_discrete("") +
    ylab("Age-Standardized Rate per 100,000") + scale_x_continuous("Year",breaks=c(2020,2025,2030,2035,2040,2045,2050)) + theme(legend.position="bottom") + 
    geom_ribbon(aes(ymin=lower*100000, ymax=upper*100000,fill=int), alpha=0.1,colour = NA)
  
  print(gg1)
  
  gg <- ggplot(data=ratediff[age=="Age-standardized" & year >=2020 & location_name==i],aes(x=year,y=rate_pct_diff_mean)) + 
    geom_line() + theme_bw() + facet_wrap(~var,scales="fixed") + #ggtitle("Age-standardized Rates of Parameters, per 100,000") + 
    expand_limits(y = 0) + scale_color_discrete("") + scale_fill_discrete("") + scale_linetype_discrete("") +
    ylab("Percent Difference in Rate") + scale_x_continuous("Year",breaks=c(2020,2025,2030,2035,2040,2045,2050)) + theme(legend.position="bottom") + 
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

pdf(paste0(outdir,"/results/model_run_results/",run_num,"/death_breakdown_diagnostics_uncert",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_AU")),".pdf"),width=12,height=8)

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
# toplot2 <- copy(pop)
# toplot2[,age_group:=floor(age/5)*5]
# toplot2 <- toplot2[,list(pop=sum(pop)),by=c("location_name","year","sex","age_group","int","draw")]
# toplot2 <- toplot2[,list(mean=mean(pop),lower=quantile(pop,probs=c(.025)),upper=quantile(pop,probs=c(.975))),by=c("location_name","year","sex","age_group","int")]

pdf(paste0(outdir,"/results/model_run_results/",run_num,"/pop_breakdown_age_year",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_AU")),".pdf"),width=12,height=8)

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
  
  # gg <- ggplot(data=toplot2[sex=="male"],aes(x=year,y=mean/1000,group=int,color=int,linetype=int)) + 
  #   geom_line() + theme_bw() + facet_wrap(~age_group) +  ggtitle(paste0(i," Population, Thousands")) + expand_limits(y = 0) +
  #   ylab("Rate") + scale_x_continuous("Year",breaks=c(2020,2025,2030,2035,2040)) + theme(legend.position="bottom") + 
  #   geom_ribbon(aes(ymin=lower/1000, ymax=upper/1000,fill=int), alpha=0.1,colour = NA)
  # 
  # print(gg)
  
}

dev.off()


rm(toplot); rm(toplot2); rm(ratediff); gc()

############################
## results from 03
############################
totcost <- list()
for (i in regions) {
  cat(paste0("loading ",i,"\n")); flush.console()
  locsshort <- gsub(" Africa","",i)
  totcost[[paste0(i)]] <- readRDS(paste0(outdirtmp,"/results/model_run_results/",run_num,"/full_cost_sims_",locsshort,".rds"))
}
totcost <- rbindlist(totcost)

totcost <- totcost[,list(cost=sum(cost),cost_discounted=sum(cost_discounted)),by=c("year","draw","sex","int","shared_intervention")]
totcost[,location_name:="African Union"]

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
saveRDS(totcost,paste0(outdirtmp,"/results/model_run_results/",run_num,"/cost",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_AU")),".rds"))



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


saveRDS(outsum,paste0(paste0(outdir,"/results/model_run_results/",run_num,"/cost_summary",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_AU")),".rds")))



cols <- brewer.pal(n=8,"Set1")[-6]

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols <- gg_color_hue(7)
cols <- c("#DE9151","#0B132B","#1C2541","#3A506B","#9C914F","#748E54")
cols <- c("#DE9151","#0B132B","#3A506B","steelblue2","#9C914F","#748E54")
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


pdf(paste0(outdir,"/results/model_run_results/",run_num,"/costing_stacked_bars",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_AU")),".pdf"),width=9,height=6)

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



pdf(paste0(outdir,"/results/model_run_results/",run_num,"/costing_totals",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_AU")),".pdf"),width=6,height=5)

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

############################
## results from 04
############################
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
                       net_benefit=sum(net_benefit,na.rm=T),net_benefit_discounted=sum(net_benefit_discounted,na.rm=T)),by=c("draw")]
netben[,roi:=full_income_plus/cost]
netben[,roi_discounted:=full_income_plus_discounted/cost_discounted]

netben <- melt(netben,id.vars=c("draw"))

saveRDS(netben,paste0(outdirtmp,"/results/model_run_results/",run_num,"/monetization_summary_draws_AU.rds"))


netben <- netben[,list(mean=mean(value),lower=quantile(value,probs=c(.025)),median=quantile(value,probs=c(0.5)),
                       upper=quantile(value,probs=c(.975))),by=c("variable")]
netben[,discounted:=""]
netben[grepl("discounted",variable),discounted:="discounted"]
netben[,variable:=gsub("_discounted","",variable)]
netben <- dcast.data.table(netben,variable~discounted,value.var=c("mean","lower","median","upper"))
setnames(netben,c("mean_","lower_","upper_","median_"),c("mean","lower","upper","median"))

netben[variable=="net_benefit",type:="Full-income Net Benefit"]
netben[variable=="roi",type:="Full-income ROI"]
netben[variable=="full_income_plus",type:="Full-income plus"]
netben[variable=="full_income",type:="Full-income"]
netben[,location_name:="African Union"]
netben <- netben[variable %in% c("net_benefit","roi","full_income_plus","full_income"),c("location_name","type","mean","mean_discounted","lower","lower_discounted",
                                                                                         "median","median_discounted","upper","upper_discounted"),with=F]

saveRDS(netben,paste0(outdir,"/results/model_run_results/",run_num,"/monetization_summary_AU.rds"))


netben <- list()
for (i in regions) {
  cat(paste0("running ",i,"\n")); flush.console()
  locsshort <- gsub(" Africa","",i)
  netben[[paste0(i)]] <- readRDS(paste0(outdirtmp,"/results/model_run_results/",run_num,"/cum_monetization_summary_draws_",locsshort,".rds"))
}
netben <- rbindlist(netben)

netben <- netben[,list(vlw=sum(vlw,na.rm=T),vlw_discounted=sum(vlw_discounted,na.rm=T),gdp=sum(gdp,na.rm=T),
                       gdp_discounted=sum(gdp_discounted,na.rm=T),full_income=sum(full_income,na.rm=T),full_income_discounted=sum(full_income_discounted,na.rm=T),
                       arf=sum(arf,na.rm=T),arf_discounted=sum(arf_discounted,na.rm=T),full_income_plus=sum(full_income_plus,na.rm=T),
                       full_income_plus_discounted=sum(full_income_plus_discounted,na.rm=T),cost=sum(cost,na.rm=T),
                       cost_discounted=sum(cost_discounted_diff,na.rm=T)),by=c("draw","year")]
netben[,location_name:="African Union"]


netben <- netben[year >= 2021]
netben[,cum_full_income_plus:=cumsum(full_income_plus),by=c("location_name","draw")]
netben[,cum_full_income_plus_discounted:=cumsum(full_income_plus_discounted),by=c("location_name","draw")]
netben[,cumcost:=cumsum(cost),by=c("location_name","draw")]
netben[,cumcost_discounted:=cumsum(cost_discounted),by=c("location_name","draw")]
netben[,cum_roi:=cum_full_income_plus/cumcost]
netben[,cum_roi_discounted:=cum_full_income_plus_discounted/cumcost_discounted]
netben

saveRDS(netben,paste0(outdirtmp,"/results/model_run_results/",run_num,"/cum_monetization_summary_draws_AU.rds"))

roi1_summ <- netben[,list(mean=mean(cum_roi),lower=quantile(cum_roi,probs=c(.025)),upper=quantile(cum_roi,probs=c(.975)),median=quantile(cum_roi,probs=c(.5)),
                        mean_discounted=mean(cum_roi_discounted),lower_discounted=quantile(cum_roi_discounted,probs=c(.025)),upper_discounted=quantile(cum_roi_discounted,probs=c(.975)),
                        median_discounted=quantile(cum_roi_discounted,probs=c(.5))),by=c("location_name","year")]
roi1_summ
roi1_summ[,type:="Full-income ROI"]


pdf(paste0(outdir,"/results/model_run_results/",run_num,"/benefit_cost_ratio_cumulative.pdf"),width=8,height=4)

gg <- ggplot(data=roi1_summ,aes(x=year,y=mean_discounted)) + 
  geom_line() + theme_bw() + #facet_wrap(~var,scales="fixed") + #ggtitle("Age-standardized Rates of Parameters, per 100,000") + 
  expand_limits(y = 0) + scale_color_discrete("") + scale_fill_discrete("") + scale_linetype_discrete("") +
  ylab("Benefit-Cost Ratio (cumulative)") + scale_x_continuous("Year",breaks=c(2020,2030,2040,2050,2060,2070,2080,2090)) + theme(legend.position="bottom") + 
  geom_ribbon(aes(ymin=lower_discounted, ymax=upper_discounted), alpha=0.1,colour = NA) + scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10))
print(gg)

dev.off()

