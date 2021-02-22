## Create plots that summarize more parameters

rm(list=ls()) 
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

run_nums <- c(3,4,5)

for (run_num in run_nums) {
  cat(paste0(run_num,"\n")); flush.console()
  locsshort <- "AU"

    
    ratediff <- readRDS(paste0(outdirtmp,"/results/model_run_results/",run_num,"/ratediff",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".rds"))
    res <- readRDS(paste0(outdirtmp,"/results/model_run_results/",run_num,"/full_results",ifelse(Sys.info()[1]=="Windows",paste0(""),paste0("_",locsshort)),".rds"))

    res <- res[var %in% c("count_inc_arf","count_inc_arf_totalcare","total_10yr_history",
                          "total_10yr_history_sp","incnum","mild_prev_pys","sev_prev_pys","deaths_severe_rhd","num_surgs","deaths_op",
                          "postsurg_pys","deaths_postsurg_rhd")]
    res[,var:=factor(var,levels=c("count_inc_arf","count_inc_arf_totalcare","total_10yr_history",
                                  "total_10yr_history_sp","incnum","mild_prev_pys","sev_prev_pys","deaths_severe_rhd","num_surgs","deaths_op",
                                  "postsurg_pys","deaths_postsurg_rhd"))]
    res <- res[age %in% c("All Ages","Age-standardized") & sex == "both"]


    res <- res[var %in% c("count_inc_arf","total_10yr_history",
                          "total_10yr_history_sp","incnum","mild_prev_pys","sev_prev_pys","deaths_severe_rhd","num_surgs","deaths_op",
                          "postsurg_pys","deaths_postsurg_rhd")]
    res <- dcast.data.table(res,location_name+year+sex+age+int+var+metric+pop_mean+pop_lower+pop_upper~uncert,value.var="value")
    res <- res[year >=2020]
    
    res[,int:=factor(int,levels=c("FALSE","TRUE"),labels=c("No Scale-up of Interventions","Scale-up of Interventions"))]
    res[,var:=factor(var,levels=c("count_inc_arf","total_10yr_history",
                                  "total_10yr_history_sp","incnum","mild_prev_pys","sev_prev_pys","deaths_severe_rhd","num_surgs","deaths_op",
                                  "postsurg_pys","deaths_postsurg_rhd"),
                          labels=c("ARF Incidence","ARF Remission Prevalence","ARF Secondary Prophylaxis",
                                   "RHD Incidence","Mild RHD Prevalence","Severe RHD Prevalence","Deaths from Severe RHD",
                                   "Cardiac Surgeries","Surgical Deaths","Postsurgical Prevalence","Deaths from RHD Post-Surgery"))]
    
    pdf(paste0(outdir,"/results/figures/appendix/model_bins_results",run_num,".pdf"),height=10,width=10)
    gg <- ggplot(data=res[age=="Age-standardized" & metric=="rate"],aes(x=year,y=mean*100000,group=int,linetype=int,color=int)) +
      geom_line() + theme_bw() + facet_wrap(~var,scales="free",nrow=4) + expand_limits(y = 0) + theme(legend.position="bottom") +
      scale_x_continuous("Year",breaks=c(2020,2025,2030,2035,2040)) + geom_ribbon(aes(ymin=lower*100000, ymax=upper*100000,fill=int), alpha=0.1,colour = NA) +
      scale_color_discrete("") + scale_fill_discrete("") + scale_linetype_discrete("") + ylab("Age-standardized Rate (per 100,000)")
      
    print(gg)
    
    gg <- ggplot(data=res[age=="All Ages" & metric=="rate"],aes(x=year,y=mean*100000,group=int,linetype=int,color=int)) +
      geom_line() + theme_bw() + facet_wrap(~var,scales="free",nrow=4) + expand_limits(y = 0) + theme(legend.position="bottom") +
      scale_x_continuous("Year",breaks=c(2020,2025,2030,2035,2040)) + geom_ribbon(aes(ymin=lower*100000, ymax=upper*100000,fill=int), alpha=0.1,colour = NA) +
      scale_color_discrete("") + scale_fill_discrete("") + scale_linetype_discrete("") + ylab("Crude Rate (per 100,000)")
    
    print(gg)
    
    dev.off()
    
}

