## Matthew Coates
## Run on the cluster the code that compiles regional numbers and creates AU numbers

rm(list=ls())
library(data.table)
library(openxlsx)

## make run flexibly--locally or on cluster if doing for multiple countries
if (Sys.info()[1] == 'Windows') {
  codedir <- paste0("[Insert own directory here]/rhd_invest_priv/")
  outdir <- paste0("[Insert own directory here]/rhd_investment_case/")
  outdirtmp <- outdir ## this is just for storing files on the cluster with many draws, not backed up
} else {
  codedir <- paste0("[Insert own directory here]/rhd_invest_priv/")
  outdir <- paste0("[Insert own directory here]/rhd_investment_case/")
  outdirtmp <- paste0("[Insert own directory here]")
}
source(paste0(codedir,"/functions/call_bsub.R"))

##########################################
## Settings for which models to run
##########################################
## Which version(s) of the model to run
torun <- c(29)


## load specs for those 
specs <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/run_key.xlsx")))
specs <- specs[run_id %in% torun]


for (scenario in unique(specs$run_id)) {

  args <- paste0("AU_",scenario)
    
  
  ## delete files
  fls <- dir(paste0(outdir,"/results/model_run_results/",scenario,"/"),pattern="_AU")
  for (fl in c(fls)) {
    file.remove(paste0(outdir,"/results/model_run_results/",scenario,"/",fl))
  }
  
  fls <- dir(paste0(outdirtmp,"/results/model_run_results/",scenario,"/"),pattern="_AU")
  for (fl in c(fls)) {
    file.remove(paste0(outdirtmp,"/results/model_run_results/",scenario,"/",fl))
  }
  
  
  ## for run to 2030 with 1000 draws, 1 location, recorded 24 to 30G memory max (use 40 by default for breathing room), and about 28 minutes
  ## change the memory specification for other specs
  memspec <- 55
  timelim <- "0:40"
  if (specs[run_id==scenario]$end_year > 2030 & specs[run_id==scenario]$end_year < 2050) memspec <- 120
  if (specs[run_id==scenario]$end_year > 2030 & specs[run_id==scenario]$end_year < 2050) timelim <- "1:00"
  if (specs[run_id==scenario]$end_year > 2049) memspec <- 180
  if (specs[run_id==scenario]$end_year > 2049) timelim <- "1:30"
  if (specs[run_id==scenario]$end_year > 2080) memspec <- 250
  if (specs[run_id==scenario]$end_year > 2080) timelim <- "8:00"
  
  call_bsub(jobname=paste0("rhd_aucomp_",scenario),queue="short",runtime=timelim,mem=memspec,cores=1,mpitype=NA,err=paste0("[Insert own directory here]/au_comp_",scenario,".txt"),
            out=paste0("[Insert own directory here]/au_comp_out_",scenario,".txt"),notify=F,
            shell="in_bsub_function",
            code="[Insert own directory here]/rhd_invest_priv/model_code/05b_compile_au_from_regions.R",args=args,test=F)

}





