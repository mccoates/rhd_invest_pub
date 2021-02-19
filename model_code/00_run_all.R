## Matthew Coates
## Run all script for RHD modeling

rm(list=ls())
library(data.table)
library(openxlsx)

## make run flexibly--locally or on cluster if doing for multiple locations
if (Sys.info()[1] == 'Windows') {
  codedir <- paste0("[Insert own directory here]/rhd_invest_priv/")
  outdir <- paste0("[Insert own directory here]/rhd_investment_case/")
  outdirtmp <- outdir ## this is just for storing files on the cluster with many draws, not backed up--if on windows, just storing in normal location (will be fewer draws if running locally)
} else {
  codedir <- paste0("[Insert own directory here]/rhd_invest_priv/")
  outdir <- paste0("[Insert own directory here]/rhd_investment_case/")
  outdirtmp <- paste0("[Insert own directory here]")
}
source(paste0(codedir,"/functions/call_bsub.R"))

##########################################
## Settings for which models to run
##########################################
## Which version(s) of the model to run (see scenarios in run_key)
torun <- c(1)
## Which step of the code to start at (code separated into 4 parts)
## just if troubleshooting--should normally be 1 unless commenting out parts below--this just deletes outputs to make sure
## the newly saved outputs are new
step_start <- c(1)

##########################################


##########################################
## Save runlist to use as reference for which should run
##########################################
## Model specifications--spreadsheet with specifications for model runs
specs <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/run_key.xlsx")))

if (torun == "all") torun <- specs$run_id
runlist <- data.frame(runs=torun,stringsAsFactors = F)
write.csv(runlist,paste0(outdir,"/results/runlist.csv"))

##########################################


##########################################
## Delete old results from code we are rerunning to
## ensure that older results do not get interpreted as newly run
##########################################
for (i in torun) {
  
  file.remove(paste0(outdir,"/results/model_run_results/",i,"/run",i,".csv"))

  if (step_start <= 1) {
    file.remove(paste0(outdirtmp,"/results/model_run_results/",i,"/projected_phar_arf_params.rds"))
    file.remove(paste0(outdirtmp,"/results/model_run_results/",i,"/arf_history.rds"))
  }

  if (step_start <= 2) {
    file.remove(paste0(outdirtmp,"/results/model_run_results/",i,"/arf_epi.rds"))
    file.remove(paste0(outdirtmp,"/results/model_run_results/",i,"/deaths.rds"))
    file.remove(paste0(outdirtmp,"/results/model_run_results/",i,"/counts_averted_sims.rds"))
    file.remove(paste0(outdirtmp,"/results/model_run_results/",i,"/full_results_sims.rds"))
    file.remove(paste0(outdir,"/results/model_run_results/",i,"/summary_results_diagnostics_uncert.pdf"))
    file.remove(paste0(outdir,"/results/model_run_results/",i,"/summary_results_uncert.pdf"))
    file.remove(paste0(outdir,"/results/model_run_results/",i,"/death_breakdown_diagnostics_uncert.pdf"))
    file.remove(paste0(outdir,"/results/model_run_results/",i,"/summary_results_uncert_decline.pdf"))
    file.remove(paste0(outdir,"/results/model_run_results/",i,"/pop_breakdown_age_year.pdf"))
    file.remove(paste0(outdirtmp,"/results/model_run_results/",i,"/pct_asrate_reduct_sims.rds"))
    file.remove(paste0(outdirtmp,"/results/model_run_results/",i,"/hf_yrs.rds"))
  }
  
  if (step_start <= 3) {
    file.remove(paste0(outdir,"/results/model_run_results/",i,"/cost_summary.rds"))
    file.remove(paste0(outdir,"/results/model_run_results/",i,"/costing_stacked_bars.pdf"))
    file.remove(paste0(outdir,"/results/model_run_results/",i,"/costing_totals.pdf"))
    file.remove(paste0(outdirtmp,"/results/model_run_results/",i,"/cost.rds"))
  }
  
  if (step_start <= 4) {
    file.remove(paste0(outdir,"/results/model_run_results/",i,"/monetization_summary.rds"))
  }
  
  
}

##########################################


##########################################
## Run the code, looping over the different specifications to run
##########################################

## if running locally, it's serial, so run within this code
## if running on cluster, do parallel by location, so submit a different piece of code that runs each step

if (Sys.info()[1] == 'Windows') {
    
  keeprunning <- T
  
  while (keeprunning==T) {
    
    ## this loop determines whether to keep running based on whether outputs have been generated
    ## from the runs expected based on the specifications input
    
    ## get list being run
    runlist <- fread(paste0(outdir,"/results/runlist.csv")) 
    
    ## generate indicator for those which have already run
    for (i in runlist$runs) {
      runlist[runs==i,check:=file.exists(paste0(outdir,"/results/model_run_results/",i,"/monetization_summary.rds"))]
    }
    
    ## determine next to run based on those without results
    nextrun <- runlist[check==F]$runs[1]
    
    ## load model specifications for the next model to run
    specs <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/run_key.xlsx")))
    cat(paste0("RUNNING ",nextrun,"\n")); flush.console()
    specsrun <- copy(specs[run_id == nextrun])
    
    
    ## some specifications should be mutually exclusive, create error if those are selected
    ## this is actually not an option in the published results--no longer considered transport costs for GAS pharyngitis infection in costing
    if (specsrun$community_based_gas == "Yes" & specsrun$gas_transport_cost == "Yes") stop("conflicting options")
    
    ## create output directory for results for this particular model
    dir.create(paste0(outdir,"/results/model_run_results/",nextrun))
    dir.create(paste0(outdirtmp,"/results/model_run_results/",nextrun))
    
    ###########################################
    ## Run steps
    ###########################################
    
    ## create temporary file that indicates the active run (to pass info to the code)
    write.csv(specsrun,paste0(outdir,"/results/model_runs/run",nextrun,".csv"),row.names=F)
    write.csv(specsrun,paste0(outdir,"/results/model_run_results/",nextrun,"/run",nextrun,".csv"),row.names=F) ## to archive settings
    
    ## Step 1 & 2: Run Model
    source(paste0(codedir,"/model_code/01_pharyngitis_arf_estimation_prep.R"),verbose=T)
    cat(paste0("Ran 1\n")); flush.console()
  
    source(paste0(codedir,"/model_code/02_rhd_model_uncert.R"),verbose=T)
    cat(paste0("Ran 2\n")); flush.console()
  
    ## Step 3: Run Costing Code
    source(paste0(codedir,"/model_code/03_costing.R"),verbose=T)
    cat(paste0("Ran 3\n")); flush.console()
  
    ## step 4: run health impact monetization code
    source(paste0(codedir,"/model_code/04_health_impact_monetization.R"),verbose=T)
    cat(paste0("Ran 4\n")); flush.console()
    
    
    ## get runlist
    runlist <- fread(paste0(outdir,"/results/runlist.csv"))
    
    ## check which have run
    for (i in runlist$runs) {
      runlist[runs==i,check:=file.exists(paste0(outdir,"/results/model_run_results/",i,"/monetization_summary.rds"))]
    }
    nextrun <- runlist[check==T]$runs[length(runlist[check==T]$runs)] ## take the last of the ones that have completed
    
    file.remove(paste0(outdir,"/results/model_runs/run",nextrun,".csv"))
    
    ## if all have run, then stop the loop
    keeprunning <- T
    if (all(runlist$check==T)) keeprunning <- F
    
  }


} else {
  
  ## loop over scenarios to run
  ## get list being run
  runlist <- fread(paste0(outdir,"/results/runlist.csv")) 
  
  ## load specs for those 
  specs <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/run_key.xlsx")))
  specs <- specs[run_id %in% runlist$runs]
  
  ## right now, there are two specifications for locations to run: AU, and regs
  ## if anything else, flag as bad input
  if (length(specs$runloc[!specs$runloc %in% c("AU","regs","SA","DRC")]) > 0) stop("wrong location")
  
  for (scenario in unique(specs$run_id)) {
    
    ## create output directory for results for this particular model
    dir.create(paste0(outdir,"/results/model_run_results/",scenario))
    dir.create(paste0(outdirtmp,"/results/model_run_results/",scenario))
    
    if (specs[run_id == scenario]$runloc=="AU") locs <- c("AU")
    if (specs[run_id == scenario]$runloc=="regs") locs <- c("Central","Eastern","Northern","Southern","Western")
    if (specs[run_id == scenario]$runloc=="DRC") locs <- c("DRC")
    if (specs[run_id == scenario]$runloc=="SA") locs <- c("SA")
    
    for (loc in locs) {
      ## loop over locations
      args <- paste0(loc,"_",scenario)
      
      
      ## delete files
      fls <- dir(paste0(outdir,"/results/model_run_results/",scenario,"/"),pattern=loc)
      for (fl in c(fls)) {
        file.remove(paste0(outdir,"/results/model_run_results/",scenario,"/",fl))
      }
      
      fls <- dir(paste0(outdirtmp,"/results/model_run_results/",scenario,"/"),pattern=loc)
      for (fl in c(fls)) {
        file.remove(paste0(outdirtmp,"/results/model_run_results/",scenario,"/",fl))
      }
      
      
      
      ## for run to 2030 with 1000 draws, 1 location, recorded 24 to 30G memory max (use 40 by default for safety), and about 28 minutes
      ## change the memory specification for other specs
      memspec <- 40
      timelim <- "1:00"
      if (specs[run_id==scenario]$end_year > 2030 & specs[run_id==scenario]$end_year < 2050) memspec <- 80
      if (specs[run_id==scenario]$end_year > 2030 & specs[run_id==scenario]$end_year < 2050) timelim <- "1:40"
      if (specs[run_id==scenario]$end_year > 2049) memspec <- 150 
      if (specs[run_id==scenario]$end_year > 2049) timelim <- "2:30"
      if (specs[run_id==scenario]$end_year > 2080) memspec <- 225
      if (specs[run_id==scenario]$end_year > 2080) timelim <- "8:00"
      if (specs[run_id==scenario]$drawnum == 4000) memspec <- 250
      if (specs[run_id==scenario]$drawnum == 4000) timelim <- "11:30"
      
      ## this method of submitting jobs will be specific to a particular computing cluster--will have to be modified to run
      call_bsub(jobname=paste0("rhd_",scenario,"_",loc),queue="short",runtime=timelim,mem=memspec,cores=1,mpitype=NA,err=paste0("[Insert own filepath for standard error output]/rhd_",scenario,"_",loc,".txt"),
                out=paste0("[Insert own filepath for standard output]/rhd_out_",scenario,"_",loc,".txt"),notify=F,
                shell="in_bsub_function",
                code="[insert own filepath for repository on cluster computing environment]/rhd_invest_priv/model_code/00b_cluster_run_scen.R",args=args,test=F)
    }
    

  }

  
}







