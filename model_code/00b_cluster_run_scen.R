## Matthew Coates
## Run RHD model code on cluster

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
  outdirtmp <- outdir ## this is just for storing files on the cluster with many draws, on local machine, these are the same (just can't run with as many draws)
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
  run_num <- 22
  specs <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/run_key.xlsx")))
  specs <- specs[run_id == run_num]
} else {
  run_num <- str_split(as.character(commandArgs()[3]),pattern="_",n=Inf,simplify=T)[,2]
  specs <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/run_key.xlsx")))
  specs <- specs[run_id == run_num]
  mloc <- str_split(as.character(commandArgs()[3]),pattern="_",n=Inf,simplify=T)[,1]
}
if (Sys.info()[1] == 'Windows' & drawnum != specs$drawnum) specs$drawnum <- drawnum
drawnum <- specs$drawnum


## now, scenario (run_num) has been passed
## location has been passed
## run appropriate steps in code


########################
## step 1 gas/arf input
########################
commandArgs <- function(...) c(paste0(mloc),paste0(run_num))
source(paste0(codedir,"/model_code/01_pharyngitis_arf_estimation_prep.R"),verbose=T)

## remove objects from workspace that we do not need
rm(list=setdiff(ls(),c("mloc","run_num","codedir","specs")))

########################
## step 2 main model
########################
commandArgs <- function(...) c(paste0(mloc),paste0(run_num))
source(paste0(codedir,"/model_code/02_rhd_model_uncert.R"),verbose=T)

## remove objects from workspace that we do not need
rm(list=setdiff(ls(),c("mloc","run_num","codedir","specs")))

########################
## step 3 costing
########################
commandArgs <- function(...) c(paste0(mloc),paste0(run_num))
source(paste0(codedir,"/model_code/03_costing.R"),verbose=T)

## remove objects from workspace that we do not need
rm(list=setdiff(ls(),c("mloc","run_num","codedir","specs")))

########################
## step 4 monetization
########################
commandArgs <- function(...) c(paste0(mloc),paste0(run_num))
source(paste0(codedir,"/model_code/04_health_impact_monetization.R"),verbose=T)

## remove objects from workspace that we do not need
rm(list=setdiff(ls(),c("mloc","run_num","codedir","specs")))





