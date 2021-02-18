## Matthew Coates
## write bsub function that puts together a bsub string based on inputs we want



call_bsub <- function(jobname="unnamed",queue="short",runtime="2:00:00",mem=4,cores=2,mpitype=NA,err=NA,out=NA,notify=F,holds=NULL,
                             shell="",code="codepath.R",args=NA,test=T) {
  ## queue options:
  ## mpi (parallel),priority (if only a couple jobs),mcore (if need 20 cores and long runtime),short (12 cores max and 12 hours),long (1 month, 12 cores),
  ## mini (1 core, 10 mins)
  queue <- paste0("-p ",queue)
  
  ## runtime options:
  ## hr:min
  if (!is.character(runtime)) stop("Make runtime character in format hr:min")
  runtime <- paste0("-t ",runtime,":00")
  
  ## memory options
  ## in numeric GB
  if (!is.numeric(mem)) stop("make mem numeric in number of GB")
  mem <- paste0("--mem ",mem,"G")
  
  ## number of cores
  ## cores, just count cores
  if (!is.numeric(cores)) stop("make cores numeric in number of cores")
  cores <- paste0("-n ",cores)
  
  ## job name
  if (jobname=="unnamed") stop("name your job")
  jobname <- paste0("-J ",jobname)
  
  ## type of mpi run if mpi
  if (!is.na(mpitype)) stop("option not yet set up")
  
  ## holds
  if (!is.null(holds)) holds <- paste0("-w ",paste(paste0('ended("',holds,'")'),collapse=" && "))
  if (is.null(holds)) holds <- ""
  
  
  ## error path
  err <- ifelse(is.na(err),"",paste0("-e ",err))
  
  ## output path
  out <- ifelse(is.na(out) | out == "","",paste0("-o ",out))
  
  ## notification
  ## true or fale above, gets pasted below if true
  notify <- ifelse(notify==T,"-N","")
  
  args <- ifelse(is.na(args[1]),"",paste(args,collapse=" "))
  #print(args)
  
  stop("need to modify code here for use--change directory")
  shellpath <- paste0("/[INSERT HERE THE DESIRED DIRECTORY FOR SHELL]/shells/",gsub("-J ","",jobname),".sh")
  file.remove(shellpath)
  ## create new version of the shell
  sink(shellpath)
  #testing sink("C:/Users/MMC33/Documents/test_shell.sh")
  cat("#!/bin/bash \n")
  cat(paste(paste0("#SBATCH ",queue),jobname,runtime,mem,cores,err,out,notify,sep="\n#SBATCH "))
  cat("\nmodule load gcc/6.2.0")
  cat("\nmodule load R/3.6.1")
  cat(paste0("\n/n/app/R/3.6.1/bin/R < ",code," --no-save ",args," \n"))
  sink()
  
  cat( readLines(shellpath), sep = "\n" )
  call <- paste0("sbatch ",shellpath)

  if (test == F) system(call)
}

