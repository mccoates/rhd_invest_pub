## Matthew Coates
## Prep GBD inputs, save as smaller file that can be stored in Git repo

rm(list=ls())
root <- "M:/"
laptop <- F
library(data.table)
library(ggplot2)
library(EnvStats)
library(stringr)
library(gtools)
library(scales)
library(grid)
library(gridExtra)

################################################################
## set directories, load functions, load metadata
################################################################
if (Sys.info()[1] == 'Windows') {
  codedir <- paste0("[Insert own directory]/rhd_invest_priv/")
  outdir <- paste0("[Insert own directory]/rhd_investment_case/")
} else {
  codedir <- paste0("[Insert own directory]/rhd_invest_priv/")
  outdir <- paste0("[Insert own directory]/rhd_investment_case/")
}
indir <- paste0(outdir,"/data/")

## load functions
source(paste0(codedir,"/functions/swap_location_names.R"))
source(paste0(codedir,"/functions/map_ids_names.R"))

specs <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/run_key.xlsx")))

base_year <- 2017
proj_yrs <- max(specs$end_year)-base_year+1 

## read in AU locations (https://au.int/en/member_states/countryprofiles2)
locs <- data.table(openxlsx::read.xlsx(paste0(outdir,"/data/AU_member_states.xlsx")))
locs <- locs[!location_name %in% c("Sahrawi Republic")] ## we don't have burden estimates for Sahrawi Republic, so we will drop

## convert to GBD country names
locs <- swap_locnames(data=locs,out="GBD",version=2017)


################################################################
## mortality, prevalence, incidence
################################################################
## this file is not in git repo--this code formats it--but if you would like this data, please reach out via GitHub mccoates
fls <- dir("[Insert own directory]/GBD2017/rhd_extras/AU_normal_data_2000_2017",pattern=".csv")
gbd <- list()
for (fl in fls) {
  gbd[[fl]] <- fread(paste0("[Insert own directory]/GBD2017/rhd_extras/AU_normal_data_2000_2017/",fl))
}
gbd <- rbindlist(gbd)

setnames(gbd,c("measure_id","location_id","sex_id","age_group_id","cause_id","metric_id","year","val","upper","lower"))
gbd <- map_to_names(gbd,codedir=codedir,keep_ids=T,gbd=2017)

## loaded in rates per 100k
gbd[,val:=val/100000]
gbd[,upper:=upper/100000]
gbd[,lower:=lower/100000]

## reshape to fill in zeros
gbd <- dcast.data.table(gbd,location_id+sex_id+age_group_id+year+metric_id+cause_id+location_name+sex+age_group_name+cause_name~measure_name,value.var=c("val","lower","upper"))
gbd[is.na(val_Deaths),val_Deaths:=0]
gbd[is.na(lower_Deaths),lower_Deaths:=0]
gbd[is.na(upper_Deaths),upper_Deaths:=0]
gbd <- melt(gbd,id.vars=c("location_id","sex_id","age_group_id","year","metric_id","cause_id","location_name","sex","age_group_name","cause_name"))
gbd[,uncert:=str_split_fixed(variable, "_", Inf)[,1]]
gbd[,measure_name:=str_split_fixed(variable, "_", Inf)[,2]]
gbd[,variable:=NULL]
gbd <- dcast.data.table(gbd,location_id+sex_id+age_group_id+year+metric_id+cause_id+location_name+sex+age_group_name+cause_name+measure_name~uncert,value.var=c("value"))

## no need to keep prev and inc for all causes
gbd <- gbd[!(cause_name %in% ("All causes") & measure_name %in% c("Prevalence","Incidence"))]

## reduce size for saving
gbd <- gbd[location_name %in% c(locs$location_name,"African Union","Fiji")] ## including Fiji for comparison 
gbd[,c("location_name","age_group_name","cause_name","sex","metric_id"):=NULL]
gbd <- map_to_ids(d=gbd,codedir=codedir,delete=F,gbd=2017)

saveRDS(gbd,paste0(codedir,"/data/gbd_inputs/gbd_inc_prev_death_inputs.RDS"))



##############################################
## populations
##############################################

pop <- list()
for (i in c("IHME_GBD_2017_POP_2015_2017_Y2018M11D08","IHME_GBD_2017_POP_2010_2014_Y2018M11D08","IHME_GBD_2017_POP_2005_2009_Y2018M11D08","IHME_GBD_2017_POP_2000_2004_Y2018M11D08")) {
  ## this file is not in git repo--this code formats it--but if you would like this data, please reach out via GitHub mccoates
  
  pop[[i]] <- fread(paste0("[Insert own directory]/GBD2017/population/",i,".CSV"))
  pop[[i]] <- pop[[i]][age_group_name %in% c("<1 year",as.character(1:94),"95 plus") & location_name %in% c(locs$location_name,"African Union","Fiji") & sex_id %in% unique(c(gbd$sex_id))]
  pop[[i]][,sex:=tolower(sex_name)]
  ## duplicates in <1 year
  pop[[i]][,c("upper","lower"):=NULL]
  pop[[i]] <- unique(pop[[i]])
  pop[[i]] <- pop[[i]][order(sex,age_group_id)]
  setnames(pop[[i]],"year_id","year")
  ## fix names
  if (i != "IHME_GBD_2017_POP_2015_2017_Y2018M11D08") setnames(pop[[i]],"val","pop")
}

pop <- rbindlist(pop)
pop <- pop[,c("location_name","sex","age_group_name","year","pop"),with=F]
pop[age_group_name == "<1 year",age_group_name:="0"]
pop[age_group_name == "95 plus",age_group_name:="95"]
pop[,age:=as.numeric(age_group_name)]
pop[,age_group_name:=NULL]
pop <- pop[,c("location_name","year","sex","age","pop"),with=F]

## add regions
pop <- merge(pop,locs[,c("location_name","au_region"),with=F],by=c("location_name"),all.x=T)

addregs <- copy(pop[!is.na(au_region)])
addregs <- addregs[,list(pop=sum(pop)),by=c("au_region","year","sex","age")]
setnames(addregs,"au_region","location_name")
pop[,au_region:=NULL]
pop <- rbind(pop,addregs)

pop <- pop[order(location_name,year,sex,age)]

saveRDS(pop,paste0(codedir,"/data/gbd_inputs/gbd_pop.RDS"))



## alternatively, some processing requires the normal GBD age groups
pop2 <- list()
for (i in c("IHME_GBD_2017_POP_2015_2017_Y2018M11D08","IHME_GBD_2017_POP_2010_2014_Y2018M11D08","IHME_GBD_2017_POP_2005_2009_Y2018M11D08","IHME_GBD_2017_POP_2000_2004_Y2018M11D08")) {
  ## this file is not in git repo--this code formats it--but if you would like this data, please reach out via GitHub mccoates
  
  pop2[[i]] <- fread(paste0("[Insert own directory]/GBD2017/population/",i,".CSV"))
  pop2[[i]] <- pop2[[i]][age_group_id %in% c(2:20,30:32,235) & location_name %in% c(locs$location_name,"African Union","Fiji") & sex_id %in% unique(c(gbd$sex_id))]
  pop2[[i]][,sex:=tolower(sex_name)]
  ## duplicates in <1 year in one of the datasets
  pop2[[i]][,c("upper","lower"):=NULL]
  pop2[[i]] <- unique(pop2[[i]])
  pop2[[i]] <- pop2[[i]][order(sex,age_group_id)]
  setnames(pop2[[i]],"year_id","year")
  ## fix names inconsistent in one of the datasets
  if (i != "IHME_GBD_2017_POP_2015_2017_Y2018M11D08") setnames(pop2[[i]],"val","pop")
}

pop2 <- rbindlist(pop2)
pop2 <- pop2[,c("location_name","sex","age_group_id","year","pop"),with=F]

## add regions
pop2 <- merge(pop2,locs[,c("location_name","au_region"),with=F],by=c("location_name"),all.x=T)

addregs <- copy(pop2[!is.na(au_region)])
addregs <- addregs[,list(pop=sum(pop)),by=c("au_region","year","sex","age_group_id")]
setnames(addregs,"au_region","location_name")
pop2[,au_region:=NULL]
pop2 <- rbind(pop2,addregs)


pop2 <- pop2[order(location_name,year,sex,age_group_id)]

saveRDS(pop2,paste0(codedir,"/data/gbd_inputs/gbd_pop_aggage.RDS"))


###########################################
## make AU regions from GBD data
###########################################
gbdreg <- map_to_names(gbd,codedir=codedir,keep_ids=T,gbd=2017)
gbdreg <- merge(gbdreg,locs[,c("location_name","au_region"),with=F],by=c("location_name"),all.x=T)
gbdreg <- gbdreg[!is.na(au_region)]
gbdreg <- merge(gbdreg,pop2,by=c("location_name","year","age_group_id","sex"),all.x=T)
gbdreg <- dcast.data.table(gbdreg,location_name+year+age_group_id+sex+location_id+measure_id+sex_id+age_group_name+au_region+measure_name+pop~cause_name,value.var="val")
gbdreg <- gbdreg[,list(`All causes`=weighted.mean(`All causes`,w=pop),
                       `Rheumatic heart disease`=weighted.mean(`Rheumatic heart disease`,w=pop)),by=c("au_region","year","age_group_id","sex","measure_id","sex_id","age_group_name","measure_name")]
gbdreg <- melt(gbdreg,id.vars=c("au_region","year","age_group_id","sex","measure_id","sex_id","age_group_name","measure_name"),variable.name="cause_name",value.name="val")
gbdreg <- map_to_ids(gbdreg,codedir=codedir,delete_id=F,gbd=2017)
gbdreg[,c("age_group_name","measure_name","sex"):=NULL]
setnames(gbdreg,"au_region","location_name")

saveRDS(gbdreg,paste0(codedir,"/data/gbd_inputs/gbd_inc_prev_death_inputs_region.RDS"))

###########################################
## fertility
###########################################
## this file is not in git repo--this code formats it--but if you would like this data, please reach out via GitHub mccoates
fert <- fread(paste0("[Insert own directory]/GBD2017/population/IHME_GBD_2017_FERT_ESTIMATES_1950_2017_Y2018M11D08.CSV"))
fert[,sex:=tolower(sex_name)]
fert <- fert[location_name %in% c(locs$location_name,"African Union") & year_id == 2017]

## add regions
fertregs <- merge(copy(fert),locs[,c("location_name","au_region"),with=F],by=c("location_name"),all.x=T)
fertregs <- fertregs[!is.na(au_region)]
setnames(fertregs,"year_id","year")
fertregs <- merge(fertregs,pop2,by=c("location_name","year","age_group_id","sex"),all.x=T)
fertregs <- fertregs[,list(val=weighted.mean(val,w=pop)),by=c("au_region","year","age_group_id","sex","sex_id","sex_name","age_group_name","measure_id","measure_name","metric_name")]
setnames(fertregs,c("au_region","year"),c("location_name","year_id"))
fert <- rbind(fert,fertregs,fill=T)

saveRDS(fert,paste0(codedir,"/data/gbd_inputs/gbd_fert.RDS"))



######################################
## extra GBD estimates on IHD used in created adjusted death numbers
######################################
## this file is not in git repo--this code formats it--but if you would like this data, please reach out via GitHub mccoates
ihd <- fread("[Insert own directory]/GBD2017/rhd_extras/ihd_cvd_other/IHME-GBD_2017_DATA-bd3899f3-1.csv")
setnames(ihd,c("measure_id","location_id","sex_id","age_group_id","cause_id","metric_id","year","val","upper","lower"))

saveRDS(ihd,paste0(codedir,"/data/gbd_inputs/gbd_ihd.RDS"))



#########################################
## GBD HF impairment estimates
#########################################
## this file is not in git repo--this code formats it--but if you would like this data, please reach out via GitHub mccoates
fls <- dir("[Insert own directory]/GBD2017/rhd_extras/impairment_data_2000_2017",pattern=".csv")
d <- list()
## downloaded the impairment data for heart failure from RHD by age, sex, location
for (fl in fls) {
  d[[fl]] <- fread(paste0("[Insert own directory]/GBD2017/rhd_extras/impairment_data_2000_2017/",fl))
}
d <- rbindlist(d)
setnames(d,c("measure_id","location_id","sex_id","age_group_id","cause_id","impairment_num","metric_id","year","val","upper","lower"))

## downloaded overall impairment numbers, but we don't need that
d <- d[!cause_id %in% 294]

## merge on names of impairments
inames <- fread("[Insert own directory]/GBD2017/rhd_extras/impairment_data_2000_2017/key/IHME-GBD_2017_DATA-b4627fbd-1.csv")
setnames(inames,"rei_id","impairment_num")
inames <- inames[,c("rei_name","impairment_num"),with=F]

d <- map_to_names(d,codedir=codedir,gbd=2017,keep_ids = T)
d <- merge(d,inames,by=c("impairment_num"),all=T)
d <- d[location_name %in% c(locs$location_name,"African Union")]

d[age_group_id == 12 & sex_id == 1 & location_name=="African Union" & year == 2017]

# pct_treated <- copy(d[age_group_id == 22 & rei_name %in% c("Heart failure","Treated heart failure")])
# pct_treated <- dcast.data.table(pct_treated,sex_id + location_name + year + age_group_id ~ rei_name,value.var="val")
# pct_treated[,pct_treated:=`Treated heart failure`/`Heart failure`*100]
# pct_treated[location_name=="African Union"]
# summary(pct_treated$pct_treated)
# ## very unrealistic 37% of HF treated in every single country...

d <- d[rei_name=="Heart failure" & age_group_id %in% c(2:20,30:32,235)]
d[age_group_id == 2,age_group_name:="0"] ## the rate under-1 is 0, so just use early neonatal for it

## extend rates to single-year age groups
d <- d[!age_group_id %in% c(3,4)]
d <- rbind(d,d,d,d,d)
d <- d[order(location_name,sex,cause_name,age_group_id,year)]
d[, id := seq_len(.N), by = c("location_name","sex","cause_name","age_group_id","year")]
d <- d[!(age_group_name=="0" & id > 1) & !(age_group_name=="1 to 4" & id > 4)]
d[,age:=id-1+as.numeric(substr(age_group_name,1,2))]
d <- d[!age %in% c(96:99)]

pop <- pop[location_name %in% unique(d$location_name)]

d <- merge(d,pop[,c("location_name","age","sex","year","pop"),with=F],by=c("location_name","age","sex","year"),all=T)
if (nrow(d[is.na(pop)]) > 0) stop("missing pop")

d[,prev_count:=val/100000*pop]
setnames(d,c("val","prev_count"),c("rhd_hf_prev_rate","rhd_hf_prev_count"))

d <- d[,c("location_name","age","sex","year","cause_name","rei_name","pop","rhd_hf_prev_rate","rhd_hf_prev_count"),with=F]
d[,rhd_hf_prev_rate:=rhd_hf_prev_rate/100000]

## add regions in
# addregs <- copy(d[location_name!="African Union"])
# addregs <- merge(addregs,locs[,c("au_region","location_name"),with=F],by=c("location_name"),all.x=T)
# if (any(is.na(addregs$au_region))) stop("missing region")
# addregs <- addregs[,list(pop=sum(pop),rhd_hf_prev_count=sum(rhd_hf_prev_count)),by=c("au_region","age","sex","year","cause_name","rei_name")]
# addregs[,rhd_hf_prev_rate:=rhd_hf_prev_count/pop]
# setnames(addregs,c("au_region"),c("location_name"))
# d <- rbind(d,addregs)

d <- d[,c("location_name","age","sex","year","cause_name","rei_name","rhd_hf_prev_rate","rhd_hf_prev_count"),with=F]


saveRDS(d,paste0(codedir,"data/gbd_inputs/prepped_hf_rhd_inputs.rds"))


################################
## GBD UHC INDEX
################################
## this file is not in git repo--this code formats it--but if you would like this data, please reach out via GitHub mccoates

uhc <- fread("[Insert own directory]/rhd_investment_case/data/IHME_GBD_2019_UHC_1990_2019_VALUES_Y2020M011D19.CSV")
uhc[,location_name:=NULL]
uhc <- map_to_names(uhc,codedir=codedir,gbd=2017)
uhc <- uhc[location_name %in% c(locs$location_name) & year_id %in% c(2019)]
setnames(uhc,"year_id","year")
uhc[,c("measure","loc_level","lower","upper"):=NULL]
saveRDS(uhc,paste0(codedir,"/data/gbd_inputs/GBD_UHC_index.RDS"))

