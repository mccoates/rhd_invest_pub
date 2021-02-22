## Explore Fiji estimates compared to GBD

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

## load GBD rates
gbd <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_inc_prev_death_inputs.RDS"))
gbd <- map_to_names(gbd,codedir=codedir,keep_ids=T,gbd=2017)
compare <- copy(gbd[measure_name=="Deaths" & location_name=="Fiji" & year == 2010 & cause_name=="Rheumatic heart disease" & age_group_id %in% c(2:20,30:32,235)])
pop <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_pop_aggage.RDS"))
compare <- merge(compare,pop[,c("location_name","sex","age_group_id","year","pop"),with=F],by=c("location_name","sex","age_group_id","year"),all.x=T)


## get both sex
compare[,tot:=val*pop]
compare <- compare[,list(pop=sum(pop),count=sum(tot)),by=c("age_group_id","age_group_name")]
compare[,rate:=count/pop*100000] ## per 100k

## read in estimates extracted from study specifically in fiji (also contains GBD comparison from when the study was published)
## but we want to compare with the 2010 results from GBD 2017 rather than the older GBD
study <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/other_inputs/fiji_study_deathrates.xlsx")))
study <- study[,c("age_group_id","Study"),with=F]
compare <- merge(compare,study,by=c("age_group_id"),all.x=T)

compare[,rat:=Study/rate]
compare[,diff:=Study-rate]

## save for comparison
#write.csv(compare,"[Insert own directory]/rhd_investment_case/results/tables/appendix/fiji_comparison.csv",row.names=F)

## manual adjustments
compare[age_group_id==5,rat:=.01]
compare[age_group_id > 18,rat:=1]
## smooth out the jump in 60-64
compare[age_group_id == 16,rat:=.9]
compare[age_group_id == 17,rat:=.8]

gbd[,val:=val]
gbd[,upper:=upper]
gbd[,lower:=lower]
gbd <- gbd[location_name=="African Union"]
gbd <- dcast.data.table(gbd,sex_id+age_group_id+year+location_name+sex+age_group_name+cause_name~measure_name,value.var="val")
gbd <- merge(gbd,compare[,c("age_group_id","rat"),with=F],by="age_group_id",all.x=T)

## convert to single-year age groups
d <- copy(gbd[location_name=="African Union"])
## extend rates to single-year age groups
d <- d[!age_group_id %in% c(2,3,4,22,27)]
d[age_group_id == 28,age_group_name:="0"]
d[age_group_id==28,age_group_id:=1]
d <- rbind(d,d,d,d,d)
d <- d[order(location_name,year,sex,cause_name,age_group_id)]
d[, id := seq_len(.N), by = c("location_name","year","sex","cause_name","age_group_id")]
d <- d[!(age_group_name=="0" & id > 1) & !(age_group_name=="1 to 4" & id > 4)]
d[,age:=id-1+as.numeric(substr(age_group_name,1,2))]
d <- d[!age %in% c(96:99)]
d[age==0,rat:=1]

d[cause_name=="Rheumatic heart disease",cause_name:="RHD"]
d[cause_name=="All causes",cause_name:="AC"]
d <- dcast.data.table(d,year+sex+age+age_group_name+location_name+sex_id+rat~cause_name,value.var=c("Prevalence","Incidence","Deaths"))
d[,c("Prevalence_All causes","Incidence_All causes"):=NULL]
d[,Deaths_RHD_adj:=Deaths_RHD*rat]

gg <- ggplot(d[year==2017],aes(x=age,y=Deaths_RHD)) + geom_line() + 
  geom_line(data=d[year==2017],aes(x=age,y=Deaths_RHD_adj),color="red") +
  facet_wrap(~sex) + scale_y_continuous(trans="log")
print(gg)

## load HF estiamtes
rhd_inputs <- readRDS(paste0(codedir,"data/gbd_inputs/prepped_hf_rhd_inputs.rds"))
rhd_inputs <- rhd_inputs[location_name=="African Union"]
d <- merge(d,rhd_inputs[,c("age","sex","year","rhd_hf_prev_rate"),with=F],by=c("age","sex","year"),all.x=T)
d[,prev_mild:=Prevalence_RHD-rhd_hf_prev_rate]


gg <- ggplot(d[year==2000],aes(x=age,y=Prevalence_RHD)) + geom_line() + 
  geom_line(data=d[year==2000],aes(x=age,y=prev_mild),color="red") +
  facet_wrap(~sex) + scale_y_continuous(trans="log")
print(gg)


## load populations
pop <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_pop.RDS"))
pop <- pop[location_name=="African Union"]

d <- merge(d,pop[,c("year","sex","age","pop"),with=F],by=c("year","sex","age"),all.x=T)
if (nrow(d[is.na(pop)]) > 0) stop("missing pop")

d[,cd_mort_risk:=(1-exp(-1*Deaths_AC))*(Deaths_AC-Deaths_RHD_adj)/Deaths_AC]
d[,rhd_mort_risk:=.25]
d[,with_cause_mort_risk:=cd_mort_risk+rhd_mort_risk]
if (nrow(d[with_cause_mort_risk > .99]) > 0) stop("with cause mort risk too high")

d[,RHD_death_count:=Deaths_RHD_adj*pop]

sum(d[year==2017]$RHD_death_count)
d[,old_RHD_death_count:=Deaths_RHD*pop]

pdf(paste0(outdir,"/results/figures/appendix/gbd_AU_fiji_adj.pdf"),width=8,height=8)

toplot <- copy(d[year==2017,c("age","sex","year","pop","Deaths_RHD","Deaths_RHD_adj"),with=F])
toplot <- melt(toplot,id.vars=c("age","sex","pop","year"))
toplot[,count:=value*pop]

sum(toplot[variable=="Deaths_RHD_adj"]$count)
sum(toplot[variable=="Deaths_RHD"]$count)

toplot[,variable:=factor(variable,levels=c("Deaths_RHD_adj","Deaths_RHD"))]

gg <- ggplot(toplot,aes(x=age,y=value*100000,group=variable,color=variable)) + geom_line(size=1.2) + theme_bw() +
  facet_wrap(~sex) + scale_color_discrete("GBD Model Estimates",labels=c("GBD 2017, Adjusted","GBD 2017")) +
  ylab("Death Rate (per 100,000), log scale") + xlab("Age") + theme(legend.position = "none") + scale_y_continuous(trans="log",breaks=c(.01,.1,1,10,100),labels = function(x) format(x, scientific = FALSE))
print(gg)

gg2 <- ggplot(toplot,aes(x=age,y=count,group=variable,color=variable)) + geom_line(size=1.2) + theme_bw() +
  facet_wrap(~sex) + scale_color_discrete("GBD Model Estimates",labels=c("GBD 2017, Adjusted","GBD 2017")) +
  ylab("Deaths") + xlab("Age") + theme(legend.position="bottom")#+ scale_y_continuous(trans="log")
print(gg2)

lay <- rbind(c(1,1,1),
             c(1,1,1),
             c(1,1,1),
             c(1,1,1),
             c(1,1,1),
             c(2,2,2),
             c(2,2,2),
             c(2,2,2),
             c(2,2,2),
             c(2,2,2))

grid.arrange(gg,gg2,layout_matrix=lay)

dev.off()






