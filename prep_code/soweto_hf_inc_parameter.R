## Matthew Coates
## Use heart of soweto data to derive transition probabilities for heart failure

rm(list=ls())
library(data.table)

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


############################################
## Load and format GBD estimates
############################################

gbd <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_inc_prev_death_inputs.RDS"))
gbd <- map_to_names(gbd,codedir=codedir,keep_ids=T,gbd=2017)
gbd <- gbd[!location_name %in% c("Fiji")] ## fiji in dataset for other purposes
gbd <- dcast.data.table(gbd,location_id+sex_id+age_group_id+year+cause_id+location_name+sex+age_group_name+cause_name~measure_name,value.var="val")
gbd <- gbd[location_name=="South Africa" & cause_name=="Rheumatic heart disease" & year == 2007 & age_group_id %in% c(5:20,30:32,235)]

## merge with more detailed RHD HF info
rhd_inputs <- readRDS(paste0(codedir,"data/gbd_inputs/prepped_hf_rhd_inputs.rds"))
rhd_inputs <- rhd_inputs[location_name == "South Africa" & year == 2007]
rhd_inputs[,age_group_name:=paste0(floor(age/5)*5," to ",floor(age/5)*5+4)]
rhd_inputs[age_group_name=="95 to 99",age_group_name:="95 plus"]
rhd_inputs[age %in% c(1:4),age_group_name:="1 to 4"]
rhd_inputs <- rhd_inputs[age!=0]
rhd_inputs <- unique(rhd_inputs[,c("location_name","sex","year","cause_name","rei_name","rhd_hf_prev_rate","age_group_name"),with=F])

## implied HF incidence from GBD estimates alone
gbd_inc <- readRDS(paste0(codedir,"/data/other_inputs/implied_hf_incidence.RDS"))
gbd_inc <- gbd_inc[year==2017]
gbd_inc <- unique(gbd_inc[,c("sex","age_group_name","inc_implied","cases_mild_persistent_prev_yr_per_pop"),with=F])
gbd_inc[age_group_name=="95 to 99",age_group_name:="95 plus"]
rhd_inputs <- merge(rhd_inputs,gbd_inc,by=c("sex","age_group_name"),all.x=T)
setnames(rhd_inputs,"inc_implied","gbd_au_inc_implied")

gbd <- merge(gbd,rhd_inputs[,c("location_name","sex","year","age_group_name","cause_name","rhd_hf_prev_rate","gbd_au_inc_implied","cases_mild_persistent_prev_yr_per_pop"),with=F],by=c("location_name","age_group_name","sex","year","cause_name"),all.x=T)

## load in pops
pop <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_pop_aggage.RDS"))
gbd <- merge(gbd,pop[,c("age_group_id","location_name","year","sex","pop"),with=F],by=c("age_group_id","year","location_name","sex"),all.x=T)
gbd[,rhd_no_hf_pyr:=Prevalence-rhd_hf_prev_rate]

## load Heart of Soweto RHD w/ HF incidence numbers
gbd[age_group_name %in% c("0 to 0","1 to 4"),HoS_hf_inc_perpop:=gbd_au_inc_implied*rhd_no_hf_pyr] ## These ages not available from HoS data, use GBD implied
gbd[age_group_name %in% c("5 to 9","10 to 14"),HoS_hf_inc_perpop:=.005*rhd_no_hf_pyr] ## make around same level as 15-19
gbd[age_group_name %in% c("15 to 19"),HoS_hf_inc_perpop:=30/100000]
gbd[age_group_name %in% c("20 to 24","25 to 29"),HoS_hf_inc_perpop:=15/100000]
gbd[age_group_name %in% c("30 to 34","35 to 39"),HoS_hf_inc_perpop:=17/100000]
gbd[age_group_name %in% c("40 to 44","45 to 49"),HoS_hf_inc_perpop:=31/100000]
gbd[age_group_name %in% c("50 to 54","55 to 59"),HoS_hf_inc_perpop:=39/100000]
gbd[age_group_name %in% c("60 to 64","65 to 69","70 to 74","75 to 79",
                          "80 to 84","85 to 89","90 to 94","95 plus"),HoS_hf_inc_perpop:=54/100000]
gbd <- gbd[order(sex,age_group_id)]
gbd[,hf_HoS_implied:=HoS_hf_inc_perpop*pop]
gbd[,gbd_hf_rhd_num:=rhd_no_hf_pyr*pop*gbd_au_inc_implied]
gbd[,tot_gbd_hf:=sum(gbd_hf_rhd_num),by=c("sex")]
gbd[,tot_HoS_hf:=sum(hf_HoS_implied),by=c("sex")]
gbd[,gbd_mild_rhd_num:=Incidence*pop+cases_mild_persistent_prev_yr_per_pop*pop]
gbd[,c("age_group_name","sex","hf_HoS_implied","gbd_hf_rhd_num"),with=F]
gbd[,scaled_HoS_implied:=hf_HoS_implied]
gbd[,constrained:=F]
gbd <- gbd[order(sex,age_group_id)]

gbd[,c("age_group_name","sex","gbd_hf_rhd_num","hf_HoS_implied"),with=F]

## constrain ages below 15-19 so not affected by rescaling
gbd[age_group_name %in% c("1 to 4","5 to 9","10 to 14"),constrained:=T]


catch <- T
count <- 1
## scale total HF incidence to that from GBD in while loop with transition probabilities capped at 10%
## this repeated scaling not necessary under current setup, but was necessary in previous versions with other constraints enforced
while(catch) {  
  
  # if (count == 1) {
  #   gbd[scaled_HoS_implied > gbd_mild_rhd_num*.1,constrained:=T]  
  #   gbd[scaled_HoS_implied > gbd_mild_rhd_num*.1,scaled_HoS_implied:=gbd_mild_rhd_num*.1]      
  # }

  gbd[,tot_HoS_hf:=sum(scaled_HoS_implied),by=c("sex")]
  gbd[,tot_gbd_hf:=sum(gbd_hf_rhd_num),by=c("sex")]
  gbd[constrained==T,tot_HoS_not_changing:=sum(scaled_HoS_implied),by=c("sex")]
  gbd[sex=="male",tot_HoS_not_changing:=max(gbd[sex=="male" & !is.na(tot_HoS_not_changing)]$tot_HoS_not_changing)]
  gbd[sex=="female",tot_HoS_not_changing:=max(gbd[sex=="female" & !is.na(tot_HoS_not_changing)]$tot_HoS_not_changing)]
  gbd[,scalar:=(tot_gbd_hf-tot_HoS_not_changing)/(tot_HoS_hf-tot_HoS_not_changing)] ## scale the remaining age groups to the total GBD minus the capped

  gbd[constrained==F,scaled_HoS_implied:=scaled_HoS_implied*scalar]    
  if (nrow(gbd[scaled_HoS_implied > gbd_mild_rhd_num*.1]) > 0) stop("implied prob > 10%")
  
  gbd[,tot_HoS_hf:=sum(scaled_HoS_implied),by=c("sex")]
  gbd[,scalar:=tot_gbd_hf/tot_HoS_hf]

  if (all(gbd$scalar==1)) catch <- F
  count <- count + 1
}



gbd[,hf_transition_prob:=scaled_HoS_implied/gbd_mild_rhd_num]

gg <- ggplot(gbd,aes(x=age_group_name,y=hf_transition_prob,color=sex,group=sex)) + theme_bw() + geom_line()
print(gg)


gg <- ggplot(gbd,aes(x=factor(age_group_id),y=scaled_HoS_implied,color=sex,group=sex)) + theme_bw() + geom_line()
print(gg)


gg <- ggplot(gbd,aes(x=factor(age_group_id),y=gbd_hf_rhd_num,color=sex,group=sex)) + theme_bw() + geom_line()
print(gg)


gg <- ggplot(gbd[age_group_id < 18],aes(x=age_group_name,y=gbd_au_inc_implied*100,color=sex,group=sex)) + theme_bw() + geom_line()
print(gg)


gg <- ggplot(gbd[age_group_id < 18],aes(x=age_group_name,y=hf_transition_prob*100,color=sex,group=sex)) + theme_bw() + geom_line()
print(gg)


gbd[,c("age_group_name","sex","gbd_au_inc_implied","hf_transition_prob"),with=F]


gbd[,location_name:="African Union"]
gbd <- rbind(gbd,gbd,gbd,gbd,gbd)
gbd <- gbd[order(location_name,sex,cause_name,age_group_id,year)]
gbd[, id := seq_len(.N), by = c("location_name","sex","cause_name","age_group_id")]
gbd <- gbd[!(age_group_name=="0" & id > 1) & !(age_group_name=="1 to 4" & id > 4)]
gbd[,age:=id-1+as.numeric(substr(age_group_name,1,2))]
gbd <- gbd[!age %in% c(96:99)]
add <- copy(gbd[age==1])
add[,age:=0]
add[,hf_transition_prob:=0]
gbd <- rbind(gbd,add)
gbd <- gbd[order(location_name,sex,cause_name,age,year)]


implied_hf_inc <- readRDS(paste0(codedir,"/data/other_inputs/implied_hf_incidence.RDS"))
implied_hf_inc <- implied_hf_inc[year==2017]
setnames(implied_hf_inc,c("inc_implied"),c("inc_implied_old"))
gbd <- merge(gbd[,c("location_name","sex","sex_id","age","hf_transition_prob"),with=F],implied_hf_inc,by=c("location_name","sex","sex_id","age"),all=T)

gg <- ggplot(implied_hf_inc,aes(x=age,y=cases_severe_persistent_prev_yr_per_pop,group=sex,color=sex)) + theme_bw() + geom_line()
print(gg)


## attempt to use ratio of 
gbd[,rat:=hf_transition_prob/inc_implied_old]
gbd[,cases_severe_persistent_prev_yr_per_pop_adj:=cases_severe_persistent_prev_yr_per_pop*rat]
gbd[is.na(cases_severe_persistent_prev_yr_per_pop_adj),cases_severe_persistent_prev_yr_per_pop_adj:=0]
gbd[,implied_severe_persistent_adj:=cases_severe_persistent_prev_yr_per_pop_adj*pop]
gbd[,implied_severe_persistent:=cases_severe_persistent_prev_yr_per_pop*pop]
gbd[,total_implied_severe_persistent_adj:=sum(implied_severe_persistent_adj),by=c("sex")]
gbd[,total_implied_severe_persistent:=sum(implied_severe_persistent),by=c("sex")]

## issue is 63,000 (GBD) versus 879,000 (new) for women
## and 48,000 versus 414,000 for men
## scale down
gbd[,scalar:=total_implied_severe_persistent/total_implied_severe_persistent_adj]
gbd[,total_implied_severe_persistent_adj:=total_implied_severe_persistent_adj*scalar]
gbd[,cases_severe_persistent_prev_yr_per_pop_adj:=cases_severe_persistent_prev_yr_per_pop_adj*scalar]

gbd[,c("inc_implied_old","cases_severe_persistent_prev_yr_per_pop","rat"):=NULL]
setnames(gbd,c("cases_severe_persistent_prev_yr_per_pop_adj"),c("cases_severe_persistent_prev_yr_per_pop"))
gbd <- gbd[,c("location_name","sex","age","sex_id","pop","hf_transition_prob","cases_mild_persistent_prev_yr_per_pop","cases_severe_persistent_prev_yr_per_pop"),with=F]
setnames(gbd,"hf_transition_prob","inc_implied_HoS")


## now we have transition probs we're going to use
## run from 2000 to 2017 to get a more reasonable age pattern (and level) of persistent cases
h <- readRDS(paste0(codedir,"/data/other_inputs/full_inc_cases_long_calcs.RDS"))
h[,inc_implied:=inc_hf_implied/cases_mild]
## derive ratio by which to adjust our HoS based estimates
rat_year <- copy(h[year==2017,c("age","sex","inc_implied"),with=F])
setnames(rat_year,c("inc_implied"),("inc_implied2017"))
h <- merge(h,rat_year,by=c("age","sex"),all.x=T)
h[,rat_adj:=inc_implied/inc_implied2017]
h[is.na(rat_adj),rat_adj:=1]
## merge on new estimates of hf prob
h <- merge(h,gbd[,c("age","sex","inc_implied_HoS"),with=F],by=c("age","sex"),all.x=T)
h[,inc_implied_HoS_adj:=inc_implied_HoS*rat_adj]



## to do this simulation, need to be clear on parameters
## start with num_mild_persistent_last_year from 2001
## add incidence of mild
## then get number leaving mild, which is cause-subtracted deaths, incidence of HF, and regression for those under age 19
g <- copy(h)
setnames(g,c("num_mild_persistent_last_year","prev_year_leftover_cases"),c("rhd_no_hf_persistent_last","rhd_hf_persistent_last"))
for (i in c(2002:2016)) {
  
  ## replace the leftover values for years after 2001 as we run through
  #if (i > 2001) {
  if (i == 2002) {
    mergeon <- copy(g[year==(i-1),c("year","age","sex","rhd_no_hf_persistent_last","rhd_hf_persistent_last"),with=F])
    setnames(mergeon,c("rhd_no_hf_persistent_last","rhd_hf_persistent_last"),c("rhd_no_hf_persistent","rhd_hf_persistent"))
  }
  if (i > 2002) mergeon <- copy(g[year==(i-1),c("year","age","sex","rhd_no_hf_persistent","rhd_hf_persistent"),with=F])
  mergeon[,year:=year+1]
  mergeon[,age:=age+1]
  mergeon[age == 96,age:=95]
  mergeon <- mergeon[,list(rhd_no_hf_persistent_sub=sum(rhd_no_hf_persistent),rhd_hf_persistent_sub=sum(rhd_hf_persistent)),by=c("year","age","sex")]

  ## sub on leftovers from previous year for next calculations
  g <- merge(g,mergeon,by=c("year","age","sex"),all.x=T)
  g[age==0,c("rhd_no_hf_persistent_sub","rhd_hf_persistent_sub"):=0]
  g[year==i,rhd_no_hf_persistent_last:=rhd_no_hf_persistent_sub]
  g[year==i,rhd_hf_persistent_last:=rhd_hf_persistent_sub]
  
  g[,c("rhd_no_hf_persistent_sub","rhd_hf_persistent_sub"):=NULL]
  
  #}
  
  ## recalculate leftover values by recalculating things each year
  regression <- exp((log(.15)+log(.01))/2)
  g[year==i,rhd_no_hf:=rhd_no_hf_persistent_last+rhd_inc_count]
  g[year==i,rhd_hf:=rhd_hf_persistent_last+rhd_no_hf*inc_implied_HoS_adj]
  g[year==i,rhd_hf_persistent:=rhd_hf-rhd_hf*with_cause_qx]  
  g[year==i,num_leaving_mild:=(mx_all-mx_rhd)*(rhd_no_hf)+rhd_no_hf*inc_implied_HoS_adj]
  g[year==i & age<=19,num_leaving_mild:=(mx_all-mx_rhd)*(rhd_no_hf)+rhd_no_hf*inc_implied_HoS_adj+regression*rhd_no_hf] ## under 20, incorporate regression to normal
  g[num_leaving_mild > rhd_no_hf,num_leaving_mild:=rhd_no_hf*.95]
  g[year==i,rhd_no_hf_persistent:=rhd_no_hf-num_leaving_mild]

}

g[year==2016,c("age","sex","rhd_no_hf_persistent","rhd_hf_persistent"),with=F]
g[,old:=0]
compare <- copy(h[year==2017,c("age","sex","num_mild_persistent_last_year","prev_year_leftover_cases"),with=F])
compare[,old:=1]
setnames(compare,c("num_mild_persistent_last_year","prev_year_leftover_cases"),c("rhd_no_hf_persistent","rhd_hf_persistent"))
compare <- rbind(compare,g[year==2016,c("age","sex","rhd_no_hf_persistent","rhd_hf_persistent","old"),with=F])
compare[,old:=as.logical(old)]

gg <- ggplot(compare[sex=="female"],aes(x=age,y=rhd_no_hf_persistent,group=old,color=old)) + geom_line()
print(gg)

gg <- ggplot(compare[sex=="male"],aes(x=age,y=rhd_hf_persistent,group=old,color=old)) + geom_line()
print(gg)

gg <- ggplot(g[sex=="male"],aes(x=age,y=rhd_hf_persistent,group=year,color=year))+ geom_line()
print(gg)

gg <- ggplot(g[sex=="male"],aes(x=age,y=rhd_no_hf_persistent,group=year,color=year))+ geom_line()
print(gg)



sum(g[year==2016]$rhd_hf_persistent)


replace <- copy(g[year==2016,c("age","sex","rhd_no_hf_persistent","rhd_hf_persistent","prevalence_rhd"),with=F])
setnames(replace,c("rhd_no_hf_persistent","rhd_hf_persistent"),c("cases_mild_persistent_prev_yr_per_pop","cases_severe_persistent_prev_yr_per_pop"))

gbd[,c("cases_mild_persistent_prev_yr_per_pop","cases_severe_persistent_prev_yr_per_pop"):=NULL] 
gbd <- merge(gbd,replace,by=c("sex","age"),all.x=T)
## make per prev to extend to country level
gbd[,cases_mild_persistent_prev_yr_per_prev:=cases_mild_persistent_prev_yr_per_pop/pop/prevalence_rhd]
gbd[,cases_severe_persistent_prev_yr_per_prev:=cases_severe_persistent_prev_yr_per_pop/pop/prevalence_rhd]
setnames(gbd,"inc_implied_HoS","inc_implied")
gbd[,c("cases_mild_persistent_prev_yr_per_pop","cases_severe_persistent_prev_yr_per_pop","prevalence_rhd"):=NULL]

saveRDS(gbd,paste0(codedir,"/data/other_inputs/soweto_hf_prob_est.RDS"))


