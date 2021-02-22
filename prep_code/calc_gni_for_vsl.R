## Matthew Coates
## calculate numbers needed for econ calculations


rm(list=ls())
library(data.table)
library(ggplot2)
library(reldist)
library(lme4)
library(ggplot2)
## set directories
if (Sys.info()[1] == 'Windows') {
  codedir <- paste0("[Insert own directory]/rhd_invest_priv/")
  outdir <- paste0("[Insert own directory]/rhd_investment_case/")
} else {
  codedir <- paste0("[Insert own directory]/rhd_invest_priv/")
  outdir <- paste0("[Insert own directory]/rhd_investment_case/")
}

source(paste0(codedir,"/functions/swap_location_names.R"))


## read in AU locations (https://au.int/en/member_states/countryprofiles2)
locs <- data.table(openxlsx::read.xlsx(paste0(outdir,"/data/AU_member_states.xlsx")))
locs <- locs[!location_name %in% c("Sahrawi Republic")] ## we don't have burden estimates for Sahrawi Republic, so we will drop

## convert to GBD country names
locs <- swap_locnames(data=locs,out="GBD",version=2017)
au_locs <- locs$location_name

pop <- data.table(openxlsx::read.xlsx(paste0(outdir,"/data/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx"),startRow=17))
setnames(pop,c("index","variant","location_name","notes","code","type","parent_code",as.character(1950:2020)))
pop <- pop[,c("location_name",as.character(1950:2020)),with=F]
pop2 <- data.table(openxlsx::read.xlsx(paste0(outdir,"/data/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx"),startRow=17,sheet=2))
setnames(pop2,c("index","variant","location_name","notes","code","type","parent_code",as.character(2020:2100)))
pop2 <- pop2[,c("location_name",as.character(2021:2100)),with=F]
pop <- merge(pop,pop2,by=c("location_name"),all.x=T)
pop[location_name=="Cabo Verde",location_name:="Cape Verde"]
pop[location_name=="Gambia",location_name:="The Gambia"]
pop[grepl("Ivoire",location_name),location_name:="Cote d'Ivoire"]
pop[grepl("watini",location_name),location_name:="Swaziland"]
pop[grepl("Tanzania",location_name),location_name:="Tanzania"]
au_locs[!au_locs %in% unique(pop$location_name)]
pop <- pop[location_name %in% au_locs]
pop <- melt(pop,id.vars=c("location_name"),value.name="pop",variable.name="year")
pop[,pop:=as.numeric(as.character(pop))]
pop[,year:=as.numeric(as.character(year))]
pop[,pop:=pop*1000]

## load GNI per capita for the VSL calculation
gni <- fread(paste0(codedir,"/data/other_inputs/API_NY.GNP.PCAP.CD_DS2_en_csv_v2_1218027.csv"),header=T)
gni[,V65:=NULL]
us_gni2015 <- gni[`Country Name`=="United States"]$`2015`
au_locs[!au_locs %in% gni$`Country Name`]
setnames(gni,c("Country Name"),c("location_name"))
gni[location_name=="Gambia, The",location_name:="The Gambia"]
gni[location_name=="Congo, Dem. Rep.",location_name:="Democratic Republic of the Congo"]
gni[location_name=="Congo, Rep.",location_name:="Congo"]
gni[location_name=="Cabo Verde",location_name:="Cape Verde"]
gni[grepl("Egypt",location_name),location_name:="Egypt"]
gni[location_name=="Eswatini",location_name:="Swaziland"]
au_locs[!au_locs %in% gni$location_name]
gni <- gni[location_name %in% c(au_locs)]
gni[is.na(`2019`)]
gni[is.na(`2015`)]
## make South Sudan missing in 2015 so we're using consistent countries
gni[location_name=="South Sudan",`2015`:=NA]
## Eritrea, Somalia, and South Sudan missing GNI pretty far back, so we'll skip those in creating average
## weight by population
gni <- gni[,c("location_name",as.character(1960:2019)),with=F]
gni <- melt(gni,id.vars=c("location_name"),variable.name="year",value.nam="gni")
gni[,year:=as.numeric(as.character(year))]
gni <- merge(gni,pop,by=c("location_name","year"),all.x=T)



## we're just using 2015 and 2019
gni <- gni[year %in% c(2015,2019)]
augni <- copy(gni[,list(gni=weighted.mean(gni,w=pop,na.rm=T)),by=c("year")])
augni[,location_name:="African Union"]

reggni <- copy(gni)
reggni <- merge(gni,locs[,c("location_name","au_region"),with=F],by=c("location_name"),all.x=T)
reggni <- reggni[,list(gni=weighted.mean(gni,w=pop,na.rm=T)),by=c("year","au_region")]
setnames(reggni,"au_region","location_name")

gni[,pop:=NULL]
gni <- rbind(gni,augni,reggni)

addgni <- data.frame(year=2015,gni=us_gni2015,location_name="United States",stringsAsFactors = F)

gni <- rbind(gni,addgni)
gni <- gni[order(location_name,year)]

## South Sudan, SOmalia, Eritrea--need assumptions
gni[location_name=="South Sudan"]$gni <- gni[location_name=="African Union"]$gni*.5 ## 1090 in 2015 (AU 2070)
gni[location_name=="Somalia"]$gni <- gni[location_name=="African Union"]$gni*.2 ## 130 in 1990 (AU < 700)
gni[location_name=="Eritrea"]$gni <- gni[location_name=="African Union"]$gni*.3 ## 600 in 2011 (AU 1834)


write.csv(gni,paste0(codedir,"/data/other_inputs/gni_for_VSL.csv"),row.names = F)




