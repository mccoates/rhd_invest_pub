## Matthew Coates
## project GNI/GDP


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
  drawnum <- 200
} else {
  codedir <- paste0("[Insert own directory]/rhd_invest_priv/")
  outdir <- paste0("[Insert own directory]/rhd_investment_case/")
  drawnum <- 1000
}
## load functions
source(paste0(codedir,"/functions/swap_location_names.R"))
source(paste0(codedir,"/functions/map_ids_names.R"))
source(paste0(codedir,"/model_code/costing_functions.R"))


## read in AU locations (https://au.int/en/member_states/countryprofiles2)
locs <- data.table(openxlsx::read.xlsx(paste0(outdir,"/data/AU_member_states.xlsx")))
locs <- locs[!location_name %in% c("Sahrawi Republic")] ## we don't have burden estimates for Sahrawi Republic, so we will drop

## convert to GBD country names
locs <- swap_locnames(data=locs,out="GBD",version=2017)
au_locs <- locs$location_name


## here, we load in constant per capita GDP (constant so we get real growth), both in USD and in PPP ID
gdp <- fread(paste0(codedir,"/data/other_inputs/WEO_Data_GDPpc_pppID_USD_constant.csv"))
gdp <- gdp[1:388]
gdp <- gdp[,c("ISO","Country","Units","Estimates Start After",as.character(1990:2024)),with=F]
gdp <- melt(gdp,id.vars=c("ISO","Country","Units","Estimates Start After"))
gdp[Units=="U.S. dollars",Units:="USD"]
gdp[Units=="Purchasing power parity; international dollars",Units:="PPP ID"]
gdp[,value:=as.numeric(gsub(",","",value))]
gdp <- dcast.data.table(gdp,ISO+Country+`Estimates Start After`+variable~Units,value.var = "value")
setnames(gdp,c("iso3","location_name","ests_start","year","GDP_PPP_ID","GDP_USD"))
gdp[,delta_GDP_PPP_ID:=c(diff(gdp$GDP_PPP_ID,lag=1),0)/GDP_PPP_ID]
gdp[,delta_GDP_USD:=c(diff(gdp$GDP_USD,lag=1),0)/GDP_USD]
gdp[year==2024,c("delta_GDP_PPP_ID","delta_GDP_USD"):=NA]

## load in GNI per capita
## PPP ID constant 
gni <- fread(paste0(codedir,"/data/other_inputs/API_NY.GNP.PCAP.PP.KD_DS2_en_csv_v2_1351034.csv"),header=T)
gni[,V65:=NULL]
setnames(gni,c("location_name","iso3","indicator","indicator_code",as.character(1960:2019)))
gni <- melt(gni,id.vars=c("location_name","iso3","indicator","indicator_code"))
gni[,value:=as.numeric(value)]

## constant USD
gni2 <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/other_inputs/API_NY.GNP.PCAP.KD_DS2_en_excel_v2_1350164.xlsx"),startRow=3))
setnames(gni2,c("location_name","iso3","indicator","indicator_code",as.character(1960:2019)))
gni2 <- melt(gni2,id.vars=c("location_name","iso3","indicator","indicator_code"))
gni2[,value:=as.numeric(value)]

setnames(gni,c("variable","value"),c("year","GNI_PPP_ID"))
setnames(gni2,c("variable","value"),c("year","GNI_USD"))
gni[,c("indicator","indicator_code"):=NULL]
gni2[,c("indicator","indicator_code"):=NULL]

gni <- merge(gni,gni2,by=c("location_name","iso3","year"),all=T)
gni[,delta_GNI_PPP_ID:=c(diff(gni$GNI_PPP_ID,lag=1),0)/GNI_PPP_ID]
gni[,delta_GNI_USD:=c(diff(gni$GNI_USD,lag=1),0)/GNI_USD]
gni[,year:=as.numeric(as.character(year))]
gdp[,year:=as.numeric(as.character(year))]
gni <- gni[year >= 1990]

gni[,gni_indic:=1]
gdp[,gdp_indic:=1]

au_locs[!au_locs %in% unique(gni$location_name)]
gni[location_name=="Cabo Verde",location_name:="Cape Verde"]
gni[location_name=="Congo, Rep.",location_name:="Congo"]
gni[location_name=="Congo, Dem. Rep.",location_name:="Democratic Republic of the Congo"]
gni[grepl("Egypt",location_name),location_name:="Egypt"]
gni[grepl("watini",location_name),location_name:="Swaziland"]
gni[grepl("Gambia",location_name),location_name:="The Gambia"]
au_locs[!au_locs %in% unique(gni$location_name)]

au_locs[!au_locs %in% unique(gdp$location_name)]
gdp[location_name=="Cabo Verde",location_name:="Cape Verde"]
gdp[location_name=="Republic of Congo",location_name:="Congo"]
gdp[location_name=="Congo, Dem. Rep.",location_name:="Democratic Republic of the Congo"]
gdp[grepl("Ivoire",location_name),location_name:="Cote d'Ivoire"]
gdp[grepl("watini",location_name),location_name:="Swaziland"]
gdp[grepl("o Tom",location_name),location_name:="Sao Tome and Principe"]
au_locs[!au_locs %in% unique(gdp$location_name)]

d <- merge(gdp,gni,by=c("location_name","iso3","year"),all=T)

au_locs[!au_locs %in% unique(d$location_name)]
d <- d[location_name %in% au_locs]

## load pops -- need to use UNPD for future projections
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

d <- merge(d,pop[,c("location_name","year","pop"),with=F],by=c("year","location_name"),all.x=T)


lm(data=d[year < 2019],formula="delta_GNI_PPP_ID~delta_GDP_PPP_ID")
lm(data=d[year < 2019],formula="delta_GNI_PPP_ID~delta_GDP_PPP_ID + location_name")
lmer(data=d[year < 2019],formula="delta_GNI_PPP_ID~delta_GDP_PPP_ID + (delta_GDP_PPP_ID | location_name)")

lm(data=d[year < 2019],formula="delta_GNI_USD~delta_GDP_USD")
lm(data=d[year < 2019],formula="delta_GNI_PPP_ID~delta_GDP_PPP_ID + location_name")
lmer(data=d[year < 2019],formula="delta_GNI_PPP_ID~delta_GDP_PPP_ID + (delta_GDP_PPP_ID | location_name)")

lm(data=d[year < 2019 & year > 1990],formula="delta_GNI_PPP_ID~delta_GDP_PPP_ID")
lm(data=d[year < 2019 & year > 1990],formula="delta_GNI_PPP_ID~delta_GDP_PPP_ID + location_name")
lmer(data=d[year < 2019 & year > 1990],formula="delta_GNI_PPP_ID~delta_GDP_PPP_ID + (delta_GDP_PPP_ID | location_name)")

lm(data=d[year < 2019 & year > 2000],formula="delta_GNI_PPP_ID~delta_GDP_PPP_ID")
lm(data=d[year < 2019 & year > 2000],formula="delta_GNI_PPP_ID~delta_GDP_PPP_ID + location_name")
lmer(data=d[year < 2019 & year > 2000],formula="delta_GNI_PPP_ID~delta_GDP_PPP_ID + (delta_GDP_PPP_ID | location_name)")
## the coefficient is roughly 0.86 across several specifications

## now, we need projections of GDP per capita in 2019 USD
## so, we should pull GDP per capita in current 2019 USD
## then, we can use the projected real % change in GDP to project forward GDP for each country, then aggregate

## load the 2019 GDP per capita numbers in current USD
gdp <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/other_inputs/API_NY.GDP.PCAP.CD_DS2_en_excel_v2_1345208.xlsx"),sheet=1,startRow=3))
setnames(gdp,c("location_name","iso3","indicator","indicator_code",as.character(1960:2019)))
gdp <- melt(gdp,id.vars=c("location_name","iso3","indicator","indicator_code"),variable.name="year",value.name="gdp")
gdp[,gdp:=as.numeric(gdp)]
gdp[location_name=="Cabo Verde",location_name:="Cape Verde"]
gdp[location_name=="Congo, Rep.",location_name:="Congo"]
gdp[location_name=="Congo, Dem. Rep.",location_name:="Democratic Republic of the Congo"]
gdp[grepl("Ivoire",location_name),location_name:="Cote d'Ivoire"]
gdp[grepl("watini",location_name),location_name:="Swaziland"]
gdp[grepl("o Tom",location_name),location_name:="Sao Tome and Principe"]
gdp[grepl("Egypt",location_name),location_name:="Egypt"]
gdp[grepl("Gambia",location_name),location_name:="The Gambia"]
au_locs[!au_locs %in% unique(gdp$location_name)]
gdp <- gdp[location_name %in% au_locs]
gdp <- gdp[year==2019]
gdp[,year:=as.numeric(as.character(year))]
gdp <- merge(gdp,pop,by=c("location_name","year"),all.x=T)


## first, figure out projections--we already have the projections from october 2019 loaded in that go to 2024
## calculate the AROC from 2019 to 2024 for each country
aroc <- copy(d)
## calculate AROC at the country level so we can apply to GDP
aroc <- aroc[year %in% c(2000,2019,2024)]
aroc <- dcast.data.table(aroc,location_name~year,value.var="GDP_PPP_ID")
aroc[,aroc_short:=log(`2024`/`2019`)/(2024-2019)*100]
aroc[,aroc_long:=log(`2019`/`2000`)/(2019-2000)*100]

## find aroc for whole set--going to use for long term--hard to make individual country projections beyond scope of IMF projections
aroc2 <- copy(d)
aroc2 <- merge(aroc2,locs[,c("location_name","au_region"),with=F],by=c("location_name"),all.x=T)
aroc2 <- aroc2[year %in% c(2000:2019)]
aroc2[is.na(GDP_PPP_ID)]
aroc2 <- aroc2[!location_name %in% c("South Sudan","Somalia")] ## too many years missing, will cause inconsistencies
aroc2 <- aroc2[,list(GDP_PPP_ID=weighted.mean(GDP_PPP_ID,w=pop)),by=c("au_region","year")]
aroc2 <- aroc2[year %in% c(2000,2019)]
aroc2 <- dcast.data.table(aroc2,au_region~year,value.var="GDP_PPP_ID")
aroc2[,aroc_long:=log(`2019`/`2000`)/(2019-2000)*100] ## in range of 1.3 to 3.4

aroc2 <- copy(d)
aroc2 <- aroc2[year %in% c(2000:2019)]
aroc2[is.na(GDP_PPP_ID)]
aroc2 <- aroc2[!location_name %in% c("South Sudan","Somalia")] ## too many years missing, will cause inconsistencies
aroc2 <- aroc2[,list(GDP_PPP_ID=weighted.mean(GDP_PPP_ID,w=pop)),by=c("year")]
aroc2 <- aroc2[year %in% c(2000,2019)]
aroc2[,test:=1]
aroc2 <- dcast.data.table(aroc2,test~year,value.var="GDP_PPP_ID")
aroc2[,aroc_long:=log(`2019`/`2000`)/(2019-2000)*100] ## in range of 1.3 to 3.4
## using 2% across locations in long run
aroc[,aroc_long:=2]

## load the COVID numbers for 2020/2021
covid <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/other_inputs/WEO_Data_april_proj.xlsx"),sheet=1,startRow=1))
covid <- covid[1:194]
covid <- covid[,c("Country","2020","2021"),with=F]
setnames(covid,c("location_name","2020","2021"))
covid
covid[location_name=="Cabo Verde",location_name:="Cape Verde"]
covid[location_name=="Republic of Congo",location_name:="Congo"]
covid[location_name=="Congo, Dem. Rep.",location_name:="Democratic Republic of the Congo"]
covid[grepl("Ivoire",location_name),location_name:="Cote d'Ivoire"]
covid[grepl("watini",location_name),location_name:="Swaziland"]
covid[grepl("o Tom",location_name),location_name:="Sao Tome and Principe"]
au_locs[!au_locs %in% unique(covid$location_name)]
covid <- covid[location_name %in% au_locs]

## these should be adjusted down further based on summer update, but they don't give many specific countries
## the April update to the summer udpate, the SSA numbers changed from -1.6 and 4.1 in April to -3.2 and 3.4
## and the middle east numbers went from -2.8 and 4 to -4.7 and 3.3
## so, we'll reduce each country's 2021 estimate by 0.7 and 2020 estimate by 1.7
covid[,`2020`:=as.numeric(`2020`)-1.7]
covid[,`2021`:=as.numeric(`2021`)-0.7]

## to calculate for AU, need actual GDP by country
gdp <- merge(gdp,covid,by=c("location_name"),all.x=T)
setnames(gdp,"gdp","gdp2019")
gdp[,gdp2020:=gdp2019*(`2020`/100+1)]
gdp[,gdp2021:=gdp2020*(`2021`/100+1)]
gdp <- merge(gdp,aroc[,c("location_name","aroc_short","aroc_long"),with=F],by=c("location_name"),all.x=T)

## then use AROC to calculate country-level GDP projections through 2050
for (i in c(2022:2025)) {
  gdp[,paste0("gdp",i):=exp((i-2021)*(aroc_short/100)+log(gdp2021))]
}

for (i in c(2026:2090)) {
  gdp[,paste0("gdp",i):=exp((i-2025)*(aroc_long/100)+log(gdp2025))]
}

## double check work
gdp[,test:=log(gdp2025/gdp2021)/(2025-2021)*100]
gdp <- gdp[,c("location_name",paste0("gdp",c(2019:2090))),with=F]
gdp <- melt(gdp,id.vars=c("location_name"),variable.name="year",value.name="gdp_pc")
gdp[,year:=as.numeric(as.character(gsub("gdp","",year)))]


augdp <- merge(copy(gdp),pop,by=c("location_name","year"),all.x=T)
augdp <- augdp[,list(gdp_pc=weighted.mean(gdp_pc,w=pop,na.rm=T)),by="year"]
augdp[,location_name:="African Union"]

reggdp <- merge(copy(gdp),pop,by=c("location_name","year"),all.x=T)
reggdp <- merge(reggdp,locs[,c("au_region","location_name"),with=F],by=c("location_name"),all.x=T)
reggdp <- reggdp[,list(gdp_pc=weighted.mean(gdp_pc,w=pop,na.rm=T)),by=c("year","au_region")]
setnames(reggdp,"au_region","location_name")

gdp <- rbind(gdp,augdp,use.names=T)
gdp <- rbind(gdp,reggdp,use.names=T)

gdp <- gdp[order(location_name,year)]
gdp[,pct_change:=c(diff(gdp$gdp_pc),0)/gdp_pc*100] ## while these numbers are weirdly offset, that helps for calc used next.
gdp[year==2090,pct_change:=NA]
gdp[,pct_change_gni:=pct_change*0.86]

gdp <- gdp[,c("location_name","year","gdp_pc","pct_change_gni"),with=F]

write.csv(gdp,paste0(codedir,"/data/other_inputs/projected_gdp_gnichange.csv"),row.names=F)


