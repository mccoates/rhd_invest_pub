#######
## Matthew Coates
## create ratios of 2017 mortality and fertility to projections
## these ratios will be applied to GBD 2017 numbers for the projection

rm(list=ls())
library(data.table)
library(stringr)
library(ggplot2)
library(openxlsx)


## set directories
if (Sys.info()[1] == 'Windows') {
  codedir <- paste0("[Insert own directory]/rhd_invest_priv/")
  datadir <- paste0("[Insert own directory]/rhd_investment_case/data/")
  outdir <- paste0("[Insert own directory]/rhd_invest_priv/data/")
} else {
  codedir <- paste0("[Insert own directory]/rhd_invest_priv/")
  datadir <- paste0("[Insert own directory]/rhd_investment_case/data/")
  outdir <- paste0("[Insert own directory]/rhd_invest_priv/data/")
}

## load functions
source(paste0(codedir,"/functions/swap_location_names.R"))

## read in AU locations (https://au.int/en/member_states/countryprofiles2)
locs <- data.table(openxlsx::read.xlsx(paste0(outdir,"/AU_member_states.xlsx")))
locs <- locs[!location_name %in% c("Sahrawi Republic")] ## we don't have burden estimates for Sahrawi Republic, so we will drop

## convert to GBD country names
locs <- swap_locnames(data=locs,out="GBD",version=2017)



## load pop projections to create weighted regions
pop <- data.table(openxlsx::read.xlsx(paste0(datadir,"/WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx"),startRow=17))
setnames(pop,c("index","variant","location_name","notes","country_code","type","parent_code","year",c(paste0(seq(from=0,to=100,by=5)))))
pop <- melt(pop,id.vars=c("index","variant","location_name","notes","country_code","type","parent_code","year"),variable.name="age_group_start",value.name="pop")
pop[,pop:=as.numeric(pop)]

popn <- data.table(openxlsx::read.xlsx(paste0(datadir,"/WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx"),startRow=17,sheet="MEDIUM VARIANT"))
setnames(popn,c("index","variant","location_name","notes","country_code","type","parent_code","year",c(paste0(seq(from=0,to=100,by=5)))))
popn <- melt(popn,id.vars=c("index","variant","location_name","notes","country_code","type","parent_code","year"),variable.name="age_group_start",value.name="pop")
popn[,pop:=as.numeric(pop)]

pop <- rbind(pop,popn)
rm(popn)
pop[,sex:="male"]


popf <- data.table(openxlsx::read.xlsx(paste0(datadir,"/WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE.xlsx"),startRow=17))
setnames(popf,c("index","variant","location_name","notes","country_code","type","parent_code","year",c(paste0(seq(from=0,to=100,by=5)))))
popf <- melt(popf,id.vars=c("index","variant","location_name","notes","country_code","type","parent_code","year"),variable.name="age_group_start",value.name="pop")
popf[,pop:=as.numeric(pop)]

popfn <- data.table(openxlsx::read.xlsx(paste0(datadir,"/WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE.xlsx"),startRow=17,sheet="MEDIUM VARIANT"))
setnames(popfn,c("index","variant","location_name","notes","country_code","type","parent_code","year",c(paste0(seq(from=0,to=100,by=5)))))
popfn <- melt(popfn,id.vars=c("index","variant","location_name","notes","country_code","type","parent_code","year"),variable.name="age_group_start",value.name="pop")
popfn[,pop:=as.numeric(pop)]

popf <- rbind(popf,popfn)
rm(popfn)
popf[,sex:="female"]

pop <- rbind(pop,popf)
rm(popf)


unique(locs$location_name[!locs$location_name %in% unique(pop$location_name)])
pop[location_name=="Cabo Verde",location_name:="Cape Verde"]
pop[grepl("Gambia",location_name),location_name:="The Gambia"]
pop[grepl("Tanzania",location_name),location_name:="Tanzania"]
pop[grepl("Ivoire",location_name),location_name:="Cote d'Ivoire"]
pop[grepl("Eswatini",location_name),location_name:="Swaziland"]
unique(locs$location_name[!locs$location_name %in% unique(pop$location_name)])

pop <- pop[!(year==2020 & variant=="Medium variant")]

pop <- dcast.data.table(pop,location_name+age_group_start+sex~year,value.var="pop")


for (i in c(2017:2099)) {
  lower <- floor(i/5)*5
  upper <- floor(i/5)*5+5
  pop[[c(as.character(i))]] <- (pop[[c(as.character(upper))]] - pop[[c(as.character(lower))]])/5*(i-lower)+pop[[c(as.character(lower))]]
}

pop <- melt(pop,id.vars=c("location_name","age_group_start","sex"),variable.name="year",value.name="pop")

## save one for weighting GNI in costing for regions
popcost <- copy(pop[year==2019])
popcost <- popcost[,list(pop=sum(pop)),by=c("location_name","year")]
popcost[,pop:=pop*1000]
popcost <- popcost[location_name %in% locs$location_name]
saveRDS(popcost,paste0(codedir,"/data/other_inputs/pop_cost_reg_weights.rds"))


## load in WPP medium variant mortality
d <- fread(paste0(datadir,"/WPP2019_Life_Table_Medium.csv"))
d <- d[Sex %in% c("Male","Female") & MidPeriod %in% c(2018,2023,2028,2033,2038,2043,2048,2053,2058,2063,2068,2073,2078,2083,2088,2093)]
d <- d[,c("Location","Sex","MidPeriod","AgeGrpStart","mx"),with=F]

## make consistent names
unique(locs$location_name[!locs$location_name %in% unique(d$Location)])
d[Location=="Cabo Verde",Location:="Cape Verde"]
d[grepl("Gambia",Location),Location:="The Gambia"]
d[grepl("Tanzania",Location),Location:="Tanzania"]
d[grepl("Ivoire",Location),Location:="Cote d'Ivoire"]
d[grepl("Eswatini",Location),Location:="Swaziland"]
unique(locs$location_name[!locs$location_name %in% unique(d$Location)])

if (length(unique(locs$location_name[!locs$location_name %in% unique(d$Location)])) > 0) stop("issue")

## subset
d <- d[Location %in% c(locs$location_name,"African Union")]
## find log mx
d[,log_mx:=log(mx)]
## take a look at the trends
gg <- ggplot(d[Location=="African Union" & Sex == "Male"],aes(x=MidPeriod,y=log_mx)) + geom_line() + facet_wrap(~AgeGrpStart,scales="free") + geom_point()
print(gg)

## interpolate between available years--linear in log space
## was initially only doing to 2030, not coded efficiently here:
d <- dcast.data.table(d,Location+Sex+AgeGrpStart~MidPeriod,value.var=c("mx","log_mx"))
d[,log_mx_2017:=log_mx_2018+(log_mx_2018-log_mx_2023)/5]
d[,log_mx_2019:=log_mx_2018-(log_mx_2018-log_mx_2023)/5]
d[,log_mx_2020:=log_mx_2018-2*((log_mx_2018-log_mx_2023)/5)]
d[,log_mx_2021:=log_mx_2018-3*((log_mx_2018-log_mx_2023)/5)]
d[,log_mx_2022:=log_mx_2018-4*((log_mx_2018-log_mx_2023)/5)]

d[,log_mx_2024:=log_mx_2023-((log_mx_2023-log_mx_2028)/5)]
d[,log_mx_2025:=log_mx_2023-2*((log_mx_2023-log_mx_2028)/5)]
d[,log_mx_2026:=log_mx_2023-3*((log_mx_2023-log_mx_2028)/5)]
d[,log_mx_2027:=log_mx_2023-4*((log_mx_2023-log_mx_2028)/5)]

d[,log_mx_2029:=log_mx_2028-((log_mx_2028-log_mx_2033)/5)]
d[,log_mx_2030:=log_mx_2028-2*((log_mx_2028-log_mx_2033)/5)]
d[,log_mx_2031:=log_mx_2028-3*((log_mx_2028-log_mx_2033)/5)]
d[,log_mx_2032:=log_mx_2028-4*((log_mx_2028-log_mx_2033)/5)]

d[,log_mx_2034:=log_mx_2033-((log_mx_2033-log_mx_2038)/5)]
d[,log_mx_2035:=log_mx_2033-2*((log_mx_2033-log_mx_2038)/5)]
d[,log_mx_2036:=log_mx_2033-3*((log_mx_2033-log_mx_2038)/5)]
d[,log_mx_2037:=log_mx_2033-4*((log_mx_2033-log_mx_2038)/5)]

d[,log_mx_2039:=log_mx_2038-((log_mx_2038-log_mx_2043)/5)]
d[,log_mx_2040:=log_mx_2038-2*((log_mx_2038-log_mx_2043)/5)]
d[,log_mx_2041:=log_mx_2038-3*((log_mx_2038-log_mx_2043)/5)]
d[,log_mx_2042:=log_mx_2038-4*((log_mx_2038-log_mx_2043)/5)]

d[,log_mx_2044:=log_mx_2043-((log_mx_2043-log_mx_2048)/5)]
d[,log_mx_2045:=log_mx_2043-2*((log_mx_2043-log_mx_2048)/5)]
d[,log_mx_2046:=log_mx_2043-3*((log_mx_2043-log_mx_2048)/5)]
d[,log_mx_2047:=log_mx_2043-4*((log_mx_2043-log_mx_2048)/5)]

d[,log_mx_2049:=log_mx_2048-((log_mx_2048-log_mx_2053)/5)]
d[,log_mx_2050:=log_mx_2048-2*((log_mx_2048-log_mx_2053)/5)]
d[,log_mx_2051:=log_mx_2048-3*((log_mx_2048-log_mx_2053)/5)]
d[,log_mx_2052:=log_mx_2048-4*((log_mx_2048-log_mx_2053)/5)]

d[,log_mx_2054:=log_mx_2053-((log_mx_2053-log_mx_2058)/5)]
d[,log_mx_2055:=log_mx_2053-2*((log_mx_2053-log_mx_2058)/5)]
d[,log_mx_2056:=log_mx_2053-3*((log_mx_2053-log_mx_2058)/5)]
d[,log_mx_2057:=log_mx_2053-4*((log_mx_2053-log_mx_2058)/5)]

d[,log_mx_2059:=log_mx_2058-((log_mx_2058-log_mx_2063)/5)]
d[,log_mx_2060:=log_mx_2058-2*((log_mx_2058-log_mx_2063)/5)]
d[,log_mx_2061:=log_mx_2058-3*((log_mx_2058-log_mx_2063)/5)]
d[,log_mx_2062:=log_mx_2058-4*((log_mx_2058-log_mx_2063)/5)]

d[,log_mx_2064:=log_mx_2063-((log_mx_2063-log_mx_2068)/5)]
d[,log_mx_2065:=log_mx_2063-2*((log_mx_2063-log_mx_2068)/5)]
d[,log_mx_2066:=log_mx_2063-3*((log_mx_2063-log_mx_2068)/5)]
d[,log_mx_2067:=log_mx_2063-4*((log_mx_2063-log_mx_2068)/5)]

d[,log_mx_2069:=log_mx_2068-((log_mx_2068-log_mx_2073)/5)]
d[,log_mx_2070:=log_mx_2068-2*((log_mx_2068-log_mx_2073)/5)]
d[,log_mx_2071:=log_mx_2068-3*((log_mx_2068-log_mx_2073)/5)]
d[,log_mx_2072:=log_mx_2068-4*((log_mx_2068-log_mx_2073)/5)]

d[,log_mx_2074:=log_mx_2073-((log_mx_2073-log_mx_2078)/5)]
d[,log_mx_2075:=log_mx_2073-2*((log_mx_2073-log_mx_2078)/5)]
d[,log_mx_2076:=log_mx_2073-3*((log_mx_2073-log_mx_2078)/5)]
d[,log_mx_2077:=log_mx_2073-4*((log_mx_2073-log_mx_2078)/5)]

d[,log_mx_2079:=log_mx_2078-((log_mx_2078-log_mx_2083)/5)]
d[,log_mx_2080:=log_mx_2078-2*((log_mx_2078-log_mx_2083)/5)]
d[,log_mx_2081:=log_mx_2078-3*((log_mx_2078-log_mx_2083)/5)]
d[,log_mx_2082:=log_mx_2078-4*((log_mx_2078-log_mx_2083)/5)]

d[,log_mx_2084:=log_mx_2083-((log_mx_2083-log_mx_2088)/5)]
d[,log_mx_2085:=log_mx_2083-2*((log_mx_2083-log_mx_2088)/5)]
d[,log_mx_2086:=log_mx_2083-3*((log_mx_2083-log_mx_2088)/5)]
d[,log_mx_2087:=log_mx_2083-4*((log_mx_2083-log_mx_2088)/5)]

d[,log_mx_2089:=log_mx_2088-((log_mx_2088-log_mx_2093)/5)]
d[,log_mx_2090:=log_mx_2088-2*((log_mx_2088-log_mx_2093)/5)]
# d[,log_mx_2086:=log_mx_2083-3*((log_mx_2083-log_mx_2088)/5)]
# d[,log_mx_2087:=log_mx_2083-4*((log_mx_2083-log_mx_2088)/5)]

## remove non-logged originals
d[,c("mx_2018","mx_2023","mx_2028","mx_2033","mx_2038","mx_2043","mx_2048","mx_2053","mx_2058","mx_2063","mx_2068","mx_2073","mx_2078","mx_2083",
     "mx_2088","mx_2093"):=NULL]

## make data long
d <- melt(d,id.vars=c("Location","Sex","AgeGrpStart"))
d[,year:=as.numeric(str_split_fixed(variable,"_",Inf)[,3])]


## re-examine trend
gg <- ggplot(d[Location=="African Union" & Sex == "Female"],aes(x=year,y=value)) + facet_wrap(~AgeGrpStart,scales="free") + geom_point()
print(gg)

## exponentiate and keep relevant years
d[,value:=exp(value)]
d <- d[year %in% c(2017:2090)]

gg <- ggplot(d[Location=="African Union" & Sex == "Male"],aes(x=year,y=value)) + facet_wrap(~AgeGrpStart,scales="free") + geom_point()
print(gg)

setnames(d,c("location_name","sex","age_group_start","variable","value","year"))
d[,sex:=tolower(sex)]

## add regions
pop[,age_group_start:=as.numeric(as.character(age_group_start))]
pop[,year:=as.numeric(as.character(year))]
add <- copy(pop[age_group_start==0])
add[,age_group_start:=1]
pop <- rbind(pop,add)

d <- merge(d,pop,by=c("location_name","sex","age_group_start","year"),all.x=T)

regs <- copy(d[location_name!="African Union"])
regs <- merge(regs,locs[,c("location_name","au_region"),with=F],by=c("location_name"),all.x=T)
if (any(is.na(regs$pop))) stop("missing")
regs <- regs[,list(value=weighted.mean(value,w=pop)),by=c("au_region","sex","age_group_start","year")]
setnames(regs,"au_region","location_name")
d[,c("variable","pop"):=NULL]
d <- rbind(d,regs,use.names=T)


## since we're using GBD mortality estimates and just trend from WPP, make reference in 2017 to use to get
## scalars by which to multiply the GBD 2017 estimates to obtain estimates for future years
ref <- copy(d[year == 2017])
setnames(ref,"value","ref")

## merge on ref and calculate ratio
d <- merge(d,ref[,c("location_name","sex","age_group_start","ref"),with=F],by=c("location_name","sex","age_group_start"),all.x=T)
d[,ratio:=value/ref]
d <- d[order(location_name,year,sex,age_group_start)]
d <- d[,c("location_name","sex","age_group_start","year","ratio"),with=F]


saveRDS(d,paste0(outdir,"/other_inputs/future_mort_scalars.RDS"))



## now for fertility
## first page is earlier years
d <- data.table(openxlsx::read.xlsx(paste0(datadir,"/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.xlsx"),startRow=17))
setnames(d,c("index","variant","location_name","notes","countrycode","type","parentcode","period","fert_15-19","fert_20-24","fert_25-29","fert_30-34","fert_35-39","fert_40-44","fert_45-49"))

## make consistent names
unique(locs$location_name[!locs$location_name %in% unique(d$location_name)])
d[location_name=="Cabo Verde",location_name:="Cape Verde"]
d[grepl("Gambia",location_name),location_name:="The Gambia"]
d[grepl("Tanzania",location_name),location_name:="Tanzania"]
d[grepl("Ivoire",location_name),location_name:="Cote d'Ivoire"]
d[grepl("Eswatini",location_name),location_name:="Swaziland"]
unique(locs$location_name[!locs$location_name %in% unique(d$location_name)])

if (length(unique(locs$location_name[!locs$location_name %in% unique(d$location_name)])) > 0) stop("issue")

d <- d[location_name %in% c(locs$location_name,"Africa")] ## "African Union" not in this WPP dataset, using africa for ratios for the aggregate
d[,period_start:=as.numeric(str_split_fixed(period,"-",Inf)[,1])]
d[,year:=period_start+3]
d <- d[year %in% c(2018)]

## second page is projections
d2 <- data.table(openxlsx::read.xlsx(paste0(datadir,"/WPP2019_FERT_F07_AGE_SPECIFIC_FERTILITY.xlsx"),startRow=17,sheet=2))
setnames(d2,c("index","variant","location_name","notes","countrycode","type","parentcode","period","fert_15-19","fert_20-24","fert_25-29","fert_30-34","fert_35-39","fert_40-44","fert_45-49"))

## make consistent names
unique(locs$location_name[!locs$location_name %in% unique(d2$location_name)])
d2[location_name=="Cabo Verde",location_name:="Cape Verde"]
d2[grepl("Gambia",location_name),location_name:="The Gambia"]
d2[grepl("Tanzania",location_name),location_name:="Tanzania"]
d2[grepl("Ivoire",location_name),location_name:="Cote d'Ivoire"]
d2[grepl("Eswatini",location_name),location_name:="Swaziland"]
unique(locs$location_name[!locs$location_name %in% unique(d2$location_name)])

if (length(unique(locs$location_name[!locs$location_name %in% unique(d2$location_name)])) > 0) stop("issue")

d2 <- d2[location_name %in% c(locs$location_name,"Africa")]
d2[,period_start:=as.numeric(str_split_fixed(period,"-",Inf)[,1])]
d2[,year:=period_start+3]
d2 <- d2[year %in% c(2023,2028,2033,2038,2043,2048,2053,2058,2063,2068,2073,2078,2083,2088,2093)]

d <- rbind(d,d2)

d <- d[,c("location_name","year",names(d)[grepl("fert",names(d))]),with=F]
d <- melt(d,id.vars=c("location_name","year"))
d[,age_start:=as.numeric(substr(str_split_fixed(variable,"_",Inf)[,2],1,2))]
d[,value:=as.numeric(value)]
d[,log_fert:=log(value)]
d <- dcast.data.table(d,location_name+age_start~year,value.var=c("value","log_fert"))


## interpolate between available years--linear in log space
## was initially only doing to 2030, not coded efficiently here:
d[,log_fert_2017:=log_fert_2018+(log_fert_2018-log_fert_2023)/5]

d[,log_fert_2019:=log_fert_2018-(log_fert_2018-log_fert_2023)/5]
d[,log_fert_2020:=log_fert_2018-2*((log_fert_2018-log_fert_2023)/5)]
d[,log_fert_2021:=log_fert_2018-3*((log_fert_2018-log_fert_2023)/5)]
d[,log_fert_2022:=log_fert_2018-4*((log_fert_2018-log_fert_2023)/5)]

d[,log_fert_2024:=log_fert_2023-((log_fert_2023-log_fert_2028)/5)]
d[,log_fert_2025:=log_fert_2023-2*((log_fert_2023-log_fert_2028)/5)]
d[,log_fert_2026:=log_fert_2023-3*((log_fert_2023-log_fert_2028)/5)]
d[,log_fert_2027:=log_fert_2023-4*((log_fert_2023-log_fert_2028)/5)]

d[,log_fert_2029:=log_fert_2028-((log_fert_2028-log_fert_2033)/5)]
d[,log_fert_2030:=log_fert_2028-2*((log_fert_2028-log_fert_2033)/5)]
d[,log_fert_2031:=log_fert_2028-3*((log_fert_2028-log_fert_2033)/5)]
d[,log_fert_2032:=log_fert_2028-4*((log_fert_2028-log_fert_2033)/5)]

d[,log_fert_2034:=log_fert_2033-((log_fert_2033-log_fert_2038)/5)]
d[,log_fert_2035:=log_fert_2033-2*((log_fert_2033-log_fert_2038)/5)]
d[,log_fert_2036:=log_fert_2033-3*((log_fert_2033-log_fert_2038)/5)]
d[,log_fert_2037:=log_fert_2033-4*((log_fert_2033-log_fert_2038)/5)]

d[,log_fert_2039:=log_fert_2038-((log_fert_2038-log_fert_2043)/5)]
d[,log_fert_2040:=log_fert_2038-2*((log_fert_2038-log_fert_2043)/5)]
d[,log_fert_2041:=log_fert_2038-3*((log_fert_2038-log_fert_2043)/5)]
d[,log_fert_2042:=log_fert_2038-4*((log_fert_2038-log_fert_2043)/5)]

d[,log_fert_2044:=log_fert_2043-((log_fert_2043-log_fert_2048)/5)]
d[,log_fert_2045:=log_fert_2043-2*((log_fert_2043-log_fert_2048)/5)]
d[,log_fert_2046:=log_fert_2043-3*((log_fert_2043-log_fert_2048)/5)]
d[,log_fert_2047:=log_fert_2043-4*((log_fert_2043-log_fert_2048)/5)]

d[,log_fert_2049:=log_fert_2048-((log_fert_2048-log_fert_2053)/5)]
d[,log_fert_2050:=log_fert_2048-2*((log_fert_2048-log_fert_2053)/5)]
d[,log_fert_2051:=log_fert_2048-3*((log_fert_2048-log_fert_2053)/5)]
d[,log_fert_2052:=log_fert_2048-4*((log_fert_2048-log_fert_2053)/5)]

d[,log_fert_2054:=log_fert_2053-((log_fert_2053-log_fert_2058)/5)]
d[,log_fert_2055:=log_fert_2053-2*((log_fert_2053-log_fert_2058)/5)]
d[,log_fert_2056:=log_fert_2053-3*((log_fert_2053-log_fert_2058)/5)]
d[,log_fert_2057:=log_fert_2053-4*((log_fert_2053-log_fert_2058)/5)]

d[,log_fert_2059:=log_fert_2058-((log_fert_2058-log_fert_2063)/5)]
d[,log_fert_2060:=log_fert_2058-2*((log_fert_2058-log_fert_2063)/5)]
d[,log_fert_2061:=log_fert_2058-3*((log_fert_2058-log_fert_2063)/5)]
d[,log_fert_2062:=log_fert_2058-4*((log_fert_2058-log_fert_2063)/5)]

d[,log_fert_2064:=log_fert_2063-((log_fert_2063-log_fert_2068)/5)]
d[,log_fert_2065:=log_fert_2063-2*((log_fert_2063-log_fert_2068)/5)]
d[,log_fert_2066:=log_fert_2063-3*((log_fert_2063-log_fert_2068)/5)]
d[,log_fert_2067:=log_fert_2063-4*((log_fert_2063-log_fert_2068)/5)]

d[,log_fert_2069:=log_fert_2068-((log_fert_2068-log_fert_2073)/5)]
d[,log_fert_2070:=log_fert_2068-2*((log_fert_2068-log_fert_2073)/5)]
d[,log_fert_2071:=log_fert_2068-3*((log_fert_2068-log_fert_2073)/5)]
d[,log_fert_2072:=log_fert_2068-4*((log_fert_2068-log_fert_2073)/5)]

d[,log_fert_2074:=log_fert_2073-((log_fert_2073-log_fert_2078)/5)]
d[,log_fert_2075:=log_fert_2073-2*((log_fert_2073-log_fert_2078)/5)]
d[,log_fert_2076:=log_fert_2073-3*((log_fert_2073-log_fert_2078)/5)]
d[,log_fert_2077:=log_fert_2073-4*((log_fert_2073-log_fert_2078)/5)]

d[,log_fert_2079:=log_fert_2078-((log_fert_2078-log_fert_2083)/5)]
d[,log_fert_2080:=log_fert_2078-2*((log_fert_2078-log_fert_2083)/5)]
d[,log_fert_2081:=log_fert_2078-3*((log_fert_2078-log_fert_2083)/5)]
d[,log_fert_2082:=log_fert_2078-4*((log_fert_2078-log_fert_2083)/5)]

d[,log_fert_2084:=log_fert_2083-((log_fert_2083-log_fert_2088)/5)]
d[,log_fert_2085:=log_fert_2083-2*((log_fert_2083-log_fert_2088)/5)]
d[,log_fert_2086:=log_fert_2083-3*((log_fert_2083-log_fert_2088)/5)]
d[,log_fert_2087:=log_fert_2083-4*((log_fert_2083-log_fert_2088)/5)]

d[,log_fert_2089:=log_fert_2088-((log_fert_2088-log_fert_2093)/5)]
d[,log_fert_2090:=log_fert_2088-2*((log_fert_2088-log_fert_2093)/5)]
# d[,log_fert_2086:=log_fert_2083-3*((log_fert_2083-log_fert_2088)/5)]
# d[,log_fert_2087:=log_fert_2083-4*((log_fert_2083-log_fert_2088)/5)]


d[,c("value_2018","value_2023","value_2028","value_2033","value_2038","value_2043","value_2048","value_2053","value_2058","value_2063","value_2068","value_2073","value_2078","value_2083",
     "value_2088","value_2093"):=NULL]

d <- melt(d,id.vars=c("location_name","age_start"))
d[,year:=as.numeric(str_split_fixed(variable,"_",Inf)[,3])]

gg <- ggplot(d[location_name=="Africa"],aes(x=year,y=value)) + facet_wrap(~age_start,scales="free") + geom_point()
print(gg)

d[,value:=exp(value)]

gg <- ggplot(d[location_name=="Africa"],aes(x=year,y=value)) + facet_wrap(~age_start,scales="free") + geom_point()
print(gg)


setnames(pop,c("age_group_start"),c("age_start"))

d <- merge(d,pop[sex=="female"],by=c("location_name","age_start","year"),all.x=T)
if (any(is.na(d$pop))) stop("missing pop")

regs <- copy(d[location_name!="Africa"])
regs <- merge(regs,locs[,c("location_name","au_region"),with=F],by=c("location_name"),all.x=T)
if (any(is.na(regs$pop))) stop("missing")
regs <- regs[,list(value=weighted.mean(value,w=pop)),by=c("au_region","age_start","year")]
setnames(regs,"au_region","location_name")
d[,c("variable","pop","sex"):=NULL]
d <- rbind(d,regs,use.names=T)



ref <- copy(d[year == 2017])
setnames(ref,"value","ref")


d <- merge(d,ref[,c("location_name","age_start","ref"),with=F],by=c("location_name","age_start"),all.x=T)
d[,rat:=value/ref]
d <- d[order(location_name,year,age_start)]
d <- d[,c("location_name","year","age_start","rat"),with=F]

## replicate for 10-14 and 50-54 (GBD has wider fertility age ranges)
add <- copy(d[age_start==15])
add[,age_start:=10]
d <- rbind(d,add)

add <- copy(d[age_start==45])
add[,age_start:=50]
d <- rbind(d,add)

d[location_name=="Africa",location_name:="African Union"]
d <- d[year %in% c(2017:2090)]

saveRDS(d,paste0(outdir,"/other_inputs/future_fert_scalars.RDS"))

  