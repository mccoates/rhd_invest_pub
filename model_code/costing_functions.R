## Matthew Coates
## Function to convert from costs in different countries to AU average or to country-specific costs


convert_costs <- function(dat=NA,locs=NA) {

  source(paste0(codedir,"/functions/map_ids_names.R"))  
  source(paste0(codedir,"/functions/swap_location_names.R"))
  
  ## read in AU locations (https://au.int/en/member_states/countryprofiles2)
  au_locs <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/AU_member_states.xlsx")))
  au_locs <- au_locs[!location_name %in% c("Sahrawi Republic")] ## we don't have burden estimates for Sahrawi Republic, so we will drop
  ## convert to GBD country names
  locregs <- swap_locnames(data=au_locs,out="GBD",version=2017)
  au_locs <- locregs$location_name
  

  ## load inflation data
  weo <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/other_inputs/WEO_Data.xlsx")))
  weo <- weo[Subject.Descriptor == "Inflation, average consumer prices",c("Country","ISO",as.character(2000:2020)),with=F]
  weo <- melt(weo,id.vars=c("Country","ISO"))
  weo[,value:=as.numeric(value)]
  setnames(weo,c("Country","ISO","year","inflation"))
  
  ## load exchange rate data
  ex <- data.table(openxlsx::read.xlsx(paste0(codedir,"/data/other_inputs/API_PA.NUS.FCRF_DS2_en_excel_v2_1218111.xlsx"),startRow=3))
  ex <- ex[,c("Country.Name","Country.Code",as.character(1960:2019)),with=F]
  ex <- melt(ex,id.vars=c("Country.Name","Country.Code")) 
  setnames(ex,c("Country","ISO","year","exchange_rate"))
  
  ## load PPP
  ppp <- fread(paste0(codedir,"/data/other_inputs/API_PA.NUS.PPP_DS2_en_csv_v2_1222173.csv"),header=T)
  ppp <- ppp[,c("Country Name","Country Code",as.character(1960:2019)),with=F]
  ppp <- melt(ppp,id.vars=c("Country Name","Country Code")) 
  setnames(ppp,c("Country","ISO","year","ppp"))
  
  inp <- merge(copy(weo),copy(ex[,c("ISO","year","exchange_rate"),with=F]),by=c("ISO","year"),all=T)
  inp <- merge(inp,copy(ppp[,c("ISO","year","ppp"),with=F]),by=c("ISO","year"),all=T)
  inp[,pi:=ppp/exchange_rate]
  inp[Country=="Cabo Verde",Country:="Cape Verde"]
  inp[Country=="Eswatini",Country:="Swaziland"]
  inp[grepl("Ivoire",Country),Country:="Cote d'Ivoire"]
  inp[Country=="Republic of Congo",Country:="Congo"]
  inp[grepl("ncipe",Country) & grepl("o Tom",Country),Country:="Sao Tome and Principe"]
  inp[,year:=as.numeric(as.character(year))]
  
  ## Rwandan exchange rate missing, but we need it for some years we have costs from
  inp[Country=="Rwanda" & year == 2019,exchange_rate:=911.5004] ## https://www.exchangerates.org.uk/USD-RWF-spot-exchange-rates-history-2019.html
  inp[Country=="Rwanda" & year == 2014,exchange_rate:=684.8188] ## https://www.exchangerates.org.uk/USD-RWF-spot-exchange-rates-history-2019.html


  au_locs[!au_locs %in% unique(inp$Country)]
  
  au_pi <- copy(inp[Country %in% c(au_locs)])
  au_pi[year==2018]
  

  gbd <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_inc_prev_death_inputs.RDS"))
  gbd <- map_to_names(gbd,codedir=codedir,keep_ids=T,gbd=2017)
  gbd <- gbd[measure_name=="Prevalence" & cause_name=="Rheumatic heart disease"]
  gbd <- gbd[age_group_id %in% c(2:20,30:32,235) & location_name %in% au_locs]
  
  pop <- readRDS(paste0(codedir,"/data/gbd_inputs/gbd_pop_aggage.RDS"))
  pop <- pop[location_name %in% c(au_locs,"African Union")]
  
  gbd <- merge(gbd,pop[,c("location_name","sex","age_group_id","year","pop"),with=F],by=c("location_name","sex","age_group_id","year"),all.x=T)
  gbd[is.na(pop)]
  gbd[,Number:=val*pop]
  gbd <- gbd[,list(Number=sum(Number)),by=c("location_name","year")]
  setnames(gbd,"location_name","Country")
  au_pi <- merge(au_pi,gbd,by=c("Country","year"),all.x=T)
  au_ave_pi <- copy(au_pi[,list(au_avg=weighted.mean(pi,w=Number,na.rm=T)),by=c("year")])
  ## this not actually used in current version

  ## get names to reorder dataset after
  names_dat <- names(dat)
    
  dat[,adj:=as.numeric(NA)]
  ## for those marked as "average" and in US currency
  if (nrow(dat[country_costed=="Average"]) > 0) {
    for (i in c(1:nrow(dat[country_costed=="Average"]))) {
      if (dat[country_costed=="Average"][i]$currency_year < 2019) dat[country_costed=="Average"][i]$adj <- prod(1+inp[Country=="United States" & year %in% c((dat[country_costed=="Average"][i]$currency_year+1):2019)]$inflation/100)
      if (dat[country_costed=="Average"][i]$currency_year > 2019) stop("currency given after 2019, invalid for costing strategy")
    }
  }
  
  ## for those marked as "tradeable," use US inflation rate 
  ## no problem here
  if (nrow(dat[country_costed=="Tradeable"]) > 0) {
    for (i in c(1:nrow(dat[country_costed=="Tradeable"]))) {
      if (dat[country_costed=="Tradeable"][i]$currency_year < 2019) dat[country_costed=="Tradeable"][i]$adj <- prod(1+inp[Country=="United States" & year %in% c((dat[country_costed=="Tradeable"][i]$currency_year+1):2019)]$inflation/100)
      if (dat[country_costed=="Tradeable"][i]$currency_year > 2019) stop("currency given after 2019, invalid for costing strategy")
    }
  }
    
  dat[country_costed %in% c("Average","Tradeable") & currency_year == 2019,adj:=1]
  ## now, inflate these two and change to 2019 USD
  if (any(is.na(dat[country_costed %in% c("Average","Tradeable")]$adj))) stop("missing adjustment")
  dat[country_costed %in% c("Average","Tradeable"),mean:=mean*adj]
  dat[country_costed %in% c("Average","Tradeable"),lower:=lower*adj]
  dat[country_costed %in% c("Average","Tradeable"),upper:=upper*adj]
  dat[country_costed %in% c("Average","Tradeable"),currency_year:=2019]
  
  ## for things costed in a particular country 
  ## convert to country currency in given year using exchange rate
  ## then use country inflation, then convert back using 2019 exchange rate
  for (spec_loc in c("Rwanda","South Africa","Burkina Faso","Cameroon")) {
    if (nrow(dat[country_costed==spec_loc]) > 0) {
      for (i in c(1:nrow(dat[country_costed==spec_loc]))) {
        if (dat[country_costed==spec_loc][i]$currency_year < 2019) {
          ## convert to RWA francs
          cyr <- dat[country_costed==spec_loc][i]$currency_year
    
          dat[country_costed==spec_loc][i]$mean <- dat[country_costed==spec_loc][i]$mean*inp[Country==spec_loc & year == cyr]$exchange_rate
          dat[country_costed==spec_loc][i]$lower <- dat[country_costed==spec_loc][i]$lower*inp[Country==spec_loc & year == cyr]$exchange_rate
          dat[country_costed==spec_loc][i]$upper <- dat[country_costed==spec_loc][i]$upper*inp[Country==spec_loc & year == cyr]$exchange_rate
          
          ## now inflate each
          dat[country_costed==spec_loc][i]$adj <- prod(1+inp[Country==spec_loc & year %in% c((dat[country_costed==spec_loc][i]$currency_year+1):2019)]$inflation/100)
          dat[country_costed==spec_loc][i]$mean <- dat[country_costed==spec_loc][i]$mean*dat[country_costed==spec_loc][i]$adj
          dat[country_costed==spec_loc][i]$lower <- dat[country_costed==spec_loc][i]$lower*dat[country_costed==spec_loc][i]$adj
          dat[country_costed==spec_loc][i]$upper <- dat[country_costed==spec_loc][i]$upper*dat[country_costed==spec_loc][i]$adj
          
          ## now convert back to 2019 USD
          dat[country_costed==spec_loc][i]$mean <- dat[country_costed==spec_loc][i]$mean/inp[Country==spec_loc & year == 2019]$exchange_rate
          dat[country_costed==spec_loc][i]$lower <- dat[country_costed==spec_loc][i]$lower/inp[Country==spec_loc & year == 2019]$exchange_rate
          dat[country_costed==spec_loc][i]$upper <- dat[country_costed==spec_loc][i]$upper/inp[Country==spec_loc & year == 2019]$exchange_rate
          
          ## now label as 2019 currency
          dat[country_costed==spec_loc][i]$currency_year <- 2019
          
        }
          
        if (dat[country_costed==spec_loc][i]$currency_year > 2019) stop("currency given after 2019, invalid for costing strategy")
      }
    }
  }
  dat[currency_year==2019 & is.na(adj),adj:=1]
  
  if (nrow(dat[!country_costed %in% c("Tradeable","Average","Rwanda","South Africa","Burkina Faso","Cameroon")]) > 0) stop("unrecognized country costed")
  
  
  ## part here to adjust from whatever country initial cost was calculated in to "African" average
  ## or to any other particular country, depending on whether running for AU or for specific countries
  gni <- fread(paste0(codedir,"/data/other_inputs/API_NY.GNP.PCAP.CD_DS2_en_csv_v2_1218027.csv"),header=T) 
  gni[,V65:=NULL]
  gni <- melt(gni,id.vars=c("Country Name","Country Code","Indicator Name","Indicator Code"),variable.name="year",value.name="gni")
  gni[,year:=as.numeric(as.character(year))]
  gni[grepl("Verde",`Country Name`),`Country Name`:="Cape Verde"]
  #d[grepl("Swazi",`Country Name`),`Country Name`:="Eswatini"]
  gni[grepl("Gambia",`Country Name`),`Country Name`:="The Gambia"]
  gni[grepl("Ivoire",`Country Name`),`Country Name`:="Cote d'Ivoire"]
  gni[grepl("Tanzania",`Country Name`),`Country Name`:="Tanzania"]
  gni[`Country Name` == "Congo, Dem. Rep.",`Country Name`:="Democratic Republic of the Congo"]
  gni[`Country Name` == "Congo, Rep.",`Country Name`:="Congo"]
  gni[`Country Name` == "Eswatini",`Country Name`:="Swaziland"]
  gni[`Country Name` == "Egypt, Arab Rep.",`Country Name`:="Egypt"]
  
  au_locs[!au_locs %in% gni$`Country Name`]
  
  ## make AU GNI, working round missingness
  augni <- copy(gni[`Country Name` %in% au_locs])
  ## updated data has 2019
  augni <- augni[year >=2000]
  ## merge on pops
  totpop <- copy(pop)
  totpop <- totpop[,list(pop=sum(pop)),by=c("year","location_name")]
  setnames(totpop,c("year","Country Name","pop"))
  add <- copy(totpop[year==2017])
  add[,year:=2018]
  add2 <- copy(add)
  add2[,year:=2019]
  totpop <- rbind(totpop,add,add2)
  add <- add2 <- NULL
  augni <- merge(augni,totpop,by=c("Country Name","year"),all.x=T)
  if (nrow(augni[is.na(pop)]) > 0) stop("missing pops")
  augni <- augni[,list(gni=weighted.mean(gni,w=pop,na.rm=T)),by=c("year")]
  augni[,`Country Name`:="AU"]
  gni <- rbind(gni,augni,fill=T)
  cgni <- copy(gni)
  
  gni <- gni[year >= 2000 & `Country Name` %in% c("Rwanda","Cameroon","Burkina Faso","South Africa","Sub-Saharan Africa","AU")]
  setnames(gni,c("country_costed","iso3","indicator","indicator_code","currency_year","gni"))
  add <- copy(gni[country_costed=="AU"])
  setnames(add,"gni","au_gni")
  gni <- merge(gni,add[,c("currency_year","au_gni"),with=F],by=c("currency_year"))
  gni[,rat_au:=au_gni/gni]
  gni[currency_year==2019]
  
  ## use GNI to adjust non-tradeable and "non-average" costs to AU level
  ## Assuming surgery costs aren't adjusted--some proportion will be done locally, some will be done in countries like South Africa
  ## this is an area of uncertainty
  dat[country_costed=="Average",country_costed:="AU"]
  dat <- merge(dat,gni[,c("country_costed","currency_year","rat_au"),with=F],by=c("country_costed","currency_year"),all.x=T)
  dat[country_costed=="South Africa" & grepl("surgery",item),rat_au:=1]
  if (nrow(dat[country_costed!="Tradeable" & is.na(rat_au)]) > 0) stop("missing conversion factor for non-tradeable cost")
  dat[country_costed!="Tradeable",mean:=mean*rat_au]
  dat[country_costed!="Tradeable",lower:=lower*rat_au]
  dat[country_costed!="Tradeable",upper:=upper*rat_au]
  dat[,rat_au:=NULL]
  dat[,adj:=NULL]
  dat[,location_name:="African Union"]
  
  ## now, these costs are all for the AU
  ## convert to all AU countries if code is being run by country
  if (locs != "African Union" & length(locs) > 5) {
    cgni <- cgni[year==2019 & `Country Name` %in% c(au_locs,"AU")]
    ## only missing countries Somalia, South Sudan, Eritrea
    cgni[,rat:=gni/cgni[`Country Name`=="AU"]$gni]
    ## adjust all non-tradeable costs which are now for "AU" to country-specific using ratio
    # cgni <- cgni[`Country Name`!="AU"]
    cgni[`Country Name`=="South Sudan",rat:=.5] ## 1090 in 2015 (AU 2070)
    cgni[`Country Name`=="Somalia",rat:=.2] ## 130 in 1990 (AU < 700)
    cgni[`Country Name`=="Eritrea",rat:=.3] ## 600 in 2011 (AU 1834)
    setnames(cgni,"Country Name","location_name")
    
    dat <- rbindlist(lapply(unique(cgni$location_name),FUN=function(x){
      out <- copy(dat)
      out[,location_name:=x]
      return(out)
    }))
    dat <- merge(dat,cgni[,c("location_name","rat"),with=F],by=c("location_name"),all.x=T)
    dat[country_costed!="Tradeable",mean:=mean*rat]
    dat[country_costed!="Tradeable",lower:=lower*rat]
    dat[country_costed!="Tradeable",upper:=upper*rat]
    dat[,rat:=NULL]
    dat[location_name=="AU",location_name:="African Union"]
    
    ## if region
  } else if (locs %in% c("South Africa","Democratic Republic of the Congo")) {
    
    cgni <- cgni[year==2019 & `Country Name` %in% c(au_locs,"AU")]
    ## make regional GNIs
    setnames(cgni,"Country Name","location_name")
    cgni[,year:=as.numeric(as.character(year))]
    
    ## only missing countries Somalia, South Sudan, Eritrea
    cgni[,rat:=gni/cgni[location_name=="AU"]$gni]
    ## adjust all non-tradeable costs which are now for "AU" to country-specific using ratio
    # cgni <- cgni[`Country Name`!="AU"]
    cgni[location_name=="South Sudan",rat:=.5] ## 1090 in 2015 (AU 2070)
    cgni[location_name=="Somalia",rat:=.2] ## 130 in 1990 (AU < 700)
    cgni[location_name=="Eritrea",rat:=.3] ## 600 in 2011 (AU 1834)
    cgni[is.na(gni),gni:=cgni[location_name=="AU"]$gni*rat]
    cgni[location_name=="AU",location_name:="African Union"]
    cgni[,rat:=gni/cgni[location_name=="African Union"]$gni]
    cgni <- cgni[location_name %in% c(locs,"African Union")]
    
    dat <- rbindlist(lapply(unique(cgni$location_name),FUN=function(x){
      out <- copy(dat)
      out[,location_name:=x]
      return(out)
    }))
    dat <- merge(dat,cgni[,c("location_name","rat"),with=F],by=c("location_name"),all.x=T)
    dat[country_costed!="Tradeable",mean:=mean*rat]
    dat[country_costed!="Tradeable",lower:=lower*rat]
    dat[country_costed!="Tradeable",upper:=upper*rat]
    dat[,rat:=NULL]
  } else if (locs != "African Union") {
    
    cgni <- cgni[year==2019 & `Country Name` %in% c(au_locs,"AU")]
    ## make regional GNIs
    setnames(cgni,"Country Name","location_name")
    cgni <- merge(cgni,locregs[,c("location_name","au_region"),with=F],by=c("location_name"),all.x=T)
    cgni[location_name=="AU",au_region:="African Union"]
    cgni[,year:=as.numeric(as.character(year))]
    
    ## need 2019 pops to pop weight
    pops <- readRDS(paste0(codedir,"/data/other_inputs/pop_cost_reg_weights.rds"))
    pops[,year:=as.numeric(as.character(year))]
    cgni <- merge(cgni,pops,by=c("location_name","year"),all.x=T)
    
    ## only missing countries Somalia, South Sudan, Eritrea
    cgni[,rat:=gni/cgni[location_name=="AU"]$gni]
    ## adjust all non-tradeable costs which are now for "AU" to country-specific using ratio
    # cgni <- cgni[`Country Name`!="AU"]
    cgni[location_name=="South Sudan",rat:=.5] ## 1090 in 2015 (AU 2070)
    cgni[location_name=="Somalia",rat:=.2] ## 130 in 1990 (AU < 700)
    cgni[location_name=="Eritrea",rat:=.3] ## 600 in 2011 (AU 1834)
    cgni[is.na(gni),gni:=cgni[location_name=="AU"]$gni*rat]
    cgni[location_name=="AU",pop:=1]
    cgni <- cgni[,list(gni=weighted.mean(gni,w=pop)),by=c("au_region")]
    setnames(cgni,"au_region","location_name")
    cgni[,rat:=gni/cgni[location_name=="African Union"]$gni]
    
    
    dat <- rbindlist(lapply(unique(cgni$location_name),FUN=function(x){
      out <- copy(dat)
      out[,location_name:=x]
      return(out)
    }))
    dat <- merge(dat,cgni[,c("location_name","rat"),with=F],by=c("location_name"),all.x=T)
    dat[country_costed!="Tradeable",mean:=mean*rat]
    dat[country_costed!="Tradeable",lower:=lower*rat]
    dat[country_costed!="Tradeable",upper:=upper*rat]
    dat[,rat:=NULL]
    
  }
  dat[,adj:=NULL]
  
  return(dat)
  
  
}





