## Matthew Coates
## Function to go between GBD IDs and names for saving file space

library(data.table)


map_to_ids <- function(d,codedir,delete_id=F,gbd=NA) {
  
  if (gbd != 2017) stop("Only version available in this version of the funciton is GBD 2017")
  
  ## if IDs already exist and we want to create new ones from the names, option to delete
  if (delete_id==T) {
    d[,age_group_id:=NULL]
    d[,location_id:=NULL]
    d[,sex_id:=NULL]
    d[,measure_id:=NULL]
    d[,metric_id:=NULL]
  }
  
  ## get location of idmaps
  idmap_loc <- paste0(codedir,"/data/id_maps/")
  
  ## AGE
  ages <- fread(paste0(idmap_loc,"age_group_map.csv"))
  ages[,age:=gsub(" ","",age_group_name)]
  if (is.null(d$age_group_id)) {
    if (!is.null(d$age_group_name)) {
      d <- merge(d,ages[,c("age_group_id","age_group_name"),with=F],by="age_group_name",all.x=T)
      if (any(is.na(d$age_group_id))) stop("created missing age_group_ids")
    }
    if (!is.null(d$age)) {
      d <- merge(d,ages[,c("age_group_id","age"),with=F],by="age",all.x=T)
      if (any(is.na(d$age_group_id))) stop("created missing age_group_ids")
    }
    if (is.null(d$age_group_id)) print("Age group id not created")
    d[,age_group_name:=NULL]
    d[,age:=NULL]
    if (is.null(d$age_group_name) & is.null(d$age_group_id) & is.null(d$age)) print("No Age Variable Present")
  }
  
  ## SEX
  sexes <- fread(paste0(idmap_loc,"sex_map.csv"))
  if (is.null(d$sex_id)) {
    if (!is.null(d$sex)) {
      d <- merge(d,sexes[,c("sex_id","sex"),with=F],by="sex",all.x=T)
      if (any(is.na(d$sex_id))) stop("created missing sex_ids")
    }
    if (is.null(d$sex_id)) print("Sex id not created")
    d[,sex:=NULL]
    if (is.null(d$sex) & is.null(d$sex_id)) print("No Sex Variable Present")
  }
  
  ## CAUSE
  causes <- fread(paste0(idmap_loc,"cause_map.csv"))
  if (is.null(d$cause_id)) {
    if (!is.null(d$cause_name)) {
      d <- merge(d,causes[,c("cause_id","cause_name"),with=F],by="cause_name",all.x=T)
      if (any(is.na(d$cause_id))) stop("created missing cause_ids")
    }
    if (is.null(d$cause_id)) print("Cause id not created")
    d[,cause_name:=NULL]
    if (is.null(d$cause_name) & is.null(d$cause_id)) print("No Cause Variable Present")
  }
  
  ## LOCATION
  locations <- fread(paste0(idmap_loc,"location_map.csv"))
  if (is.null(d$location_id)) {
    if (!is.null(d$location_name)) {
      d <- merge(d,locations[,c("location_id","location_name"),with=F],by="location_name",all.x=T)
      if (any(is.na(d$location_id))) stop("created missing location_ids")
    }
    if (is.null(d$location_id)) print("Location id not created")
    d[,location_name:=NULL]
    if (is.null(d$location_name) & is.null(d$location_id)) print("No Location Variable Present")
  }
  
  ## MEASURE
  measures <- fread(paste0(idmap_loc,"measure_map.csv"))
  if (is.null(d$measure_id)) {
    if (!is.null(d$measure_name)) {
      d <- merge(d,measures[,c("measure_id","measure_name"),with=F],by="measure_name",all.x=T)
      if (any(is.na(d$measure_id))) stop("created missing measure_ids")
    }
    if (is.null(d$measure_id)) print("measure id not created")
    d[,measure_name:=NULL]
    if (is.null(d$measure_name) & is.null(d$measure_id)) print("No measure Variable Present")
  }
  
  ## METRIC
  metrics <- fread(paste0(idmap_loc,"metric_map.csv"))
  if (is.null(d$metric_id)) {
    if (!is.null(d$metric_name)) {
      d <- merge(d,metrics[,c("metric_id","metric_name"),with=F],by="metric_name",all.x=T)
      if (any(is.na(d$metric_id))) stop("created missing metric_ids")
    }
    if (is.null(d$metric_id)) print("metric id not created")
    d[,metric_name:=NULL]
    if (is.null(d$metric_name) & is.null(d$metric_id)) print("No metric Variable Present")
  }
  
  
  ## REI
  reis <- data.table(read.csv(paste0(idmap_loc,"risk_hierarchy.csv")))
  if (is.null(d$rei_id)) {
    if (!is.null(d$rei_name)) {
      d <- merge(d,reis[,c("rei_id","rei_name"),with=F],by="rei_name",all.x=T)
      if (any(is.na(d$rei_id))) stop("created missing rei_ids")
    }
    if (is.null(d$rei_id)) print("rei id not created")
    d[,rei_name:=NULL]
    if (is.null(d$rei_name) & is.null(d$rei_id)) print("No rei Variable Present")
  }
  
  ## interesection
  if (!is.null(d$intersection) | !is.null(d$intersection_id)) {
    int <- data.table(read.csv(paste0(idmap_loc,"intersection_map.csv")))
    if (is.null(d$intersection_id)) {
      if (!is.null(d$intersection)) {
        d <- merge(d,int[,c("intersection","intersection_id"),with=F],by="intersection",all.x=T)
        if (any(is.na(d$intersection_id))) stop("created missing intersection_ids")
      }
      if (is.null(d$intersection_id)) print("interesction id not created")
      d[,intersection:=NULL]
      if (is.null(d$intersection) & is.null(d$intersection_id)) print("No intersection Variable Present")
    }
  }
  
  return(d)
  
}


map_to_names <- function(d,codedir,keep_ids=F,gbd=NA) {
  
  if (gbd != 2017) stop("Only version available in this version of the funciton is GBD 2017")
  
  ## get location of idmaps
  idmap_loc <- paste0(codedir,"/data/id_maps/")
  
  ## AGE
  ages <- fread(paste0(idmap_loc,"age_group_map.csv"))
  ages[,age:=gsub(" ","",age_group_name)]
  if (is.null(d$age_group_name)) {
    if (!is.null(d$age_group_id)) {
      d <- merge(d,ages[,c("age_group_id","age_group_name","age"),with=F],by="age_group_id",all.x=T)
      if (any(is.na(d$age_group_id))) stop("created missing age_group_ids")
    }
    if (is.null(d$age_group_id)) print("Age group id not created")
    if (!keep_ids) d[,age_group_id:=NULL]
    if (is.null(d$age_group_name) & is.null(d$age_group_id) & is.null(d$age)) print("No Age Variable Present")
  }
  
  ## SEX
  sexes <- fread(paste0(idmap_loc,"sex_map.csv"))
  if (is.null(d[["sex"]])) {
    if (!is.null(d$sex_id)) {
      d <- merge(d,sexes[,c("sex_id","sex"),with=F],by="sex_id",all.x=T)
      if (any(is.na(d$sex))) stop("created missing sex_ids")
    }
    if (is.null(d$sex)) print("Sex not created")
    if (!keep_ids) d[,sex_id:=NULL]
    if (is.null(d$sex) & is.null(d$sex_id)) print("No Sex Variable Present")
  }
  
  ## CAUSE
  causes <- fread(paste0(idmap_loc,"cause_map.csv"))
  if (is.null(d$cause_name)) {
    if (!is.null(d$cause_id)) {
      d <- merge(d,causes[,c("cause_id","cause_name"),with=F],by="cause_id",all.x=T)
      if (any(is.na(d$cause_name))) stop("created missing cause_names")
    }
    if (is.null(d$cause_name)) print("Cause name not created")
    if (!keep_ids) d[,cause_id:=NULL]
    if (is.null(d$cause_name) & is.null(d$cause_id)) print("No Cause Variable Present")
  }
  
  ## LOCATION
  locations <- fread(paste0(idmap_loc,"location_map.csv"))
  loclevels <- fread(paste0(idmap_loc,"loc_levels.csv"))
  setnames(loclevels,"level","loc_level")
  if (is.null(d$location_name)) {
    if (!is.null(d$location_id)) {
      d <- merge(d,locations[,c("location_id","location_name"),with=F],by="location_id",all.x=T)
      d <- merge(d,loclevels[,c("location_id","location_name","loc_level"),with=F],by="location_id",all.x=T)
      d[,location_name:=location_name.x]
      d[is.na(location_name),location_name:=location_name.y]
      d[,location_name.x:=NULL]
      d[,location_name.y:=NULL]
      d <- d[!is.na(location_name)]
      if (any(is.na(d$location_name))) stop("created missing location_names")
    }
    if (is.null(d$location_name)) print("Location name not created")
    if (!keep_ids) d[,location_id:=NULL]
    if (is.null(d$location_name) & is.null(d$location_id)) print("No Location Variable Present")
  }
  
  ## MEASURE
  measures <- fread(paste0(idmap_loc,"measure_map.csv"))
  if (is.null(d$measure_name)) {
    if (!is.null(d$measure_id)) {
      d <- merge(d,measures[,c("measure_id","measure_name"),with=F],by="measure_id",all.x=T)
      if (any(is.na(d$measure_name))) stop("created missing measure_name")
    }
    if (is.null(d$measure_name)) print("measure names not created")
    if (!keep_ids) d[,measure_id:=NULL]
    if (is.null(d$measure_name) & is.null(d$measure_id)) print("No measure Variable Present")
  }
  
  ## METRIC
  metrics <- fread(paste0(idmap_loc,"metric_map.csv"))
  if (is.null(d$metric_name)) {
    if (!is.null(d$metric_id)) {
      d <- merge(d,metrics[,c("metric_id","metric_name"),with=F],by="metric_id",all.x=T)
      if (any(is.na(d$metric_name))) stop("created missing metric_names")
    }
    if (is.null(d$metric_name)) print("metric name not created")
    if (!keep_ids) d[,metric_id:=NULL]
    if (is.null(d$metric_name) & is.null(d$metric_id)) print("No metric Variable Present")
  }
  
  
  ## REI
  reis <- data.table(read.csv(paste0(idmap_loc,"risk_hierarchy.csv")))
  if (is.null(d$rei_name)) {
    if (!is.null(d$rei_id)) {
      d <- merge(d,reis[,c("rei_id","rei_name"),with=F],by="rei_id",all.x=T)
      if (any(is.na(d$rei_name))) stop("created missing rei_names")
    }
    if (is.null(d$rei_name)) print("rei name not created")
    if (!keep_ids) d[,rei_id:=NULL]
    if (is.null(d$rei_name) & is.null(d$rei_id)) print("No rei Variable Present")
  }
  
  if (keep_ids == F) d[,c("rei_id","metric_id","measure_id","location_id","cause_id","sex_id","age_id"):=NULL]
  
  return(d)
  
}


