list.of.packages <- c("data.table", "reshape2", "igraph", "dplyr", "foreach", "doSNOW","snow", "doParallel",
                      "sp","rgdal","rgeos","maptools", "sf", "leaflet", "geosphere", "s2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Modify this for your local computer
wd_base = "~/git/"
setwd(paste0(wd_base, "humanitarian-ssp-projections"))

load(file="./intermediate_data/land_and_sea.RData")

centroid_list = list()
for(iso3 in land_and_sea$ISO_A3){
  poly = land_and_sea[which(land_and_sea$ISO_A3==iso3),]
  centroid = gCentroid(poly)
  centroid_df = data.frame(lon=centroid$x, lat=centroid$y, iso3=iso3)
  centroid_list[[iso3]] = centroid_df
}
centroids = rbindlist(centroid_list)
centroids$iso3[which(centroids$iso3=="ZAR")] = "COD"
centroids$iso3[which(centroids$iso3=="ROM")] = "ROU"
centroids$iso3[which(centroids$iso3=="TMP")] = "TLS"

displacement = fread("~/git/saint/outputs/regression_iiasa_unhcr_displaced2_forecast.csv")
displacement = subset(displacement, Scenario=="SSP1")
keep = c(
  "displaced_persons",
  "Region",
  "year",
  "pop",
  "gdp",
  "urban"
)
displacement = displacement[,keep, with=F]
setnames(displacement, "Region", "iso3")
training_set = merge(displacement, centroids, by="iso3")

load("./INFORM/interpolated_inform.RData")
climate = inform
rm(inform)
names(climate) = c("iso3", "variable", "year", "scenario", "value")
climate = subset(climate, scenario=="Historical")
climate$scenario = NULL
climate_w = dcast(climate, iso3+year~variable)
climate_w$climate_affected_persons = rowSums(
  climate_w[,c(
    "AFF_DR", "EX_EQ_MMI6", "EX_EQ_MMI8", "EX_TC_SS3", "EX_TC_SS1", "EX_TS", "EX_FL"
  )], na.rm=T
)
keep = c(
  "climate_affected_persons",
  "iso3",
  "year"
)
climate_w = climate_w[,keep]
training_set = merge(training_set, climate_w, by=c("iso3", "year"))

load("./uppsala_replication/PredictionSSP_1.RData")
conflict = PredictionSSP_1
rm(PredictionSSP_1)
gc()
conflict_iso3 = fread("./supporting_data/uppsala_iso3.csv")
conflict = merge(conflict, conflict_iso3)
keep = c(
  "conflict",
  "temp",
  "iso3",
  "year"
)
conflict = conflict[,keep]
training_set = merge(training_set, conflict, by=c("iso3", "year"))

load("./fts/plans.RData")
fts_plans = subset(fts_plans,!is.na(location_iso3) & original_requirements > 0)
fts_aggregate = fts_plans[,.(humanitarian_needs=sum(original_requirements,na.rm=T)),by=.(year,location_iso3)]
setnames(fts_aggregate,"location_iso3", "iso3")
training_set = merge(training_set, fts_aggregate, by=c("iso3", "year"), all.x=T)
training_set$humanitarian_needs[which(is.na(training_set$humanitarian_needs))] = 0

training_set = training_set[,c(
  "humanitarian_needs",
  "displaced_persons",
  "climate_affected_persons",
  "conflict",
  "temp",
  "gdp",
  "pop",
  "urban",
  "iso3",
  "lat",
  "lon",
  "year"
)]
fwrite(training_set, "./intermediate_data/tripartite.csv")