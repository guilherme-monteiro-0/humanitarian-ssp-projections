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

displacement = fread("~/git/saint/outputs/regression_displacement_worldclim_forecast.csv")
displacement$displaced_persons[which(displacement$year>=2023)] = displacement$y_hat[which(displacement$year>=2023)]
displacement$displaced_persons[which(displacement$displaced_persons<0)] = 0
keep = c(
  "displaced_persons",
  "scenario",
  "iso3",
  "year"
)
displacement = displacement[,keep, with=F]
forecasting_set = merge(displacement, centroids, by="iso3")

climate = fread("~/git/saint/outputs/regression_climate_worldclim_forecast.csv")
climate$climate_disasters[which(climate$year>=2014)] = climate$y_hat[which(climate$year>=2014)]
keep = c(
  "climate_disasters",
  "scenario",
  "iso3",
  "year"
)
climate = climate[,keep,with=F]
forecasting_set = merge(forecasting_set, climate, by=c("scenario", "iso3", "year"))

conflict = fread("~/git/saint/outputs/binary_conflict_clim_bigram_forecast.csv")
conflict$conflict[which(conflict$year>=2014)] = conflict$y_hat[which(conflict$year>=2014)]
keep = c("scenario", "iso3", "year", "conflict")
conflict = conflict[,keep, with=F]
forecasting_set = merge(forecasting_set, conflict, by=c("scenario","iso3", "year"))

load("./fts/plans.RData")
fts_plans = subset(fts_plans,!is.na(location_iso3))
fts_aggregate = fts_plans[,.(humanitarian_needs=sum(original_requirements,na.rm=T)),by=.(year,location_iso3)]
setnames(fts_aggregate,"location_iso3", "iso3")
forecasting_set = merge(forecasting_set, fts_aggregate, by=c("iso3", "year"), all.x=T)
forecasting_set$humanitarian_needs[which(is.na(forecasting_set$humanitarian_needs))] = 0

forecasting_set = forecasting_set[order(forecasting_set$scenario, forecasting_set$iso3, forecasting_set$year),]
# forecasting_set$pop = forecasting_set$pop * 1e6

# forecasting_set$humanitarian_needs = forecasting_set$humanitarian_needs / (forecasting_set$pop * 1e6)
# forecasting_set$displaced_persons = forecasting_set$displaced_persons / (forecasting_set$pop * 1e6)
# forecasting_set$climate_affected_persons = forecasting_set$climate_affected_persons / (forecasting_set$pop * 1e6)
# forecasting_set$pop = NULL
# 
# forecasting_set$displaced_persons[which(forecasting_set$displaced_persons > forecasting_set$pop)] = 
#   forecasting_set$pop[which(forecasting_set$displaced_persons > forecasting_set$pop)]
# forecasting_set$climate_affected_persons[which(forecasting_set$climate_affected_persons > forecasting_set$pop)] = 
#   forecasting_set$pop[which(forecasting_set$climate_affected_persons > forecasting_set$pop)]
# forecasting_set$climate_affected_persons[which(forecasting_set$climate_affected_persons < 0)] = 0

# forecasting_set$scenario = paste(
#   forecasting_set$scenario,
#   forecasting_set$iso3,
#   forecasting_set$year,
#   sep="|"
# )
forecasting_set = forecasting_set[,c(
  "humanitarian_needs",
  "scenario",
  # "pop",
  "displaced_persons",
  "climate_disasters",
  "conflict",
  "iso3",
  # "lat",
  # "lon",
  "year"
)]
fwrite(forecasting_set, "./intermediate_data/tripartite_bigram_forecasting.csv")
