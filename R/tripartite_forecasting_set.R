list.of.packages <- c("data.table", "reshape2", "igraph", "dplyr", "foreach", "doSNOW","snow", "doParallel",
                      "sp","rgdal","rgeos","maptools", "sf", "leaflet", "geosphere", "s2", "zoo")
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
displacement$displaced_persons = displacement$y_hat
displacement$displaced_persons[which(displacement$displaced_persons<0)] = 0
keep = c(
  "displaced_persons",
  "scenario",
  "iso3",
  "year"
)
displacement = displacement[,keep, with=F]
forecasting_set = merge(displacement, centroids, by="iso3")

load("intermediate_data/iiasa.RData")
setnames(iiasa,c("Scenario","Region"),c("scenario", "iso3"))
iiasa$scenario = tolower(iiasa$scenario)
iiasa = iiasa[,c("scenario","iso3", "year", "pop")]
forecasting_set = merge(forecasting_set, iiasa, by=c("scenario","iso3", "year"))

# climate = fread("~/git/saint/outputs/regression_climate_worldclim_forecast.csv")
# climate$climate_disasters = climate$y_hat
# keep = c(
#   "climate_disasters",
#   "scenario",
#   "iso3",
#   "year"
# )
# climate = climate[,keep,with=F]
# forecasting_set = merge(forecasting_set, climate, by=c("scenario", "iso3", "year"))
# load("INFORM/interpolated_inform.RData")
# inform_hist = subset(inform, IndicatorId=="AFF_DR" & Scenario=="Historical")
# inform_future = subset(inform, IndicatorId=="AFF_DR" & Scenario %in% c("RCP45-SSP1","RCP45-SSP2","RCP85-SSP3","RCP85-SSP5"))
# hist_grid = expand.grid(
#   Iso3=unique(inform_hist$Iso3),
#   Year=unique(inform_hist$Year),
#   Scenario=c("RCP45-SSP1","RCP45-SSP2","RCP85-SSP3","RCP85-SSP5")
#   )
# inform_hist$Scenario = NULL
# inform_hist = merge(inform_hist, hist_grid, all=T)
# inform = rbind(inform_hist, inform_future)
# inform = inform[,c("Iso3", "Year", "Scenario", "IndicatorScore")]
# names(inform) = c("iso3","year", "scenario", "drought")
# scenario_map = c(
#   "RCP45-SSP1"="ssp1",
#   "RCP45-SSP2"="ssp2",
#   "RCP85-SSP3"="ssp3",
#   "RCP85-SSP5"="ssp5"
#   )
# inform$scenario = scenario_map[inform$scenario]
# forecasting_set = merge(forecasting_set, inform, by=c("scenario","iso3", "year"))
worldclim_hist = fread("./WorldClim/ACCESS-CM2/processed/historical.csv")
setnames(worldclim_hist,"ISO_A3", "iso3")
worldclim_future = fread("./WorldClim/ACCESS-CM2/processed/future.csv")
setnames(worldclim_future,"ISO_A3", "iso3")
worldclim_hist = subset(worldclim_hist, iso3 %in% unique(worldclim_future$iso3))
worldclim_grid = expand.grid(
  scenario=unique(worldclim_future$scenario),
  iso3=unique(worldclim_hist$iso3),
  year=unique(worldclim_hist$year)
)
worldclim_hist = merge(worldclim_hist, worldclim_grid, all=T)
worldclim = rbindlist(list(worldclim_hist, worldclim_future), use.names=T)
worldclim$prec = rowSums(
  worldclim[,c(
    "prec_1",
    "prec_2",
    "prec_3",
    "prec_4",
    "prec_5",
    "prec_6",
    "prec_7",
    "prec_8",
    "prec_9",
    "prec_10",
    "prec_11",
    "prec_12"
  )]
)
worldclim$tmax = rowMeans(
  worldclim[,c(
    "tmax_1",
    "tmax_2",
    "tmax_3",
    "tmax_4",
    "tmax_5",
    "tmax_6",
    "tmax_7",
    "tmax_8",
    "tmax_9",
    "tmax_10",
    "tmax_11",
    "tmax_12"
  )]
)
worldclim = worldclim[,c("iso3","year","scenario","prec","tmax")]

worldclim_grid2 = expand.grid(
  scenario=unique(worldclim$scenario),
  iso3=unique(worldclim$iso3),
  year=min(worldclim$year):max(worldclim$year)
)
worldclim = merge(worldclim, worldclim_grid2, all=T)
worldclim = worldclim[order(worldclim$scenario, worldclim$iso3, worldclim$year),]
worldclim = worldclim[,.(
  prec=na.approx(prec),
  tmax=na.approx(tmax),
  year=year
), by=.(scenario, iso3)]
forecasting_set = merge(forecasting_set, worldclim, by=c("scenario","iso3", "year"))


conflict = fread("~/git/saint/outputs/binary_conflict_clim_bigram_forecast.csv")
conflict$conflict = conflict$y_hat
keep = c("scenario", "iso3", "year", "conflict")
conflict = conflict[,keep, with=F]
forecasting_set = merge(forecasting_set, conflict, by=c("scenario","iso3", "year"))

# load("./fts/plans.RData")
# fts_plans = subset(fts_plans,!is.na(location_iso3))
# fts_aggregate = fts_plans[,.(humanitarian_needs=sum(original_requirements,na.rm=T)),by=.(year,location_iso3)]
# setnames(fts_aggregate,"location_iso3", "iso3")
# forecasting_set = merge(forecasting_set, fts_aggregate, by=c("iso3", "year"), all.x=T)
# forecasting_set$humanitarian_needs[which(is.na(forecasting_set$humanitarian_needs))] = 0
# iati = fread("./IATI/humanitarian_iati.csv")
# names(iati) = c(
#   "year",
#   "x_recipient_code",
#   "transaction_type",
#   "value",
#   "activities",
#   "publishers"
# )
# iati_isos = fread("IATI/isos.csv")
# iati = merge(iati, iati_isos)
# iati = subset(iati, transaction_type %in% c("Disbursement", "Expenditure"))
# iati = iati[,.(value=sum(value, na.rm=T), activities=sum(activities), publishers=sum(publishers)), by=.(
#   year, iso3
# )]
# iati_years = iati[,.(activities=sum(activities), publishers=sum(publishers)), by=.(year)]
# iati = subset(iati, year > 1970 & year < 2023 & value > 0)
# iati$value = iati$value / iati$publishers
# iati = iati[,c("iso3","year","value")]
# setnames(iati, "value", "humanitarian_needs")
# forecasting_set = merge(forecasting_set, iati, by=c("iso3", "year"), all.x=T)
# forecasting_set$humanitarian_needs[which(is.na(forecasting_set$humanitarian_needs))] = 0
# forecasting_set = subset(forecasting_set, year > 1997)
# pin = fread("intermediate_data/pin.csv")
# setnames(pin, "value", "humanitarian_needs")
# forecasting_set = merge(forecasting_set, pin, by=c("iso3", "year"), all.x=T)
# forecasting_set$humanitarian_needs[which(is.na(forecasting_set$humanitarian_needs))] = 0
# forecasting_set = subset(forecasting_set, year > 2017)
hum_spend = fread("supporting_data/hum_spend.csv", na.strings=c("","-"))
hum_spend$humanitarian_needs = as.numeric(gsub(",","",hum_spend$hum_spend))
hum_spend = hum_spend[,c("iso","year","humanitarian_needs")]
setnames(hum_spend, "iso", "iso3")
forecasting_set = merge(forecasting_set, hum_spend, by=c("iso3", "year"), all.x=T)
forecasting_set$humanitarian_needs[which(is.na(forecasting_set$humanitarian_needs))] = 0
forecasting_set = subset(forecasting_set, year >= 2013)


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
  "pop",
  "displaced_persons",
  "tmax",
  "prec",
  "conflict",
  "iso3",
  # "lat",
  # "lon",
  "year"
)]
fwrite(forecasting_set, "./intermediate_data/tripartite_bigram_forecasting.csv")
