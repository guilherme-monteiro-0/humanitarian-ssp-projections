list.of.packages <- c("data.table", "reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Modify this for your local computer
wd_base = "~/git/"
setwd(paste0(wd_base, "humanitarian-ssp-projections"))

worldclim = fread("intermediate_data/conflict_clim_bigram.csv")
worldclim$conflict = NULL

emdat = fread("EM-DAT/emdat_110723.csv")
setnames(
  emdat,
  c("ISO","Year","Total Affected"),
  c("iso3","year","affected_persons")
)
emdat_agg = emdat[,.(affected_persons=sum(affected_persons, na.rm=T)),by=.(iso3, year, `Disaster Type`)]
emdat_agg_m = melt(emdat_agg, id.vars=c("iso3", "year", "Disaster Type"))
emdat_agg_w = dcast(emdat_agg_m, iso3+year~`Disaster Type`)
emdat_agg_w[is.na(emdat_agg_w)] = 0
names(emdat_agg_w) = make.names(names(emdat_agg_w))
emdat_agg_w$climate_affected_persons = rowSums(
  emdat_agg_w[,c(
    "Drought",
    "Epidemic",
    "Extreme.temperature",
    "Flood",
    "Fog",
    "Glacial.lake.outburst",
    "Insect.infestation",
    "Storm",
    "Wildfire"
  )]
)
keep = c(
  "iso3",
  "year",
  "climate_affected_persons"
)
emdat_agg_w = emdat_agg_w[,keep]


climate_worldclim = merge(emdat_agg_w, worldclim, all.y=T)
climate_worldclim[is.na(climate_worldclim)] = 0
climate_worldclim = climate_worldclim[,c(
  "climate_affected_persons"
  ,"gdpgrowth"
  ,paste("prec",c(1:12),sep="_")
  ,paste("tmin",c(1:12),sep="_")
  ,paste("tmax",c(1:12),sep="_")
  ,"iso3"
  ,"year"
)]
fwrite(climate_worldclim, "intermediate_data/climate_worldclim.csv")

worldclim_forecasting = fread("intermediate_data/large/conflict_clim_forecasting.csv")
worldclim_forecasting$conflict = NULL

climate_worldclim_forecasting = merge(emdat_agg_w, worldclim_forecasting, all.y=T)
climate_worldclim_forecasting[is.na(climate_worldclim_forecasting)] = 0
climate_worldclim_forecasting = climate_worldclim_forecasting[,c(
  "climate_affected_persons"
  ,"scenario"
  ,"gdpgrowth"
  ,paste("prec",c(1:12),sep="_")
  ,paste("tmin",c(1:12),sep="_")
  ,paste("tmax",c(1:12),sep="_")
  ,"iso3"
  ,"year"
)]
fwrite(climate_worldclim_forecasting, "intermediate_data/large/climate_worldclim_forecasting.csv")