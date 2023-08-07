list.of.packages <- c("data.table", "reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Modify this for your local computer
wd_base = "~/git/"
setwd(paste0(wd_base, "humanitarian-ssp-projections"))

worldclim = fread("intermediate_data/conflict_clim_bigram.csv")
worldclim$conflict = NULL

displacement = fread("intermediate_data/large/iiasa_unhcr_displaced2_bigram.csv")
setnames(displacement, "Region", "iso3")
displacement = displacement[,c("displaced_persons", "iso3", "year")]

displacement_worldclim = merge(displacement, worldclim)
displacement_worldclim = displacement_worldclim[,c(
  "displaced_persons"
  ,"gdpgrowth"
  ,paste("prec",c(1:12),sep="_")
  # ,paste("tmin",c(1:12),sep="_")
  ,paste("tmax",c(1:12),sep="_")
  ,"iso3"
  ,"year"
)]
fwrite(displacement_worldclim, "intermediate_data/displacement_worldclim.csv")

worldclim_forecasting = fread("intermediate_data/large/conflict_clim_forecasting.csv")
worldclim_forecasting$conflict = NULL

displacement_forecasting = fread("intermediate_data/large/iiasa_unhcr_displaced2_forecasting.csv")
setnames(displacement_forecasting, "Region", "iso3")
setnames(displacement_forecasting, "Scenario", "scenario")
displacement_forecasting$scenario = tolower(displacement_forecasting$scenario)
displacement_forecasting = displacement_forecasting[,c("displaced_persons", "scenario", "iso3", "year")]

displacement_worldclim_forecasting = merge(displacement_forecasting, worldclim_forecasting)
displacement_worldclim_forecasting = displacement_worldclim_forecasting[,c(
  "displaced_persons"
  ,"scenario"
  ,"gdpgrowth"
  ,paste("prec",c(1:12),sep="_")
  # ,paste("tmin",c(1:12),sep="_")
  ,paste("tmax",c(1:12),sep="_")
  ,"iso3"
  ,"year"
)]
fwrite(displacement_worldclim_forecasting, "intermediate_data/large/displacement_worldclim_forecasting.csv")