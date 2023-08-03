list.of.packages <- c("data.table", "reshape2", "igraph", "dplyr", "foreach", "doSNOW","snow", "doParallel")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Modify this for your local computer
wd_base = "~/git/"
setwd(paste0(wd_base, "humanitarian-ssp-projections"))

load("./uppsala_replication/PredictionSSP_1.RData")
conflict = PredictionSSP_1
rm(PredictionSSP_1)
gc()
conflict_iso3 = fread("./supporting_data/uppsala_iso3.csv")
conflict = merge(conflict, conflict_iso3)

keep = c(
  "conflict",
  "iso3",
  "year"
)
conflict = conflict[,keep]
conflict = subset(conflict, year<=2013)
conflict$conflict[which(conflict$conflict==2)] = 1

worldclim_hist = fread("./WorldClim/ACCESS-CM2/processed/historical.csv")
setnames(worldclim_hist,"ISO_A3", "iso3")
worldclim_future = fread("./WorldClim/ACCESS-CM2/processed/future.csv")
setnames(worldclim_future,"ISO_A3", "iso3")
worldclim_grid = expand.grid(
  scenario=unique(worldclim_future$scenario),
  iso3=unique(worldclim_hist$iso3),
  year=unique(worldclim_hist$year)
)
worldclim_hist = merge(worldclim_hist, worldclim_grid, all=T)
worldclim = rbindlist(list(worldclim_hist, worldclim_future), use.names=T)

conflict = merge(conflict, worldclim, all.y=T)
conflict = conflict[,c(
  "conflict"
  ,"scenario"
  ,paste("prec",c(1:12),sep="_")
  ,paste("tmin",c(1:12),sep="_")
  ,paste("tmax",c(1:12),sep="_")
  ,"iso3"
  ,"year"
)]
training = fread("intermediate_data/conflict_clim.csv")
out_of_set = setdiff(conflict$iso3, training$iso3)
conflict = subset(conflict, !iso3 %in% out_of_set)
fwrite(conflict,"intermediate_data/large/conflict_clim_forecasting.csv")
