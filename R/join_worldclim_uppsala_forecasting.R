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

worldclim = merge(worldclim, conflict, all.x=T)
diff.isos = setdiff(worldclim$iso3, conflict$iso3)
worldclim = subset(worldclim, !iso3 %in% diff.isos)
worldclim = worldclim[,c(
  "conflict",
  "scenario",
  "prec",
  "tmin",
  "tmax",
  "iso3",
  "year"
)]
fwrite(worldclim,"intermediate_data/conflict_clim_forecasting.csv")
