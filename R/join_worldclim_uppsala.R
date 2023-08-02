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

worldclim = fread("./WorldClim/ACCESS-CM2/processed/historical.csv")
setnames(worldclim,"ISO_A3", "iso3")
conflict = merge(conflict, worldclim)
conflict = conflict[,c(
  "conflict",
  "prec",
  "tmin",
  "tmax",
  "iso3",
  "year"
)]
fwrite(conflict,"intermediate_data/conflict_clim.csv")
