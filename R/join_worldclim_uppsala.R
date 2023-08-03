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
worldclim = subset(worldclim, year<=2013)
setnames(worldclim,"ISO_A3", "iso3")
conflict = merge(conflict, worldclim)
conflict = conflict[,c(
  "conflict"
  ,paste("prec",c(1:12),sep="_")
  ,paste("tmin",c(1:12),sep="_")
  ,paste("tmax",c(1:12),sep="_")
  ,"iso3"
  ,"year"
)]
fwrite(conflict,"intermediate_data/conflict_clim.csv")
