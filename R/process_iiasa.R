list.of.packages <- c("data.table","reshape2", "zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Modify this for your local computer
wd_base = "~/git/"
setwd(paste0(wd_base, "humanitarian-ssp-projections"))

# Population
pop = fread("IIASA/pop.csv")
pop[,c("Model", "Variable", "Unit", "Notes")] = NULL
pop_history = fread("IIASA/pop_history.csv", header=T)
setnames(pop_history, "Scenario (History)", "Scenario")
pop_history[,c("Model", "Variable", "Unit")] = NULL
pop_history_l = melt(pop_history, id.vars=c("Scenario", "Region"), variable.name = "year", value.name = "pop")
pop_history_l = pop_history_l[complete.cases(pop_history_l),]
pop_history_l_agg = data.table(pop_history_l)[,.(pop=mean(pop, na.rm=T)), by=.(Region, year)]
pop_history_w_agg = dcast(pop_history_l_agg, Region~year, value.var="pop")
overlapping_years = intersect(names(pop), names(pop_history_w_agg))
overlapping_years = overlapping_years[which(overlapping_years!="Region")]
pop_history_w_agg[,overlapping_years] = NULL
pop = merge(pop, pop_history_w_agg, by="Region", all.x=T)
pop_l = melt(pop, id.vars=c("Scenario", "Region"), variable.name = "year", value.name = "pop")
pop_l = data.table(pop_l)[,.(pop=mean(pop, na.rm=T)), by=.(Scenario, Region, year)]
pop_l$year = as.numeric(as.character(pop_l$year))
pop_l_years = c(min(pop_l$year):max(pop_l$year))
pop_l_grid = expand.grid(Scenario=unique(pop_l$Scenario), Region=unique(pop_l$Region), year=pop_l_years)
pop_l = merge(pop_l, pop_l_grid, by=c("Scenario", "Region", "year"), all=T)
pop_l_spline = pop_l[,.(year=year, pop=na.spline(pop)), by=.(Scenario, Region)]

# GDP
gdp = fread("IIASA/gdp.csv")
gdp[,c("Model", "Variable", "Unit", "Notes")] = NULL
gdp_history = fread("IIASA/gdp_history.csv", header=T)
setnames(gdp_history, "Scenario (History)", "Scenario")
gdp_history[,c("Model", "Variable", "Unit")] = NULL
gdp_history_l = melt(gdp_history, id.vars=c("Scenario", "Region"), variable.name = "year", value.name = "gdp")
gdp_history_l = gdp_history_l[complete.cases(gdp_history_l),]
gdp_history_l_agg = data.table(gdp_history_l)[,.(gdp=mean(gdp, na.rm=T)), by=.(Region, year)]
gdp_history_w_agg = dcast(gdp_history_l_agg, Region~year, value.var="gdp")
overlapping_years = intersect(names(gdp), names(gdp_history_w_agg))
overlapping_years = overlapping_years[which(overlapping_years!="Region")]
gdp_history_w_agg[,overlapping_years] = NULL
gdp = merge(gdp, gdp_history_w_agg, by="Region", all.x=T)
gdp_l = melt(gdp, id.vars=c("Scenario", "Region"), variable.name = "year", value.name = "gdp")
gdp_l = data.table(gdp_l)[,.(gdp=mean(gdp, na.rm=T)), by=.(Scenario, Region, year)]
gdp_l$year = as.numeric(as.character(gdp_l$year))
gdp_l_years = c(min(gdp_l$year):max(gdp_l$year))
gdp_l_grid = expand.grid(Scenario=unique(gdp_l$Scenario), Region=unique(gdp_l$Region), year=gdp_l_years)
gdp_l = merge(gdp_l, gdp_l_grid, by=c("Scenario", "Region", "year"), all=T)
gdp_l_spline = gdp_l[,.(year=year, gdp=na.spline(gdp)), by=.(Scenario, Region)]


# Urban
urban = fread("IIASA/urban.csv")
urban[,c("Model", "Variable", "Unit", "Notes")] = NULL
urban_history = fread("IIASA/urban_history.csv", header=T)
setnames(urban_history, "Scenario (History)", "Scenario")
urban_history[,c("Model", "Variable", "Unit")] = NULL
urban_history_l = melt(urban_history, id.vars=c("Scenario", "Region"), variable.name = "year", value.name = "urban")
urban_history_l = urban_history_l[complete.cases(urban_history_l),]
urban_history_l_agg = data.table(urban_history_l)[,.(urban=mean(urban, na.rm=T)), by=.(Region, year)]
urban_history_w_agg = dcast(urban_history_l_agg, Region~year, value.var="urban")
overlapping_years = intersect(names(urban), names(urban_history_w_agg))
overlapping_years = overlapping_years[which(overlapping_years!="Region")]
urban_history_w_agg[,overlapping_years] = NULL
urban = merge(urban, urban_history_w_agg, by="Region", all.x=T)
urban_l = melt(urban, id.vars=c("Scenario", "Region"), variable.name = "year", value.name = "urban")
urban_l = data.table(urban_l)[,.(urban=mean(urban, na.rm=T)), by=.(Scenario, Region, year)]
urban_l$year = as.numeric(as.character(urban_l$year))
urban_l_years = c(min(urban_l$year):max(urban_l$year))
urban_l_grid = expand.grid(Scenario=unique(urban_l$Scenario), Region=unique(urban_l$Region), year=urban_l_years)
urban_l = merge(urban_l, urban_l_grid, by=c("Scenario", "Region", "year"), all=T)
urban_l_spline = urban_l[,.(year=year, urban=na.spline(urban)), by=.(Scenario, Region)]


iiasa = merge(pop_l_spline, gdp_l_spline, by=c("Scenario", "Region", "year"), all=T)
iiasa = merge(iiasa, urban_l_spline, by=c("Scenario", "Region", "year"), all=T)
fwrite(iiasa,"intermediate_data/iiasa.csv")
save(iiasa,file="intermediate_data/iiasa.RData")
