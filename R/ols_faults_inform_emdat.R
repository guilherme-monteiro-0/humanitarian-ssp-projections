list.of.packages <- c("data.table","plm","Hmisc","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Modify this for your local computer
wd_base = "~/git/"
setwd(paste0(wd_base, "humanitarian-ssp-projections"))

load("INFORM/interpolated_inform.RData")
inform = subset(inform, Scenario=="Historical")
setnames(inform, c("Iso3", "Year"), c("iso3", "year"))
inform[,c("Scenario")] = NULL
inform_m = melt(inform, id.vars=c("iso3", "year", "IndicatorId"), measure.vars = "IndicatorScore")
inform_w = dcast(inform_m, iso3+year~IndicatorId)

load("intermediate_data/processed_faults.RData")
setnames(faults_agg, "count", "faults_count")

analysis_set = merge(inform_w, faults_agg, by="iso3", all.x=T)

fit = lm(EX_EQ_MMI6~faults_count, data=analysis_set)
summary(fit)

emdat = fread("EM-DAT/emdat_110723.csv")
setnames(emdat,"ISO","iso3")
emdat_agg = emdat[,.(count=.N),by=.(iso3, Year, `Disaster Type`)]
emdat_agg_m = melt(emdat_agg, id.vars=c("iso3", "Year", "Disaster Type"))
emdat_agg_w = dcast(emdat_agg_m, iso3+Year~`Disaster Type`)
emdat_agg_w[is.na(emdat_agg_w)] = 0
names(emdat_agg_w) = make.names(names(emdat_agg_w))

analysis_set2 = merge(emdat_agg_w, faults_agg, by="iso3", all.x=T)

fit = lm(Earthquake~faults_count, data=analysis_set2)
summary(fit)

load("fts/plans.RData")

fts_plans = subset(fts_plans,!is.na(location_iso3) & original_requirements > 0)
fts_aggregate = fts_plans[,.(humanitarian_needs=sum(original_requirements,na.rm=T)),by=.(year,location_iso3)]
setnames(fts_aggregate,c("location_iso3","year"), c("iso3","Year"))

analysis_set3 = merge(fts_aggregate, emdat_agg_w, by=c("iso3", "Year"), all.x=T)
analysis_set3[is.na(analysis_set3)] = 0

fit = lm(humanitarian_needs~
    Animal.accident+
    Complex.Disasters+
    Drought+
    Earthquake+
    Epidemic+
    Extreme.temperature+
    Flood+
    Industrial.accident+
    Insect.infestation+
    Landslide+
    Mass.movement..dry.+
    Miscellaneous.accident+
    Storm+
    Transport.accident+
    Volcanic.activity+
    Wildfire
    , data=analysis_set3)
summary(fit)

emdat_agg2 = emdat[,.(
  affected=sum(`Total Affected`, na.rm=T),
  disaster_costs=sum(`Total Damages, Adjusted ('000 US$)`, na.rm=T)
  ),by=.(iso3, Year)]

analysis_set4 = merge(fts_aggregate, emdat_agg2, by=c("iso3", "Year"), all.x=T)
analysis_set4[is.na(analysis_set4)] = 0

fit = lm(humanitarian_needs~affected+disaster_costs, data=analysis_set4)
summary(fit)
