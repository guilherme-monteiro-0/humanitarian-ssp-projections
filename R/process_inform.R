list.of.packages <- c("data.table","openxlsx","Hmisc", "zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Modify this for your local computer
wd_base = "~/git/"
setwd(paste0(wd_base, "humanitarian-ssp-projections"))

projected = read.xlsx("INFORM/INFORM CC Risk Index all scenarios.xlsx", sheet=5)
historical = read.xlsx("INFORM/INFORM2022_TREND_2013_2022_v063_ALL.xlsx")

projected = subset(projected, IndicatorId %in% unique(historical$IndicatorId))
projected$Scenario[which((projected$Scenario=="INFORM Climate Change Risk 2022"))] = "INFORM Climate Change Risk 2022 Historical"
projected$Scenario = substr(projected$Scenario, 33, nchar(projected$Scenario))

historical = subset(historical, IndicatorId %in% unique(projected$IndicatorId))
historical = subset(historical, INFORMYear < 2022)
inform_indicators = unique(historical[,c("IndicatorId","IndicatorName")])
historical[,c("SurveyYear", "Indicator.Type", "IndicatorName")] = NULL
setnames(historical,"INFORMYear", "Year")
historical$Scenario = "Historical"

inform = rbind(historical, projected)

indicators = unique(inform$IndicatorId)
countries = unique(inform$Iso3)
years = c(2014:2100)
scenarios = unique(inform$Scenario)
expansion = expand.grid(list(
  IndicatorId=indicators,
  Iso3=countries,
  Year=years,
  Scenario=scenarios
))
expansion = subset(expansion, (Year<2022 & Scenario=="Historical") | (Year>2022 & Scenario!="Historical"))

inform = merge(inform, expansion, all=T)
inform = inform[order(inform$Year),]

combinations = length(indicators) * length(countries) * (length(scenarios) - 1)
pb = txtProgressBar(max=combinations, style=3)
c_i = 0

for(indicator in indicators){
  for(country in countries){
    for(scenario in scenarios){
      if(scenario != "Historical"){
        c_i = c_i + 1
        setTxtProgressBar(pb, c_i)
        selection_criteria = which(
          inform$Iso3==country &
            inform$IndicatorId==indicator &
            (inform$Scenario==scenario | (
              inform$Scenario=="Historical" & inform$Year==2022
            ))
        )
        if(sum(!is.na(inform$IndicatorScore[selection_criteria])) > 1){
          inform$IndicatorScore[selection_criteria] = na.approx(inform$IndicatorScore[selection_criteria],rule=2)
        }
      }
    }
  }
}
close(pb)
save(inform, inform_indicators, file="INFORM/interpolated_inform.RData")
