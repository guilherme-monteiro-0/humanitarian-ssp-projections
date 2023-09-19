list.of.packages <- c("data.table","reshape2", "ggplot2", "scales", "openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd_base = "~/git/"
setwd(paste0(wd_base, "humanitarian-ssp-projections"))

unhcr_pop = fread("./UNHCR/population.csv", skip=14, na.strings=c("","-"))
keep = c(
  "Year",
  "Country of origin (ISO)",
  "Refugees under UNHCR's mandate",
  "Asylum-seekers",
  "IDPs of concern to UNHCR"
)
unhcr_pop = unhcr_pop[,keep, with=F]
names(unhcr_pop) = c("year", "iso3", "refugees", "asylum", "idps")
unhcr_pop_l = melt(unhcr_pop, id.vars=c("year", "iso3"))
unhcr_pop_l$iso3[which(unhcr_pop_l$iso3=="ESH")] = "MAR" # No Western Sahara in World network
unhcr_pop_agg = data.table(unhcr_pop_l)[,.(displaced_persons=sum(value,na.rm=T)), by=.(year)]
unhcr_pop_agg = subset(unhcr_pop_agg, year > 1992)

worldclim_hist = fread("./WorldClim/ACCESS-CM2/processed/historical.csv")
setnames(worldclim_hist,"ISO_A3", "iso3")

worldclim_hist$prec = rowSums(
  worldclim_hist[,c(
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
worldclim_hist$tmax = rowMeans(
  worldclim_hist[,c(
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
keep = c("iso3", "year", "prec", "tmax")
worldclim_hist = worldclim_hist[,keep, with=F]

worldclim_hist1 = subset(worldclim_hist, year >= 1960 & year <= 1980)
worldclim_hist1 = worldclim_hist1[,.(tmax=mean(tmax), prec=mean(prec))]
worldclim_hist1$year = 1980

worldclim_hist2 = subset(worldclim_hist, year >= 1980 & year <= 2000)
worldclim_hist2 = worldclim_hist2[,.(tmax=mean(tmax), prec=mean(prec))]
worldclim_hist2$year = 2000

worldclim_hist3 = subset(worldclim_hist, year >= 2000 & year <= 2020)
worldclim_hist3 = worldclim_hist3[,.(tmax=mean(tmax), prec=mean(prec))]
worldclim_hist3$year = 2020

worldclim_hist = rbindlist(list(worldclim_hist1, worldclim_hist2, worldclim_hist3))
worldclim_hist$scenario = "Historical"

worldclim_future = fread("./WorldClim/ACCESS-CM2/processed/future.csv")
setnames(worldclim_future,"ISO_A3", "iso3")
worldclim_future$prec = rowSums(
  worldclim_future[,c(
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
worldclim_future$tmax = rowMeans(
  worldclim_future[,c(
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
keep = c("scenario", "iso3", "year", "prec", "tmax")
worldclim_future = worldclim_future[,keep, with=F]
worldclim_future = worldclim_future[,.(tmax=mean(tmax), prec=mean(prec)), by=.(scenario, year)]
worldclim_future$scenario = toupper(worldclim_future$scenario)
worldclim = rbind(worldclim_hist, worldclim_future)

reds = c(
  "#e84439", "#f8c1b2", "#f0826d", "#bc2629", "#8f1b13", "#fce3dc", "#fbd7cb", "#f6b0a0", "#ec6250", "#dc372d", "#cd2b2a", "#a21e25", "#6b120a"
)
oranges = c(
  "#eb642b", "#f6bb9d", "#f18e5e", "#d85b31", "#973915", "#fde5d4", "#fcdbbf", "#facbad", "#f3a47c", "#ee7644", "#cb5730", "#ac4622", "#7a2e05"
)
yellows = c(
  "#f49b21", "#fccc8e", "#f9b865", "#e48a00", "#a85d00", "#feedd4", "#fee7c1", "#fedcab", "#fac47e", "#f7a838", "#df8000", "#ba6b15", "#7d4712"
)
pinks = c(
  "#c2135b", "#e4819b", "#d64278", "#ad1257", "#7e1850", "#f9cdd0", "#f6b8c1", "#f3a5b6", "#e05c86", "#d12568", "#9f1459", "#8d0e56", "#65093d"
)
purples = c(
  "#893f90", "#c189bb", "#a45ea1", "#7b3b89", "#551f65", "#ebcfe5", "#deb5d6", "#cb98c4", "#af73ae", "#994d98", "#732c85", "#632572", "#42184c"
)
blues = c(
  "#0089cc", "#88bae5", "#5da3d9", "#0071b1", "#0c457b", "#d3e0f4", "#bcd4f0", "#a3c7eb", "#77adde", "#4397d3", "#105fa3", "#00538e", "#0a3a64"
)
greens = c(
  "#109e68", "#92cba9", "#5ab88a", "#1e8259", "#16513a", "#c5e1cb", "#b1d8bb", "#a2d1b0", "#74bf93", "#3b8c61", "#00694a", "#005b3e", "#07482e"
)
greys = c(
  "#6a6569", "#a9a6aa", "#847e84", "#555053", "#443e42", "#d9d4da", "#cac5cb", "#b3b0b7", "#b9b5bb", "#5a545a", "#736e73", "#4e484c", "#302b2e"
)

di_style = theme_bw() +
  theme(
    panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.minor.x = element_blank()
    ,panel.grid.major.y = element_line(colour = greys[2])
    ,panel.grid.minor.y = element_blank()
    ,panel.background = element_blank()
    # ,plot.background = element_blank()
    ,axis.line.x = element_line(colour = "black")
    ,axis.line.y = element_blank()
    ,axis.ticks = element_blank()
    ,legend.position = "bottom"
  )
rotate_x_text_45 = theme(
  axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
)
worldclim$year = factor(
  worldclim$year,
  levels = c(1980, 2000, 2020, 2040, 2060, 2080, 2100),
  labels = c(
    "1960-1980",
    "1980-2000",
    "2000-2020",
    "2020-2040",
    "2040-2060",
    "2060-2080",
    "2080-2100"
  )
)

unhcr_pop_agg$displaced_persons_mil = unhcr_pop_agg$displaced_persons / 1e6
p0 = ggplot(unhcr_pop_agg, aes(x=year, y=displaced_persons_mil)) +
  geom_bar(stat="identity", fill=reds[1]) +
  scale_y_continuous(
    expand = c(0, 0),
    ) + # Force y-grid to start at x-axis
  scale_x_continuous(n.breaks=7) +
  expand_limits(y=c(0,105)) +
  di_style +
  rotate_x_text_45 + # Or chose _90 or remove for horizontal
  labs(
    y="Millions of displaced persons",
    x="",
    fill=""
  )
p0
ggsave("outputs/p0_displacement.png", plot = p0, width = 5, height = 3)
fwrite(unhcr_pop_agg, "outputs/p0_displacement.csv")


p1 = ggplot(worldclim, aes(x=year, y=tmax, group=scenario, fill=scenario)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=reds) +
  scale_y_continuous(expand = c(0, 0)) + # Force y-grid to start at x-axis
  di_style +
  rotate_x_text_45 + # Or chose _90 or remove for horizontal
  labs(
    y="Mean annual maximum monthly global temperature (°C)",
    x="",
    fill=""
  )
p1
ggsave("outputs/p1_tmax.png", plot = p1, width = 12, height = 8)
fwrite(worldclim, "outputs/p1_tmax_and_p2_prec.csv")

prec_baseline = 1000
worldclim$prec = worldclim$prec - prec_baseline

p2 = ggplot(worldclim, aes(x=year, y=prec, group=scenario, fill=scenario)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=purples) +
  scale_y_continuous(expand = c(0, 0), labels = function(y) y + prec_baseline) + # Force y-grid to start at x-axis
  di_style +
  rotate_x_text_45 + # Or chose _90 or remove for horizontal
  labs(
    y="Mean annual precipitation per 10' square (mm)",
    x="",
    fill=""
  )
p2 
ggsave("outputs/p2_prec.png", plot = p2, width = 12, height = 8)


load("./uppsala_replication/PredictionSSP_1.RData")
conflict = PredictionSSP_1
rm(PredictionSSP_1)
gc()
conflict_iso3 = fread("./supporting_data/uppsala_iso3.csv")
conflict = merge(conflict, conflict_iso3)

keep = c(
  "conflict",
  "iso3",
  "year",
  "PopSSP1",
  "PopSSP2",
  "PopSSP3",
  "PopSSP5",
  "YMHEPSSP1",
  "YMHEPSSP2",
  "YMHEPSSP3",
  "YMHEPSSP5",
  "GDPcapSSP1",
  "GDPcapSSP2",
  "GDPcapSSP3",
  "GDPcapSSP5"
)
conflict = conflict[,keep]
conflict_l = melt(conflict,id.vars=c("conflict","iso3","year"))
conflict_l$variable = as.character(conflict_l$variable)
conflict_l$scenario = tolower(substr(conflict_l$variable, nchar(conflict_l$variable)-3, nchar(conflict_l$variable)))
conflict_l$variable = substr(conflict_l$variable, 1, nchar(conflict_l$variable)-4)
conflict_w = dcast(conflict_l, conflict+iso3+year+scenario~variable)
conflict_w = conflict_w[,c(
  "conflict",
  "scenario",
  "GDPcap",
  "Pop",
  "YMHEP",
  "iso3",
  "year"
)]
conflict_w$conflict[which(conflict_w$conflict==2)] = 1
conflict_w$gdp = conflict_w$GDPcap * conflict_w$Pop * 1e3
conflict_w_agg = data.table(conflict_w)[,.(gdp=sum(gdp, na.rm=T)), by=.(scenario,year)]
conflict_w_agg = conflict_w_agg[order(conflict_w_agg$year),]
conflict_w_agg$scenario = toupper(conflict_w_agg$scenario)
conflict_w_agg$scenario[which(
  conflict_w_agg$year < 2014 & conflict_w_agg$scenario == "SSP1"
)] = "Historical"
conflict_w_agg = subset(conflict_w_agg,
  year >= 2014 | scenario=="Historical"
)
conflict_w_agg$gdp = conflict_w_agg$gdp / 1e12
p3 = ggplot(conflict_w_agg,aes(x=year,y=gdp,group=scenario,color=scenario)) +
  geom_line(linewidth=1) +
  scale_color_manual(values=c(
    reds[1],
    yellows[1],
    greens[1],
    blues[1],
    purples[1]
  )) + # Choose colour here
  scale_y_continuous(labels=dollar) + # Force y-grid to start at x-axis
  expand_limits(y=c(0, max(conflict_w_agg$gdp*1.1))) + # Start at 0 if wanted, add 10% padding to max
  # scale_x_continuous(breaks=c(2000, 2001, 2002)) + # Set manually to avoid 2000.0
  di_style +
  labs(
    y="Global GDP (US$ trillions)",
    x="",
    color=""
  )
p3
ggsave("outputs/p3_gdp.png", plot = p3, width = 12, height = 8)
fwrite(conflict_w_agg, "outputs/p3_gdp.csv")

conflict_w_agg2 = data.table(conflict_w)[,.(conflict=sum(conflict, na.rm=T)), by=.(scenario,year)]
conflict_w_agg2 = subset(conflict_w_agg2, scenario=="ssp1" & year < 2014)

p4 = ggplot(conflict_w_agg2,aes(x=year,y=conflict)) +
  geom_line(linewidth=1, color=reds[1]) +
  scale_y_continuous(expand = c(0, 0)) + # Force y-grid to start at x-axis
  expand_limits(y=c(0, max(conflict_w_agg2$conflict*1.1))) +
  scale_x_continuous(n.breaks=7) +
  di_style +
  labs(
    y="Count of global conflicts",
    x="",
    color=""
  )
p4
ggsave("outputs/p4_conflict.png", plot = p4, width = 12, height = 8)
fwrite(conflict_w_agg2, "outputs/p4_conflict.csv")

wd_base = "~/git/"
setwd(paste0(wd_base, "saint"))

forecast = fread("outputs/binary_conflict_clim_bigram_forecast.csv")
forecast$y_prob = forecast$y_hat
forecast$y_hat = forecast$y_prob

forecast$conflict[which(forecast$year>=2014)] = forecast$y_hat[which(forecast$year>=2014)]
forecast$scenario = toupper(forecast$scenario)
forecast$scenario[which(forecast$year < 2014 & forecast$scenario=="SSP1")] = "Historical"
forecast = subset(forecast, year > 2013 | scenario=="Historical")
forecast_agg = forecast[,.(
  conflicts=sum(conflict, na.rm=T)
), by=.(scenario, year)]

p5 = ggplot(forecast_agg,aes(x=year,y=conflicts,group=scenario,color=scenario)) +
  geom_line(linewidth=1) +
  scale_color_manual(values=c(
    reds[1],
    yellows[1],
    greens[1],
    blues[1],
    purples[1]
  )) + # Choose colour here
  scale_y_continuous(expand = c(0, 0)) + # Force y-grid to start at x-axis
  scale_x_continuous(n.breaks=7) +
  expand_limits(y=c(0, max(forecast_agg$conflicts*1.1))) + # Start at 0 if wanted, add 10% padding to max
  di_style +
  labs(
    y="Aggregate global conflict probabilities",
    x="",
    color=""
  )
p5
ggsave("~/git/humanitarian-ssp-projections/outputs/p5_conflict_forecast.png", plot = p5, width = 12, height = 8)
fwrite(forecast_agg, "~/git/humanitarian-ssp-projections/outputs/p5_conflict_forecast.csv")

forecast = fread("outputs/regression_displacement_worldclim_forecast.csv")
forecast$displaced_persons[which(forecast$year>=1960)] = forecast$y_hat[which(forecast$year>=1960)]
forecast$displaced_persons[which(forecast$displaced_persons<0)] = 0
forecast$displaced_persons = forecast$displaced_persons / 1e6
forecast$scenario = toupper(forecast$scenario)
forecast$scenario[which(forecast$year < 2023 & forecast$scenario=="SSP1")] = "Historical"
forecast = subset(forecast, year > 2022 | scenario=="Historical")
forecast_agg = forecast[,.(
  displaced_persons=sum(displaced_persons, na.rm=T)
), by=.(scenario, year)]

p6 = ggplot(forecast_agg,aes(x=year,y=displaced_persons,group=scenario,color=scenario)) +
  geom_line(linewidth=1) +
  scale_color_manual(values=c(
    reds[1],
    yellows[1],
    greens[1],
    blues[1],
    purples[1]
  )) + # Choose colour here
  scale_y_continuous(expand = c(0, 0)) + # Force y-grid to start at x-axis
  scale_x_continuous(n.breaks=7) +
  expand_limits(y=c(0, max(forecast_agg$displaced_persons*1.1))) + # Start at 0 if wanted, add 10% padding to max
  di_style +
  labs(
    y="Displaced persons (millions)",
    x="",
    color=""
  )
p6
ggsave("~/git/humanitarian-ssp-projections/outputs/p6_displacement_forecast.png", plot = p6, width = 12, height = 8)
fwrite(forecast_agg, "~/git/humanitarian-ssp-projections/outputs/p6_displacement_forecast.csv")


forecast = fread("outputs/regression_displacement_worldclim2_forecast.csv")
forecast$displaced_persons[which(forecast$year>=2022)] = forecast$y_hat[which(forecast$year>=2022)]
forecast$displaced_persons[which(forecast$displaced_persons<0)] = 0
forecast$displaced_persons = forecast$displaced_persons / 1e6
forecast$scenario = toupper(forecast$scenario)
forecast$scenario[which(forecast$year < 2022 & forecast$scenario=="SSP1")] = "Historical"
forecast = subset(forecast, year >= 2022 | scenario=="Historical")
forecast_agg = forecast[,.(
  displaced_persons=sum(displaced_persons, na.rm=T)
), by=.(scenario, year)]
max.hist = max(subset(forecast_agg, scenario=="Historical" & year==2021)$displaced_persons, na.rm=T)
min.forecast = min(subset(forecast_agg, year==2022)$displaced_persons, na.rm=T)
gap = max.hist - min.forecast
forecast_agg$displaced_persons[which(forecast_agg$scenario!="Historical")] =
  forecast_agg$displaced_persons[which(forecast_agg$scenario!="Historical")] +
  gap
p6b = ggplot(forecast_agg,aes(x=year,y=displaced_persons,group=scenario,color=scenario)) +
  geom_line(linewidth=1) +
  scale_color_manual(values=c(
    "#000000",
    reds[2:5]
  )) + # Choose colour here
  scale_y_continuous(expand = c(0, 0)) + # Force y-grid to start at x-axis
  scale_x_continuous(n.breaks=7) +
  expand_limits(y=c(0, max(forecast_agg$displaced_persons*1.1))) + # Start at 0 if wanted, add 10% padding to max
  di_style +
  labs(
    y="Millions of displaced persons",
    x="",
    color=""
  )
p6b
ggsave("~/git/humanitarian-ssp-projections/outputs/p6b_displacement_forecast.png", plot = p6b, width = 5.2, height = 3.2)
fwrite(forecast_agg, "~/git/humanitarian-ssp-projections/outputs/p6b_displacement_forecast.csv")



forecast = fread("outputs/regression_climate_worldclim_forecast.csv")
forecast$climate_disasters[which(forecast$year>=2023)] = forecast$y_hat[which(forecast$year>=2023)]
forecast$climate_disasters[which(forecast$climate_disasters<0)] = 0
forecast$scenario = toupper(forecast$scenario)
forecast$scenario[which(forecast$year < 2023 & forecast$scenario=="SSP1")] = "Historical"
forecast = subset(forecast, year > 2022 | scenario=="Historical")
forecast_agg = forecast[,.(
  climate_disasters=sum(climate_disasters, na.rm=T)
), by=.(scenario, year)]

p7 = ggplot(forecast_agg,aes(x=year,y=climate_disasters,group=scenario,color=scenario)) +
  geom_line(linewidth=1) +
  scale_color_manual(values=c(
    reds[1],
    yellows[1],
    greens[1],
    blues[1],
    purples[1]
  )) + # Choose colour here
  scale_y_continuous(expand = c(0, 0)) + # Force y-grid to start at x-axis
  scale_x_continuous(n.breaks=7) +
  expand_limits(y=c(0, max(forecast_agg$climate_disasters*1.1))) + # Start at 0 if wanted, add 10% padding to max
  di_style +
  labs(
    y="Total global climate disasters",
    x="",
    color=""
  )
p7
ggsave("~/git/humanitarian-ssp-projections/outputs/p7_climate_forecast.png", plot = p7, width = 12, height = 8)
fwrite(forecast_agg, "~/git/humanitarian-ssp-projections/outputs/p7_climate_forecast.csv")


ols_data = read.csv("~/git/saint/data/tripartite_bigram.csv")
ols_data$displaced_persons = as.numeric(ols_data$displaced_persons)
ols_data$conflict = as.numeric(ols_data$conflict)
ols = lm(humanitarian_needs~
           displaced_persons+
           tmax+
           prec+
           conflict
         , data=ols_data
)
summary(ols)
ols_intercept = summary(ols)$coefficients[[1]]
ols_displaced = summary(ols)$coefficients[[2]]
ols_tmax = summary(ols)$coefficients[[3]]
ols_prec = summary(ols)$coefficients[[4]]
ols_conflict = summary(ols)$coefficients[[5]]

forecast = fread("~/git/saint/data/tripartite_bigram_forecasting.csv")
forecast$intercept = ols_intercept
forecast$displacement_beta = forecast$displaced_persons * ols_displaced
forecast$tmax_beta = forecast$tmax * ols_tmax
forecast$prec_beta = forecast$prec * ols_prec
forecast$conflict_beta = forecast$conflict * ols_conflict
forecast$historical_humanitarian_needs = forecast$humanitarian_needs
forecast$humanitarian_needs = predict.lm(ols, newdata=forecast)
forecast$scenario = toupper(forecast$scenario)
forecast_sub = subset(forecast, year %in% c(2020, 2050, 2100))
forecast_agg = data.table(forecast_sub)[,.(
  humanitarian_needs=sum(humanitarian_needs, na.rm=T),
  historical_humanitarian_needs=sum(historical_humanitarian_needs, na.rm=T),
  tmax=sum(tmax, na.rm=T),
  prec=sum(prec, na.rm=T),
  displaced_persons=sum(displaced_persons, na.rm=T),
  conflict=sum(conflict, na.rm=T)
), by=.(scenario, year)]
forecast_agg$year = factor(forecast_agg$year)
forecast_agg$humanitarian_needs = forecast_agg$humanitarian_needs / 1e9
forecast_agg$historical_humanitarian_needs = forecast_agg$historical_humanitarian_needs / 1e9
pin_baseline = 0
forecast_agg$humanitarian_needs_label = forecast_agg$humanitarian_needs - pin_baseline
p8 = ggplot(forecast_agg, aes(x=scenario,y=humanitarian_needs_label,fill=year,group=year)) +
  scale_y_continuous(expand = c(0, 0), labels = function(y) y + pin_baseline) +
  scale_fill_manual(values = reds) +
  geom_bar(stat="identity", position="dodge") +
  di_style +
  labs(x="", fill="", y="Humanitarian spend (USD$ billions)")
p8
ggsave("~/git/humanitarian-ssp-projections/outputs/p8_needs_bars.png", plot = p8, width = 12, height = 8)
fwrite(forecast_agg, "~/git/humanitarian-ssp-projections/outputs/p8_needs_bars.csv")


forecast_agg = data.table(forecast)[,.(
  historical_humanitarian_needs=sum(historical_humanitarian_needs, na.rm=T),
  humanitarian_needs=sum(humanitarian_needs, na.rm=T),
  intercept=sum(intercept, na.rm=T),
  displaced_persons=sum(displaced_persons, na.rm=T),
  displacement_beta=sum(displacement_beta, na.rm=T),
  tmax=sum(tmax, na.rm=T),
  prec=sum(prec, na.rm=T),
  tmax_beta=sum(tmax_beta, na.rm=T),
  prec_beta=sum(prec_beta),
  conflict=sum(conflict, na.rm=T),
  conflict_beta=sum(conflict_beta, na.rm=T)
), by=.(scenario, year)]
forecast_agg$humanitarian_needs = forecast_agg$humanitarian_needs / 1e9
forecast_agg_baseline = forecast_agg$humanitarian_needs[which.min(forecast_agg$humanitarian_needs)]
forecast_agg$humanitarian_needs_label = forecast_agg$humanitarian_needs / forecast_agg_baseline

p9 = ggplot(forecast_agg,aes(x=year,y=humanitarian_needs_label,group=scenario,color=scenario)) +
  geom_line(linewidth=1) +
  scale_color_manual(values=c(
    reds[1],
    yellows[1],
    greens[1],
    blues[1],
    purples[1]
  )) + # Choose colour here
  scale_y_continuous(labels=percent) + # Force y-grid to start at x-axis
  scale_x_continuous(n.breaks=7) +
  di_style +
  labs(
    y="Humanitarian spend (% of baseline)",
    x="",
    color=""
  )
p9
ggsave("~/git/humanitarian-ssp-projections/outputs/p9_needs_lines.png", plot = p9, width = 12, height = 8)
fwrite(forecast_agg, "~/git/humanitarian-ssp-projections/outputs/p9_needs_lines.csv")

forecast_agg_l = melt(forecast_agg,
                      id.vars=c("scenario", "year"),
                      measure.vars = c("intercept","displacement_beta","tmax_beta","prec_beta","conflict_beta"))
forecast_agg_l$value = forecast_agg_l$value / 1e9

p10 = ggplot(subset(forecast_agg_l, scenario=="SSP5"),aes(x=year,y=value,group=variable,fill=variable)) +
  geom_area() +
  scale_y_continuous(expand = c(0, 0)) +
  # scale_fill_manual(values=reds) +
  scale_x_continuous(n.breaks=7) +
  di_style +
  labs(
    y="Contribution to SSP5 humanitarian spend (USD $ billions)",
    x="",
    fill=""
  )
p10
ggsave("~/git/humanitarian-ssp-projections/outputs/p10_beta.png", plot = p10, width = 12, height = 8)
fwrite(forecast_agg_l, "~/git/humanitarian-ssp-projections/outputs/p10_beta.csv")

load("~/git/humanitarian-ssp-projections/intermediate_data/iiasa.RData")
iiasa$Region = as.character(iiasa$Region)
iiasa = subset(iiasa, Region == "World")
iiasa_agg = data.table(iiasa)[,.(pop=sum(pop, na.rm=T)),by=.(Scenario,year)]
iiasa_agg$Scenario = as.character(iiasa_agg$Scenario)
iiasa_agg$Scenario[which(iiasa_agg$year <= 2010 & iiasa_agg$Scenario == "SSP1")] = "Historical"
iiasa_agg = subset(iiasa_agg, year > 2010 | Scenario == "Historical")
p11 = ggplot(iiasa_agg,aes(x=year,y=pop,group=Scenario,color=Scenario)) +
  geom_line(linewidth=1) +
  scale_color_manual(values=c(
    reds
  )) + # Choose colour here
  scale_y_continuous(
    expand = c(0, 0),
    n.breaks=7,
    labels = label_comma()
    ) + # Force y-grid to start at x-axis
  scale_x_continuous(n.breaks=7) +
  expand_limits(y=c(0, max(iiasa_agg$pop*1.1))) + # Start at 0 if wanted, add 10% padding to max
  di_style +
  labs(
    y="World population (millions)",
    x="",
    color=""
  )
p11
ggsave("~/git/humanitarian-ssp-projections/outputs/p11_pop.png", plot = p11, width = 12, height = 8)
fwrite(iiasa_agg, "~/git/humanitarian-ssp-projections/outputs/p11_pop.csv")

