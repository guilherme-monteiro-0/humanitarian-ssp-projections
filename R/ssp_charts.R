list.of.packages <- c("data.table","reshape2", "ggplot2", "scales", "openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd_base = "~/git/"
setwd(paste0(wd_base, "humanitarian-ssp-projections"))

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
    ,plot.background = element_blank()
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

ggplot(worldclim, aes(x=year, y=tmax, group=scenario, fill=scenario)) +
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

prec_baseline = 1000
worldclim$prec = worldclim$prec - prec_baseline

ggplot(worldclim, aes(x=year, y=prec, group=scenario, fill=scenario)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=purples) +
  scale_y_continuous(expand = c(0, 0), labels = function(y) y + prec_baseline) + # Force y-grid to start at x-axis
  di_style +
  rotate_x_text_45 + # Or chose _90 or remove for horizontal
  labs(
    y="Mean annual precipitation per 10 minute square (mm)",
    x="",
    fill=""
  )


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
ggplot(conflict_w_agg,aes(x=year,y=gdp,group=scenario,color=scenario)) +
  geom_line(size=2) +
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
