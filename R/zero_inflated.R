list.of.packages <- c("data.table","pscl","boot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

# Modify this for your local computer
wd_base = "~/git/"
setwd(paste0(wd_base, "humanitarian-ssp-projections"))

load("uppsala_replication/PredictionSSP_1.RData")

# Write to intermediate data for Python experiments
PredictionSSP_1 = transform(PredictionSSP_1,
                            lGDPcap_c1 = lGDPcap*c1,
                            lGDPcap_c2 = lGDPcap*c2,
                            lGDPcap_ltsc0 = lGDPcap*ltsc0
)
keep = c(
  "conflict",
  "temp",
  "nb_conflict",
  "YMHEP",
  "lpop",
  "lGDPcap",
  "ltsc0",
  "nc",
  "ncc1",
  "ncc2",
  "ltsnc",
  "ncts0",
  "lpop",
  "lGDPcap_c1",
  "lGDPcap_c2",
  "lGDPcap_ltsc0",
  "ltimeindep"
)
analysis_set = PredictionSSP_1[,keep]
analysis_set = analysis_set[complete.cases(analysis_set),]

m1 <- zeroinfl(conflict ~
                 temp+
                 nb_conflict+
                 YMHEP+
                 lpop+
                 lGDPcap+
                 ltsc0+
                 nc+
                 ncc1+
                 ncc2+
                 ltsnc+
                 ncts0+
                 lpop+
                 lGDPcap_c1+
                 lGDPcap_c2+
                 lGDPcap_ltsc0+
                 ltimeindep
, data = analysis_set)
summary(m1)
mnull <- update(m1, . ~ 1)
pchisq(2 * (logLik(m1) - logLik(mnull)), df = 3, lower.tail = FALSE)
mcFadden = 1-logLik(m1)/logLik(mnull)
mcFadden
