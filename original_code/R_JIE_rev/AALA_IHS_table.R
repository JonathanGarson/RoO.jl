#
library(HeadR)
rm(list=ls())
#
calib.years <- 2011:2019
#
L2norm <- function(u,v) sqrt(sum((u-v)^2))
CAMUS = c("CA","MX","US")
MEX_con_lib <- "con" # either con or lib assumption on Mexican share
#
DR <- readRDS("Data/RDS_JIE_rev/AALA_rev.rds")[year %in% calib.years]  # data prepared by R_JIE_rev/AALA_clean.R
#
# Choose the conservative or liberal going forward, and compute data density, also load tau index
if(MEX_con_lib=="con")  DR[,nafta_shr := nafta_shr_con] else DR[,nafta_shr := nafta_shr_lib] 
DR[,summary(nafta_shr)]
DR[,typeA := fifelse(type %like% "Truck","Truck","Car/MPV")]
DR[,ellA := fcase(ell == "CA","Canada",ell=="US","USA",ell=="MX","Mexico")]
#
#unique carlines
iqr <- function(x){IQR(x)}
DRc <- dcast(DR[ell %in% CAMUS],ellA~typeA,value.var = list("carlines","nafta_shr","nafta_shr"),fun = list(uniqueN,median,iqr))
#
# Bring in tau 
DT <- readRDS("Data/RDS_JIE_rev/tau_index_DRF.rds")
sink(file="Tables_JIE_rev/tauD_count.txt")
DT[!is.na(tauD),.N]
sink()
setnames(DT,"tauD","tau_index")
#unique models
DTc <- dcast(DT,V_iso_o~HS_head,value.var = list("V_id","tau_index"),fun = list(uniqueN,median))

#B for both lambda and tau:
DB <- cbind(DRc,DTc)
#length = carline-years or model-years
DB[,out := texout(.(ellA,`carlines_uniqueN_Car/MPV`,`nafta_shr_median_Car/MPV`,
                    V_id_uniqueN_8703,tau_index_median_8703,carlines_uniqueN_Truck,nafta_shr_median_Truck,
                    V_id_uniqueN_8704,tau_index_median_8704))]
writeLines(DB$out,"Tables_JIE_rev/AALA_IHS_table.tex")
