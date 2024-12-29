library(tictoc)
library(HeadR)
library(latex2exp)
library(parallel)
#rm(list=ls()) # This is done in master_JIE.R
source("R_JIE_rev/AALA_calibration_functions.R") # 
source("R_JIE_rev/welfare_functions.R") # 
cat0 <- function(...) cat(...,sep="") 
#
#Those parameters do not change 
MEX_con_lib <- "con" # either con or lib assumption on Mexican share
alpha.base <- 0.15
theta.base <- 4
eta <- 4
Nfactor <- 100
#Kappa should take 3 values : 20 (base), 10 and 30
kappa_AVE <- if (!exists("kappa_AVE")) 20 else kappa_AVE
#
#Create directories if no existe
plotdir <- paste0("Plots_JIE_rev/AALA_calib/kappa",kappa_AVE)
if(!file.exists(plotdir)) {
  dir.create(plotdir)
}
rdsdir <- paste0("Data/RDS_JIE_rev/kappa",kappa_AVE)
if(!file.exists(rdsdir)) {
  dir.create(rdsdir)
}
if (file.exists("Data/RDS_JIE_rev/Choice0")) {
  print("Directory exists")
} else {
  dir.create("Data/RDS_JIE_rev/Choice0",recursive = FALSE, showWarnings = TRUE)
  print("Directory did not exist so  created it")
}
#
#Load parameters needed in simulations
zeta0 <- readRDS("Data/RDS_JIE_rev/zeta0.rds") # created in XL_Laffer "NAFTA by Country" code section
LA_hat <- readRDS("Data/RDS_JIE_rev/LA_hat.rds") # created in XL_Laffer "NAFTA by Country" code section
LA_hat[,LA_data := fcase(V_iso_o=="USA",81727,
                         V_iso_o=="MEX",62757,
                         V_iso_o=="CAN",37135)]
LA_grid <- CJ(V_iso_o= c("CAN","MEX","USA"),V_iso_o_prime=c("CAN","MEX","USA"))
LA_grid <- merge(LA_grid,LA_hat,by="V_iso_o")
LA_grid <- merge(LA_grid,LA_hat,by.x="V_iso_o_prime",by.y="V_iso_o")
LA_grid[, LA_rat0 := (LA_data.x/LA_data.y)*(LA_hat.D_i.y/LA_hat.D_i.x)]
LA_grid <- LA_grid[,.(ell=V_iso_o, ell_prime=V_iso_o_prime, LA_rat0)]
#LA_grid is merged in the aggregation in the C.hat.Agg.base.prime_DRF in welfare_functions.R
#
#Shares of sales for NAFTA as a destination (see Nafta_import_share.R 2018 or 2019)
share.F <- 0.25 
share.USA <- 0.50
share.MEX <- 0.15
share.CAN <- 0.10
#
calib.years <- 2011:2019
#
#Set up the stage: get the data + Num.obs
DR = readRDS("Data/RDS_JIE_rev/AALA_rev.rds")[year %in% calib.years]  # data prepared by AALA_clean.R
CAMUS = c("CA","MX","US")
# apply the selected liberal or conservative setting for nafta_shr
if(MEX_con_lib == "con") DR[,nafta_shr := nafta_shr_con] else DR[,nafta_shr := nafta_shr_lib]
Num.obs = DR[!is.na(nafta_shr)& ell %in% CAMUS,.N]  #1614 (when restricting years)
# load and adjust parameters
RCR.prime.base <- 0.75 
params <- readRDS(paste0("Data/RDS_JIE_rev/Params4/params_opt_",MEX_con_lib,"_alpha",alpha.base*100,"_theta",theta.base,".rds")) # restoring optimized parameters 
params <- c(RCR.prime=RCR.prime.base,params) # C.hat function needs RCR.prime.
params$RCR.prime <- 0
# load tau data and adjust parameters
DC <- readRDS("Data/RDS_JIE_rev/tau_index_DRF.rds") # has all 3 trade cost penalties
DC <- DC[ !is.na(tauD) & !is.na(tauR) & !is.na(tauF)] # need all 3 of them
#
# Loop over HS
for(chosen.HS in 8703:8704) {
params$taumat <- DC[HS_head == chosen.HS,.(V_iso_o,tauD,tauR,tauF,tauD_Q,tauR_Q,tauF_Q,chosen_R)]
#
# choose base parameters for relocation
params$omegaR <- 1
params$omegaF <- 1
params$kappa <- 1+kappa_AVE/100 # 
#
# Create price data for later table output with two price changes values
P.hat_625 <- C.hat.Agg.base.prime_DRF(RCR.base=0,RCR.prime = 0.625)
P.hat_75 <- C.hat.Agg.base.prime_DRF(RCR.base=0,RCR.prime = 0.75)
P.hat_85 <- C.hat.Agg.base.prime_DRF(RCR.base=0,RCR.prime = 0.85)
#
# Loop over whole range of RCR values to create main dataset (out)
RCR_range <- seq(0.0,1.0,by=0.001)
tic()
out <- mclapply(RCR_range,function(x) C.hat.Agg.base.prime_DRF(RCR.base=params$RCR.prime,RCR.prime = x),mc.cores=detectCores()-1) |> rbindlist()
toc()
#output, for the no RoO case, the number of carlines making each compliance choice, including the NCFs that will be dropped later.
choice0 <- sim_choice_DRF(RCR = 0,theta=params$theta,mu=params$mu,sigma=params$sigma,conc.err=params$conc.err,
               alpha=params$alpha,conc.alpha=params$conc.alpha,tau_data=params$taumat,N=Num.obs*Nfactor,
               omegaR=params$omegaR,omegaF=params$omegaF,kappa=params$kappa)
choice0RDS <- paste0("Data/RDS_JIE_rev/Choice0/roo_choice0_HS",chosen.HS,"_DRF.rds")
saveRDS(choice0,choice0RDS)
#
# Create graphs and RDSs, for 3 countries
for(chosen.country in c("CAN","MEX","USA")) {
#
XL <- data.table(RCR=100*RCR_range,out[V_iso_o == chosen.country,
                                       .(X=X_hat.D_i,L=L_hat.D_i,LA=LA_hat.D_i,LT =LT_hat.D_i)])
#
# save results for Table:
lafferRDS <- paste0("Data/RDS_JIE_rev/kappa",kappa_AVE,"/XLT_laffer_",chosen.country,"_HS",chosen.HS,"_DRF.rds")
saveRDS(XL,lafferRDS)
#
#choices underlying the Laffer curve ====
choices_smpl <- mclapply(RCR_range,sim_choice_DRF,theta=params$theta,mu=params$mu,sigma=params$sigma,conc.err=params$conc.err,
                         alpha=params$alpha,conc.alpha=params$conc.alpha,tau_data=params$taumat,N=Num.obs*Nfactor,
                         omegaR=params$omegaR,omegaF=params$omegaF,kappa=params$kappa,mc.cores = detectCores()-1)
choices_smpl <- rbindlist(choices_smpl)
choices_smpl <- choices_smpl[V_iso_o==chosen.country]
# count of NCF
NCF0 <- choices_smpl[choice=="NCF" & RCR==0]$N
choices_smpl[choice=="NCF", N := N-NCF0]
choices_smpl[, frac := N/sum(N), by = RCR]
choices_smpl[,`:=` (frac_USA = N_USA/N,frac_CAN = N_CAN/N,frac_MEX = N_MEX/N)]
choices_wide <- dcast(choices_smpl, RCR~choice, value.var = c("frac","frac_USA","frac_CAN","frac_MEX"))
# the following three "drop" commands generate warnings, which are not a concern
choices_wide[, c("frac_USA_CDU","frac_USA_CDC","frac_USA_NCD","frac_USA_NCF"):=NULL]
choices_wide[, c("frac_CAN_CDU","frac_CAN_CDC","frac_CAN_NCD","frac_CAN_NCF"):=NULL]
choices_wide[, c("frac_MEX_CDU","frac_MEX_CDC","frac_MEX_NCD","frac_MEX_NCF"):=NULL]
# apply a function to replace NA with 0 to all columns
NAto0 <- function(x) fifelse(is.na(x),0,x)
choices_wide <- choices_wide[,lapply(.SD,NAto0)]
#
# Save results for Table:
choicesRDS <- paste0("Data/RDS_JIE_rev/kappa",kappa_AVE,"/roo_choice_",chosen.country,"_HS",chosen.HS,"_DRF.rds")
saveRDS(choices_wide,choicesRDS)
#
# Price index changes for the CDC carlines at RCR = 75%, 85% (relative to 62.5%)
P_625 <- P.hat_625[V_iso_o == chosen.country,.(RCR=100*RCR.prime,P_CDC=P_hat.D_i_CDC,P=P_hat.D_i)] #new
P_75 <- P.hat_75[V_iso_o == chosen.country,.(RCR=100*RCR.prime,P_CDC=P_hat.D_i_CDC,P=P_hat.D_i)]
P_85 <- P.hat_85[V_iso_o == chosen.country,.(RCR=100*RCR.prime,P_CDC=P_hat.D_i_CDC,P=P_hat.D_i)]
P.hat <- rbind(P_625,P_75,P_85)
priceRDS <- paste0("Data/RDS_JIE_rev/kappa",kappa_AVE,"/price_change_",
                   chosen.country,"_HS",chosen.HS,"_DRF.rds") #comply constrained and all
saveRDS(P.hat,priceRDS)
#
cat("Completed HS ",chosen.HS,"for country ",chosen.country,"\n")
}
#NA aggregate
RCR_range <- seq(0.0,1.0,by=0.001)
tic()
out <- mclapply(RCR_range,function(x) C.hat.AggNA.base.prime_DRF(RCR.base=params$RCR.prime,RCR.prime = x),mc.cores=detectCores()-1) |> rbindlist()
toc()
NAXL <- data.table(RCR=100*RCR_range,out[,.(X=X_hat.D_i,L=L_hat.D_i,LA=LA_hat.D_i,LT =LT_hat.D_i)])
#
# save results for  LT_laffer.R
NAlafferRDS <- paste0("Data/RDS_JIE_rev/kappa",kappa_AVE,"/XLT_laffer_HS",chosen.HS,"_DRF.rds")
saveRDS(NAXL,NAlafferRDS)
}



