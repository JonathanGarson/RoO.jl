library(tictoc)
library(HeadR)
library(latex2exp)
library(parallel)
rm(list=ls())
source("R_JIE_rev/AALA_calibration_functions.R") # 
source("R_JIE_rev/welfare_functions.R") # 
cat0 <- function(...) cat(...,sep="") # like paste0, add to HeadR
#
MEX_con_lib <- "con" # either con or lib assumption on Mexican share
alpha.base <- 0.15
theta.base <- 4
eta <- 4
Nfactor <- 100
zeta <- readRDS("Data/RDS_JIE_rev/zeta.rds")$zeta[1] # data, see appendix for sources
share.F <- 0.25 # see Nafta_import_share.R 2018 
share.USA <- 0.50
share.MEX <- 0.15
share.CAN <- 0.10
#
calib.years <- 2011:2019
#
# set up the stage: get the data + Num.obs
DR = readRDS("Data/RDS_JIE_rev/AALA_rev.rds")[year %in% calib.years]  # data prepared by AALA_clean.R
CAMUS = c("CA","MX","US")
# apply the selected liberal or conservative setting for nafta_shr
if(MEX_con_lib == "con") DR[,nafta_shr := nafta_shr_con] else DR[,nafta_shr := nafta_shr_lib]
Num.obs = DR[!is.na(nafta_shr)& ell %in% CAMUS,.N]  #1614 (when restricting years)
# load and adjust parameters
RCR.prime.base <- 0.75 
# choose the directory for the settings
params <- readRDS(paste0("Data/RDS_JIE_rev/Params4/params_opt_",MEX_con_lib,"_alpha",alpha.base*100,"_theta",theta.base,".rds")) # restoring optimized parameters 
params <- c(RCR.prime=RCR.prime.base,params) # C.hat function needs RCR.prime.
params$RCR.prime <- 0
# reload  tau data, change parameters to have same setup as with DRF version (with endogenous assembly)
DC <- readRDS("Data/RDS_JIE_rev/tau_index_DRF.rds") # has all 3 trade cost penalties
params$tau <- data.table(tau=DC$tauD,tauQ=DC$tauD_Q)
#
#
# COMBINED NAFTA ====
#
# Create zeta0, by running this code once for the Nafta value of the RCR
zetarun <- C.hat.Agg.base.prime(RCR.base=params$RCR.prime,RCR.prime = 0.625)
zeta0 #0.647
rm(zetarun)

# Loop over whole range
RCR_range <- seq(0.0,1.0,by=0.001)
tic()
out <- mclapply(RCR_range,function(x) C.hat.Agg.base.prime(RCR.base=params$RCR.prime,RCR.prime = x),mc.cores=detectCores()-1) |> rbindlist()
toc()
XL <- data.table(RCR=100*RCR_range,out[compliance == "All",.(X =X_hat.D_i,L=L_hat.D_i,LT =LT_hat.D_i)])
#
# Statistics to report when discussing XLT_laffer.pdf====
#when does the laffer become binding
cat0("first RCR for which content higher than with no ROO: ",XL[order(X)][X>0]$RCR[1],"%\n")
cat0("peak of Laffer curves for X, ",XL[order(-X)]$RCR[1],"% , L ",XL[order(-L)]$RCR[1],"% and LT ",XL[order(-LT)]$RCR[1],"%\n")
X_pctchg = 100*((1+XL[RCR==75]$X/100)/(1+XL[RCR==62.5]$X/100)-1)
cat0("USMCA (75%) increase in parts shares relative to Nafta (62.5%): ",round(X_pctchg,1),"%\n")
L_pctchg = 100*((1+XL[RCR==75]$L/100)/(1+XL[RCR==62.5]$L/100)-1)
cat0("USMCA (75%) increase in parts employment relative to Nafta (62.5%): ",round(L_pctchg,1),"%\n")
LT_pctchg = 100*((1+XL[RCR==75]$LT/100)/(1+XL[RCR==62.5]$LT/100)-1)
cat0("USMCA (75%) increase in parts+ assembly employment relative to Nafta (62.5%): ",round(LT_pctchg,1),"%\n")
#
# Figure of JIE initial submission
pdf("Plots_JIE_rev/AALA_calib/XLT_laffer.pdf",width=8,height=4.5)
par(mar=c(4,4,1,0.5)+0.1 )
XL[,plot(RCR,X,type = "l",lwd=2,col="black",
         xlab= "Regional content requirement (%)",
         ylab = TeX("Change  in regional content, employment (%)"),
         ylim=c(min(LT),max(X)),xaxt="n",las=2)]
xaxmarks =c(0,25,50,60,75,85,100)
axis(side = 1, at=xaxmarks,labels=xaxmarks)
XL[,lines(RCR,L,type = "l",lwd=2,col="orange")]
XL[,lines(RCR,LT,type = "l",lwd=2,col="maroon")]
abline(h=0,lty="dotted")
abline(v=75,lty="dashed")
abline(v=62.5,lty="dashed")
abline(v=85,lty="dashed")
text(95,3.5,TeX("$\\hat{X}^D$"),col="black")
text(95,1.5,TeX("$\\hat{L}^D$"),col="orange")
text(87.5,0.65,TeX("$\\hat{L}^D_T$"),col="maroon")
mtext("NAFTA",side=3,at=62.5)
mtext("USMCA",side=3,at=75)
mtext("prop.",side=3,at=85)
legend("topleft",legend=c(TeX("$\\hat{X}^D$: parts share"),
                          TeX("$\\hat{L}^D$: parts employment"),
                          TeX("$\\hat{L}^D_T$: total employment (incl. assembly)")),
       col = c("black","orange","maroon"),lty=c("solid","solid","solid"),
       bty="n")
dev.off()
#
# Now generate two RDS, one for cars, one for trucks, to be used later (combined with DRF ones) in the revision graph.
#
for(chosen.HS in 8703:8704) {
params$tau <- data.table(tau=DC[HS_head == chosen.HS]$tauD, tauQ=DC[HS_head == chosen.HS]$tauD_Q)
RCR_range <- seq(0.0,1.0,by=0.001)
tic()
out <- mclapply(RCR_range,function(x) C.hat.Agg.base.prime(RCR.base=params$RCR.prime,RCR.prime = x),mc.cores=detectCores()-1) |> rbindlist()
toc()
XL <- data.table(RCR=100*RCR_range,out[compliance == "All",.(X =X_hat.D_i,L=L_hat.D_i,LT =LT_hat.D_i)])
lafferRDS <- paste0("Data/RDS_JIE_rev/XLT_laffer_HS",chosen.HS,".rds")
saveRDS(XL,lafferRDS)
}
#
#  NAFTA BY COUNTRY ====
#
# Generate zeta0
params$tau <- data.table(V_iso_o=DC$V_iso_o, tau=DC$tauD,tauQ=DC$tauD_Q)
zetarun <- C.hat.Agg.base.prime_o(RCR.base=params$RCR.prime,RCR.prime = 0.625)
zeta0
saveRDS(zeta0,"Data/RDS_JIE_rev/zeta0.rds")
LA_hat <- zetarun[compliance == "All",.(V_iso_o,LA_hat.D_i)]
LA_hat
saveRDS(LA_hat,"Data/RDS_JIE_rev/LA_hat.rds") # This is used on DRF version
rm(zetarun)
#
# Now tau should be accompanied with its V_iso_o
for(chosen.HS in 8703:8704) {
params$tau <- data.table(V_iso_o=DC[HS_head == chosen.HS]$V_iso_o, tau=DC[HS_head == chosen.HS]$tauD, tauQ=DC[HS_head == chosen.HS]$tauD_Q)
# Create price data for later table output with two price changes values
P.hat_625 <- C.hat.Agg.base.prime_o(RCR.base=0,RCR.prime = 0.625)
P.hat_75 <- C.hat.Agg.base.prime_o(RCR.base=0,RCR.prime = 0.75)
P.hat_85 <- C.hat.Agg.base.prime_o(RCR.base=0,RCR.prime = 0.85)
#
# loop over whole range
RCR_range <- seq(0.0,1.0,by=0.001)
tic()
out <- mclapply(RCR_range,function(x) C.hat.Agg.base.prime_o(RCR.base=params$RCR.prime,RCR.prime = x),mc.cores=detectCores()-1) |> rbindlist()
toc()
#
cat("Done with HS ",chosen.HS,"\n")
#
for(chosen.country in c("CAN","MEX","USA")) {
#
# Choices 
choices_smpl <- out[V_iso_o==chosen.country & compliance_prime!="All"] 
choices_smpl <- choices_smpl[, share.tot := sum(share.obs) ,by=.(RCR.prime)] 
choices_smpl <- choices_smpl[, share.obs := 100*(share.obs/share.tot)] 
choices_smpl[,RCR := RCR.prime*100]
choices_wide <- dcast(choices_smpl, RCR~compliance_prime, value.var = "share.obs")
NAto0 <- function(x) fifelse(is.na(x),0,x)
choices_wide <- choices_wide[,lapply(.SD,NAto0)]
setnames(choices_wide,old = c("Comply-constrained","Comply-unconstrained", "Non-compliant" ),new =c("CC","CU","NC"))
# save results for Table:
choicesRDS <- paste0("Data/RDS_JIE_rev/roo_choice_",chosen.country,"_HS",chosen.HS,".rds")
saveRDS(choices_wide,choicesRDS)
#
# XLT figure 
XL <- out[compliance_prime=="All" & compliance=="All",.(RCR=100*RCR.prime,V_iso_o=V_iso_o,X =X_hat.D_i,L=L_hat.D_i,LT =LT_hat.D_i)]
XL <- XL[V_iso_o==chosen.country]
# save results for Table:
lafferRDS <- paste0("Data/RDS_JIE_rev/XLT_laffer_",chosen.country,"_HS",chosen.HS,".rds")
saveRDS(XL,lafferRDS)
#
#statistics to report when discussing XLT_laffer.pdf====
sink(file=paste0("Tables_JIE_rev/XLT_laffer_stats_",chosen.country,"_HS",chosen.HS,".txt")) # 
cat0("Statistics for: ",chosen.country,", and HS: ",chosen.HS,"\n")
cat0("first RCR for which content higher than with no ROO: ",XL[order(X)][X>0]$RCR[1],"%\n")
cat0("peak of Laffer curves for X, ",XL[order(-X)]$RCR[1],"% , L ",XL[order(-L)]$RCR[1],"% and LT ",XL[order(-LT)]$RCR[1],"%\n")
X_pctchg = 100*((1+XL[RCR==75]$X/100)/(1+XL[RCR==62.5]$X/100)-1)
cat0("USMCA (75%) increase in parts shares relative to Nafta (62.5%): ",round(X_pctchg,1),"%\n")
L_pctchg = 100*((1+XL[RCR==75]$L/100)/(1+XL[RCR==62.5]$L/100)-1)
cat0("USMCA (75%) increase in parts employment relative to Nafta (62.5%): ",round(L_pctchg,1),"%\n")
LT_pctchg = 100*((1+XL[RCR==75]$LT/100)/(1+XL[RCR==62.5]$LT/100)-1)
cat0("USMCA (75%) increase in parts+ assembly employment relative to Nafta (62.5%): ",round(LT_pctchg,1),"%\n")
LT2_pctchg = 100*((1+XL[RCR==85]$LT/100)/(1+XL[RCR==62.5]$LT/100)-1)
cat0("USMCA proposition (85%) increase in parts+ assembly employment relative to Nafta (62.5%): ",round(LT2_pctchg,1),"%\n")
sink()
#
pdf(paste0("Plots_JIE_rev/AALA_calib/XLT_laffer_",chosen.country,"_HS",chosen.HS,".pdf"),width=5,height=5)
par(mar=c(4,4,1,0.5)+0.1 )
XL[,plot(RCR,X,type = "l",lwd=2,col="black",
         xlab= "Regional content requirement (%)",
         ylab = TeX("Change  in regional content, employment (%)"),
         ylim=c(min(LT),max(X)),xaxt="n",las=2)]
xaxmarks =c(0,25,50,60,75,85,100)
axis(side = 1, at=xaxmarks,labels=xaxmarks)
XL[,lines(RCR,L,type = "l",lwd=2,col="orange")]
XL[,lines(RCR,LT,type = "l",lwd=2,col="maroon")]
abline(h=0,lty="dotted")
abline(v=75,lty="dashed")
abline(v=62.5,lty="dashed")
mtext("NAFTA",side=3,at=62.5,cex=0.8,adj=1)
mtext("USMCA",side=3,at=75,cex=0.8,adj=0)
legend("topleft",legend=c(TeX("$\\hat{X}^D$: parts share"),
                          TeX("$\\hat{L}^D$: parts employment"),
                          TeX("$\\hat{L}^D_T$: parts+assembly emp.")),
       col = c("black","orange","maroon"),lty=c("solid","solid","solid"),
       bty="n")
dev.off()
#
# price index changes for the CDC carlines at RCR = 75%, 85% (relative to 62.5%)
P_625 <- P.hat_625[V_iso_o == chosen.country & compliance_prime == "All",.(RCR=100*RCR.prime,P=P_hat.D_i)]
P_75 <- P.hat_75[V_iso_o == chosen.country & compliance_prime == "All",.(RCR=100*RCR.prime,P=P_hat.D_i)]
P_85 <- P.hat_85[V_iso_o == chosen.country & compliance_prime == "All",.(RCR=100*RCR.prime,P=P_hat.D_i)]
P.hat <- rbind(P_625,P_75,P_85)
P_625 <- P.hat_625[V_iso_o == chosen.country & compliance_prime == "Comply-constrained",.(RCR=100*RCR.prime,P_CDC=P_hat.D_i)]
P_75 <- P.hat_75[V_iso_o == chosen.country & compliance_prime == "Comply-constrained",.(RCR=100*RCR.prime,P_CDC=P_hat.D_i)]
P_85 <- P.hat_85[V_iso_o == chosen.country & compliance_prime == "Comply-constrained",.(RCR=100*RCR.prime,P_CDC=P_hat.D_i)]
P.hat_CDC <- rbind(P_625,P_75,P_85)
P.hat <- merge(P.hat,P.hat_CDC,by="RCR")
priceRDS <- paste0("Data/RDS_JIE_rev/price_change_",chosen.country,"_HS",chosen.HS,".rds")
saveRDS(P.hat,priceRDS)
}
}



