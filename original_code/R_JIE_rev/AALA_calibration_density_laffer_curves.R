# This file is a "daughter" to AALA_calibration_brute_force.R 
# It is not standalone, and generates the density and laffer curves. 
# Generates calibrated density and several versions of the laffer curve: all / cars - 3 countries / trucks 3 countries.
#
# Load optimized parameters  for both curves ====
params <- readRDS(paste0("Data/RDS_JIE_rev/Params4/params_opt_",MEX_con_lib,"_alpha",alpha.base*100,"_theta",theta.base,".rds")) 
params$tau <- data.table(tau=DC$tauD,tauQ=1)
#
# Density curve of lambda (data and one run of model) ====
set.seed(140341)
#
sim_out <- sim_lambda_alpha(RCR=params$RCR,theta=params$theta,mu=params$mu,sigma=params$sigma,conc.err=params$conc.err,
                            mu.alpha=params$alpha,conc.alpha=params$conc.alpha,tau_data=params$tau,N=Num.obs*20)
hist(sim_out$lambda_R)  
summary(sim_out$lambda_model)
mean(sim_out$lambda_model %between% c(0,1)) # 100% between 0 and 1,
#
CC_pct = round(100*sim_out$CC_frac,1)
CU_pct = round(100*sim_out$CU_frac,1)
cat("Compliance fractions: ",CC_pct,"% constrained",CU_pct, "% unconstrained") #24%/62%
lambda_sim = 100*sim_out$lambda_model
lambda_sim_d = density(lambda_sim)
# density of simulated lambda
DS = data.table(x=lambda_sim_d$x,y=lambda_sim_d$y)
DS[,x_rnd := round(x)]
DS <- DS[x_rnd %between% c(0,100),.(den_sim = mean(y)),by=x_rnd]
DS <- merge(DD,DS,by="x_rnd") # 
fit = L2norm(DS$den_data,DS$den_sim)
ymax = 0.035
#
# Density fit for JIE  ====
pdf(paste0("Plots_JIE_rev/AALA_calib/AALA_calib_bf_",MEX_con_lib,"_alpha",alpha.base*100,"_theta",theta.base,".pdf"),5,5)
par(mar=c(4,4,1,1)+0.1 )
# empty plot
plot(c(0,100),c(0,0),col="black",lty="dotted",main ="",xlab="Nafta cost share (%)",
     ylab="Density",xlim=c(0,100),xaxs="i",ylim=c(0.0,ymax),type="n")
# data and simulation densities
lines(lambda_data_d,lwd=2,col="black")
lines(lambda_sim_d,lwd=2,col="blue")
#
legend("topright",lwd=c(2,2,2,3),col=c("black","blue"),legend=c("Data",TeX("Model (fitted)")),bty="n")
legend("topleft",legend=c(TeX(sprintf(r'($\theta = %.2f$)', params$theta)),
                          TeX(sprintf(r'($\mu = %.3f$)', params$mu)),
                          TeX(sprintf(r'($\sigma = %.3f$)', params$sigma)),
                          TeX(sprintf(r'($\bar{\alpha} = %.2f$)', params$alpha)),
                          TeX(sprintf(r'($\nu_\alpha = %.2f$)', params$conc.alpha)),
                          TeX(sprintf(r'($\nu_\lambda = %.2f$)', params$conc.err)),
                          TeX(sprintf(r'(CC = %.1f)', CC_pct)),
                          TeX(sprintf(r'(CU = %.1f)', CU_pct)),
                          TeX(sprintf(r'($L_2$norm = %.3f)',fit))),
       bty="n")
dev.off()
#
# Laffer curve out of calibrated version ====
# (uses last version of params, which has calibrated values)
L <- 1000
roos <- seq(0.001,0.999,length.out = L)
# sim_lambda_bar takes some time to do the L=1000 repetitions
RCS_smpl <- sapply(roos,sim_avg_RCS_alpha,theta=params$theta,mu=params$mu,sigma=params$sigma,conc.err=params$conc.err,
                   alpha=params$alpha,conc.alpha=params$conc.alpha,tau_data=params$tau,N=Num.obs*20)
#
roo.laffer.range <- range(RCS_smpl)
#
# calculations for JIE text
sink(file=paste0("Tables_JIE_rev/roo_lafferstats.txt")) # 
cat("RCS  with no ROO: ",data.table(RCR=roos,RCS = RCS_smpl)[order(RCR)]$RCS[1],"%\n")
cat("first RCR for which content higher than with no ROO: ",data.table(RCR=roos,RCS = RCS_smpl)[order(RCR)][round(RCS,2)>round(RCS[1],2)]$RCR[1]*100,"%\n")
DT <- data.table(RCR=roos,RCS = RCS_smpl)[round(RCR*100,1)==62.5 |round(RCR*100,1)==75.0]
cat("RCS under NAFTA: ",DT$RCS[1],"%","\n")
cat("RCS under USMCA: ",DT$RCS[2],"%","\n")
cat("change in RCS from USMCA: ",100*(DT$RCS[2]/DT$RCS[1]-1),"%","\n")
DT <- data.table(RCR=roos,RCS = RCS_smpl)[order(-RCS)][1]
cat("Laffer peaks at RCR = ",100*DT$RCR[1],"but RCS is just ",DT$RCS[1])
sink()
#
pdf(paste0("Plots_JIE_rev/AALA_calib/roo_laffer_calib_bf_",MEX_con_lib,"_alpha",alpha.base*100,"_theta",theta.base,".pdf"),width=5,height=5)
par(mar=c(4,5,1,0)+0.1 )
plot(100*roos,RCS_smpl,type="n",col="black",lwd=2,ylim=c(roo.laffer.range[1],roo.laffer.range[2]+0.25),
     ylab=TeX("Average regional content share (%)"),
     xlab=TeX("Regional content requirement (%)"))
lines(100*roos,RCS_smpl,col="black",lwd=2)
#lines(lowess(roos,RCS_smpl,f=0.1),col="blue",lwd=2)
abline(h=min(RCS_smpl),lty="dotted",col="black")
abline(v=62.5,lty="dashed")
abline(v=75,lty="dashed")
text(c(62.5,75),max(RCS_smpl)+0.25,c("NAFTA","USMCA"),pos=c(2,4))
dev.off()

#
# New figures for revision
# Laffer curves: cars vs trucks in separate figures====
# 3 countries for each square figure: mex green can red us blue
# (replace tau in params)
#
# save base parameters:
params_all <- copy(params)
#
# Cars ==== 
L <- 1000
roos <- seq(0.001,0.999,length.out = L)
# sim_lambda_bar takes some time to do the L=1000 repetitions
#
#USA:
params <- copy(params_all)
params$tau <-  data.table(tau=DC[HS_head==8703 & V_iso_o=="USA"]$tauD,tauQ=1)
RCS_USA <- sapply(roos,sim_avg_RCS_alpha,theta=params$theta,mu=params$mu,sigma=params$sigma,conc.err=params$conc.err,
                  alpha=params$alpha,conc.alpha=params$conc.alpha,tau_data=params$tau,N=Num.obs*20)
#CAN:
params <- copy(params_all)
params$tau <-  data.table(tau=DC[HS_head==8703 & V_iso_o=="CAN"]$tauD,tauQ=1)
RCS_CAN <- sapply(roos,sim_avg_RCS_alpha,theta=params$theta,mu=params$mu,sigma=params$sigma,conc.err=params$conc.err,
                  alpha=params$alpha,conc.alpha=params$conc.alpha,tau_data=params$tau,N=Num.obs*20)
#
#MEX:
params <- copy(params_all)
params$tau <-  data.table(tau=DC[HS_head==8703 & V_iso_o=="MEX"]$tauD,tauQ=1)
RCS_smpl <- sapply(roos,sim_avg_RCS_alpha,theta=params$theta,mu=params$mu,sigma=params$sigma,conc.err=params$conc.err,
                   alpha=params$alpha,conc.alpha=params$conc.alpha,tau_data=params$tau,N=Num.obs*20)
RCS_MEX <- copy(RCS_smpl)
#
roo.laffer.range <- range(RCS_MEX,RCS_CAN,RCS_USA) # MEX have much larger range
#
pdf(paste0("Plots_JIE_rev/AALA_calib/roo_laffer_cars_3ctry_calib_bf_",MEX_con_lib,"_alpha",alpha.base*100,"_theta",theta.base,".pdf"),width=5,height=5)
par(mar=c(4,5,1,0)+0.1 )
plot(100*roos,RCS_MEX,type="n",col="forestgreen",lwd=2,ylim=c(roo.laffer.range[1],roo.laffer.range[2]+0.25),
     ylab=TeX("Average regional content share (%)"),
     xlab=TeX("Regional content requirement (%)"))
lines(100*roos,RCS_MEX,col="forestgreen",lwd=2)
abline(v=62.5,lty="dashed")
abline(v=75,lty="dashed")
text(c(62.5,75),max(RCS_MEX,RCS_CAN,RCS_USA)+0.25,c("NAFTA","USMCA"),pos=c(2,4))
#
lines(100*roos,RCS_USA,col="blue",lwd=2)
lines(100*roos,RCS_CAN,col="red",lwd=2)
#
legend("topleft", col=c("forestgreen","blue","red"),lwd=2,
       legend=c("Mexico","USA","Canada"),horiz = FALSE,cex=0.8)
dev.off()


# Trucks ==== 
L <- 1000
roos <- seq(0.001,0.999,length.out = L)
# sim_lambda_bar takes some time to do the L=1000 repetitions
#
#USA:
params <- copy(params_all)
params$tau <-  data.table(tau=DC[HS_head==8704 & V_iso_o=="USA"]$tauD,tauQ=1)
RCS_USA <- sapply(roos,sim_avg_RCS_alpha,theta=params$theta,mu=params$mu,sigma=params$sigma,conc.err=params$conc.err,
                  alpha=params$alpha,conc.alpha=params$conc.alpha,tau_data=params$tau,N=Num.obs*20)
#CAN:
params <- copy(params_all)
params$tau <-  data.table(tau=DC[HS_head==8704 & V_iso_o=="CAN"]$tauD,tauQ=1)
RCS_CAN <- sapply(roos,sim_avg_RCS_alpha,theta=params$theta,mu=params$mu,sigma=params$sigma,conc.err=params$conc.err,
                  alpha=params$alpha,conc.alpha=params$conc.alpha,tau_data=params$tau,N=Num.obs*20)
#
#MEX:
params <- copy(params_all)
params$tau <-  data.table(tau=DC[HS_head==8704 & V_iso_o=="MEX"]$tauD,tauQ=1)
RCS_smpl <- sapply(roos,sim_avg_RCS_alpha,theta=params$theta,mu=params$mu,sigma=params$sigma,conc.err=params$conc.err,
                   alpha=params$alpha,conc.alpha=params$conc.alpha,tau_data=params$tau,N=Num.obs*20)
RCS_MEX <- copy(RCS_smpl)
#
roo.laffer.range <- range(RCS_MEX,RCS_CAN,RCS_USA) # MEX have much larger range
#
pdf(paste0("Plots_JIE_rev/AALA_calib/roo_laffer_trucks_3ctry_calib_bf_",MEX_con_lib,"_alpha",alpha.base*100,"_theta",theta.base,".pdf"),width=5,height=5)
par(mar=c(4,5,1,0)+0.1 )
plot(100*roos,RCS_MEX,type="n",col="forestgreen",lwd=2,ylim=c(roo.laffer.range[1],roo.laffer.range[2]+0.25),
     ylab=TeX("Average regional content share (%)"),
     xlab=TeX("Regional content requirement (%)"))
lines(100*roos,RCS_MEX,col="forestgreen",lwd=2)
abline(v=62.5,lty="dashed")
abline(v=75,lty="dashed")
text(c(62.5,75),max(RCS_MEX,RCS_CAN,RCS_USA)+0.25,c("NAFTA","USMCA"),pos=c(2,4))
#
lines(100*roos,RCS_USA,col="blue",lwd=2)
lines(100*roos,RCS_CAN,col="red",lwd=2)
#
legend("topleft", col=c("forestgreen","blue","red"),lwd=2,
       legend=c("Mexico","USA","Canada"),horiz = FALSE,cex=0.8)
dev.off()
