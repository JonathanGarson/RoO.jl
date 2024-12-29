# Figure 8
library(HeadR)
library(latex2exp)
rm(list=ls())
# make a directory "Data/RDS_JIE_rev/Params4" for the parameters (if it does not already exist)
if (file.exists("Data/RDS_JIE_rev/Params4")) {
  print("Directory exists")
} else {
  dir.create("Data/RDS_JIE_rev/Params4",recursive = FALSE, showWarnings = TRUE)
  print("Directory did not exist so  created it")
}

source("R_JIE_rev/AALA_calibration_functions.R") # 
#
calib.years <- 2011:2019
L2norm <- function(u,v) sqrt(sum((u-v)^2))
CAMUS = c("CA","MX","US")
MEX_con_lib <- "con" # either con or lib assumption on Mexican share
dist.alpha <- "Beta" # 
alpha.base <- 0.15
theta.base <- 4
params = list(theta=theta.base,mu=0.1,sigma=0.1,RCR=0.625,tau=1.025,alpha.lo=0,alpha.hi=0,alpha.a =1e7,alpha.b=1e7)
RCR = 100*params$RCR
#
DR = readRDS("Data/RDS_JIE_rev/AALA_rev.rds")[year %in% calib.years]   # data prepared by AALA_clean.R
#
#
# Choose the conservative or liberal going forward, and compute data density
if(MEX_con_lib=="con")  DR[,nafta_shr := nafta_shr_con] else DR[,nafta_shr := nafta_shr_lib] 
DR[,summary(nafta_shr)]
lambda_data_d = DR[!is.na(nafta_shr) & ell %in% CAMUS, density(nafta_shr)]
Num.obs = DR[!is.na(nafta_shr)& ell %in% CAMUS,.N]  #1704
#
# Plot : model of density of lambda with only delta heterogeneity, including data density ====
# pedagogical: shows density hole and spike problems
# do it for alpha = 0, so that we have a minimal version 
#
# simulation
set.seed(140422)
params = list(theta=theta.base,mu=0,sigma=0.2,RCR=0.625,tau=1.1,alpha.lo=0,alpha.hi=0,alpha.a =1e7,alpha.b=1e7) # pseudo calibrated 
sim_out <- sim_lambda(RCR=params$RCR,theta=params$theta,mu=params$mu,sigma=params$sigma,
                   alpha.lo=params$alpha.lo,alpha.hi=params$alpha.hi,alpha.a=params$alpha.a,alpha.b=params$alpha.b,
                   tau_data=params$tau,N=Num.obs*20)
# densities from the simulated model
lambda_sim = 100*sim_out$lambda_model
lambda_sim_d = density(lambda_sim)
lambda_U_d = density(sim_out$lambda_U*100) # density for unconstrained
#
# compute a number of ranges for the graph
x_rng = seq(0.01,99.9,by=0.01)
y_rng = pdf_U(x_rng,theta=params$theta,mu=params$mu,sigma=params$sigma)
ycap = 0.065
ymax = max(c(lambda_data_d$y,lambda_U_d$y))
ymax = 1.2*min(ymax,ycap)
eps =0.5
lambda.RCR <- lambda_RCR(RCR=params$RCR,alpha=0) # firm-specific if alpha het, not here.
RCR <- 100*lambda.RCR
brk = ymax*0.85
gap = 0.0015
#
#  plot the model + lib + con data ====
pdf("Plots_JIE_rev/AALA_calib/AALA_calib_model_data.pdf",7.5,4)
par(mar=c(4,4,0.5,0.75)+0.1 )
# empty plot
plot(c(0,100),c(0,0),col="black",lty="dotted",main ="",xlab="Nafta cost share (%)",
     ylab="Density",xlim=c(0,100),xaxs="i",ylim=c(0.0,ymax),type="n",xaxt="n")
# data and RCR
DR[!is.na(nafta_shr_con) & ell %in% CAMUS,lines(density(nafta_shr_con),col="black",lwd=2)]
# unconstrained lambda_U
lines(x_rng,y_rng,type="l",col="orange",lwd=3)
# model (basic, delta het only, alpha = 0)
delta_star = delta.star(lambda_R=lambda.RCR,tau=params$tau,theta=params$theta)
lambda_star = 100*lambda_U(delta_star,params$theta)
xaxmarks =c(0,lambda_star,25,50,RCR,75,85,100)
axis(side = 1, at=xaxmarks,labels=c(0,TeX("$\\lambda_U(\\delta^*)$"),25,50,TeX(sprintf(r'($\lambda_R = %.1f $)',RCR)),75,85,100))
# non-compliers density
polygon(c(x_rng[x_rng<lambda_star],lambda_star),c(y_rng[x_rng<lambda_star],0),col=adjustcolor("forestgreen",0.25),border=NA)
# compliers unconstrained density
polygon(c(RCR,x_rng[x_rng>RCR]),c(0,y_rng[x_rng>RCR]),col=adjustcolor("forestgreen",0.25),border=NA)
lines(x_rng[x_rng<lambda_star],y_rng[x_rng<lambda_star],col="forestgreen",lwd=1)
segments(lambda_star,0,lambda_star,max(pdf_U(lambda_star,theta=params$theta,mu=params$mu,sig=params$sig)),col="forestgreen",lwd=1)
segments(lambda_star,0,RCR,0,col="forestgreen",lwd=1)
# complier constrained 
polygon(c(RCR-eps,RCR+eps,RCR+eps,RCR-eps),c(ymax*0.95,ymax*0.95,brk,brk+gap),col=adjustcolor("forestgreen",0.25),border = NULL)
polygon(c(RCR-eps,RCR+eps,RCR+eps,RCR-eps),c(brk,brk-gap,0,0),col=adjustcolor("forestgreen",0.25))
text(RCR,ymax*0.95,sprintf("Comply con.= %.2f",round(sim_out$comply_frac,2)),pos=4,cex=0.8)
lines(x_rng[x_rng>RCR],y_rng[x_rng>RCR],col="forestgreen",lwd=1)
# text(17.5,ymax/3,"Non-compliers",pos=2,cex=0.8,adj=0)
# arrows(17,ymax/3,lambda_star-3*eps, 0.6* eps/100,length=0.07)
text(10,ymax/3,"Non-compliers",pos=3,cex=0.8,adj=0)
arrows(10,ymax/3,lambda_star-3*eps, 0.6* eps/100,length=0.07)
text(65,0.005,"Comply uncon.",pos=4,cex=0.8)
#arrows(75,0.007,RCR+8*eps,0.005,length=0.07)
#legend("topleft",lwd=c(2,2,2),col=c("blue","black","orange","forestgreen"),legend=c("Data (con)","Data (lib)",TeX("Model (no ROO)"),"Model (with ROO)"),bty="n")
legend("topleft",lwd=c(2,2,2),col=c("black","orange","forestgreen"),legend=c("Data",TeX("Model (no ROO)"),"Model (with ROO)"),bty="n")
legend("topright",legend=c(TeX(sprintf(r'($\theta = %.1f$)', params$theta)),
                           TeX(sprintf(r'($\mu = %.2f$)', params$mu)),
                           TeX(sprintf(r'($\sigma = %.2f$)', params$sigma)),
                           TeX(sprintf(r'($\tau = %.2f$)', params$tau)),
                           TeX(sprintf(r'($\alpha = %.0f$)', 0))),
       bty="n")
dev.off()

