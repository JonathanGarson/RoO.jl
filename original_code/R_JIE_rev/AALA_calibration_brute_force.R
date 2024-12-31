library(tictoc)
library(HeadR)
library(latex2exp)
library(parallel)
library(beepr)
rm(list=ls())
source("original_code/R_JIE_rev/AALA_calibration_functions.R") # 
#
calib.years <- 2011:2019
gen.figs <- TRUE # set to TRUE to generate  density and Laffer curves
numpar <- 4 # number of parameters to brute force  (grid search) over
#
L2norm <- function(u,v) sqrt(sum((u-v)^2))
CAMUS = c("CA","MX","US")
MEX_con_lib <- "con" # either con or lib assumption on Mexican share
dist.alpha <- "Beta" # 
alpha.base <- 0.15
theta.base <- 4
params = list(RCR=0.625)
RCR = 100*params$RCR
#
DR <- readRDS("original_code/Data/RDS_JIE_rev/AALA_rev.rds")[year %in% calib.years]  # data prepared by AALA_clean.R, with many fixes
#
# Choose the conservative or liberal going forward, and compute data density, also load tau index
if(MEX_con_lib=="con")  DR[,nafta_shr := nafta_shr_con] else DR[,nafta_shr := nafta_shr_lib] 
DR[,summary(nafta_shr)]
lambda_data_d = DR[!is.na(nafta_shr) & ell %in% CAMUS, density(nafta_shr)]
Num.obs = DR[!is.na(nafta_shr)& ell %in% CAMUS,.N]  #1704
DD <- data.table(x=lambda_data_d$x,y=lambda_data_d$y)
DD[,x_rnd := round(x)]
DD <- DD[x_rnd %between% c(0,100),.(den_data = mean(y)),by=x_rnd]
#
DC <- readRDS("original_code/Data/RDS_JIE_rev/tau_index_DRF.rds") # 
#
params = list(theta=theta.base,mu=0.0,sigma=0.0,RCR=0.625,tau=DC$tau_index,alpha=alpha.base,conc.err=1e10)
params$tau <- data.table(tau=DC$tauD,tauQ=1)
#
# Optimization with 4 params: mu sigma alpha-conc err-conc ====
#
  mu.grid <- seq(from=-0.1,to=0.25,by=0.01) # 36 values
  sigma.grid <- seq(from=0.0,to=0.25,by=0.01) #26
  dist.alpha <- "Beta"
  alphacon.grid <- c(1,1.25,1.5,1.75,2,2.25,2.5,3:20) 
  errcon.grid <- c(2:25) #24
  #
  param.grid <- CJ(sigma=sigma.grid,alphacon =alphacon.grid,errcon =errcon.grid) #cartesian product of params (other than mu)
  n.vals = dim(param.grid)[1]
  k = dim(param.grid)[2]+1  # number of parameters to brute force over
  #
  # test <- brute_force_4par(0) # check it works for mu= 0 before launching full search
  tic()
  # DT.results <- mclapply(mu.grid,brute_force_4par,mc.cores = detectCores()-2) |> rbindlist()
  # cl <- makeCluster(detectCores() - 2)
  # DT.results <- parLapply(cl, mu.grid, brute_force_4par) |> rbindlist()
  DT.results <- mclapply(mu.grid,brute_force_4par,mc.cores = 1) |> rbindlist()
  toc()
  #
  DT.results[order(loss)] |> head(n=10)
  beepr::beep()
  setorder(DT.results,loss)
  #the first obs of each vector is the best fit parameter "1" => lowest value of loss fn.
  mu1 =DT.results$mu[1]
  sigma1 =DT.results$sigma[1]
  alphacon1 =DT.results$cona[1]
  errcon1 =DT.results$cone[1]
  #plot the least-loss-cona alpha draws: with concentration <2, histogram looks bimodal
  ubeta_draws(10000,centre=alpha.base,concentration =DT.results[order(loss)][1,]$cona) |> hist()
  # loss function not monotonic in the concentration parameter (holding mu, sigma at opt values)
  setorder(DT.results,mu,sigma,cone,cona)
  DT.results[mu==mu1 & sigma==sigma1 & cone==errcon1,scatter(cona,loss,log="x",type="b")]
  setorder(DT.results,mu,sigma,cona,cone)
  DT.results[mu==mu1 & sigma==sigma1 & cona==alphacon1,scatter(cone,loss,log="x",type="b")]
  
  # save the parameters for the density plot and laffer curve plot
  params = list(theta=theta.base,mu=mu1,sigma=sigma1,
                RCR=0.625,tau=DC$tauD,alpha=alpha.base,conc.alpha=alphacon1,conc.err=errcon1)
  params$tau <- data.table(tau=DC$tauD,tauQ=1)
  saveRDS(params,paste0("Data/RDS_JIE_rev/Params4/params_opt_",MEX_con_lib,"_alpha",alpha.base*100,"_theta",theta.base,".rds")) #saving optimized parameters for use in welfare calculations

# Params4/params_opt_ is the one we actually use on line 4 of the AALA_calibration_density_laffer_curves.R 
#
#now create the figures using the parameters above.
if(gen.figs==TRUE) source("R_JIE_rev/AALA_calibration_density_laffer_curves.R")    
