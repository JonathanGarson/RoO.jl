# Notation:
# RCR: the ROO imposed by the government on the regional cost share of total costs
# lambda_R: the regional cost share of parts costs
# chi_R: the regional parts share 
# alpha is the share of assembly in total costs (uniform first, and then heterogeneous)
#

#
# Base functions ====
#

# lambda_R as a function of RCR (expressed as cost share incl assembly), for a given alpha
lambda_RCR <- function(RCR,alpha) {
  lambda_R <- fifelse(alpha>=0 & alpha<RCR, (RCR-alpha)/(1-alpha),0)
  return(lambda_R)
  }
# chi_R as a function  of lambda_R 
chi_lambda <- function(lambda_R,delta,theta) {
  denom = 1+((1/lambda_R -1)/delta)^(theta/(theta+1))
  return(1/denom)
}
# Unconstrained parts share
chi_U <- function(delta,theta) 1/(1+delta^(-theta))
# Unconstrained costs share : uses the EK miracle 
lambda_U <- chi_U 
#
# Analytic density of chi_U (and lambda_U) for unconstrained firms
pdf_U <- function(x,theta,mu,sigma,pct=TRUE) {
  if(pct==TRUE) x = x/100 
  y = x/(1-x)
  g =  dlnorm(y^(1/theta),mu,sigma)*(1/(theta*x^2))*y^(1+1/theta)
  if(pct==TRUE) return(g/100) else return(g)
}
# Unconstrained cost  (call it C_U?).
C_U <- function(delta,theta) { #  index <- (1+delta^(-theta))^(-1/theta)
  index <- chi_U(delta,theta)^(1/theta)
  return(index)
}
# Cost of compliance 
C_comply <- function(lambda_R,delta,theta) {
  chi_R <- chi_lambda(lambda_R,delta=delta,theta=theta) #  eqn 5
  k <- (1+theta)/theta
  chi_R^k +delta*(1-chi_R)^k  # eqn 2
}
# C.tilde is C_comply / C_U
C.tilde <- function(lambda_R,delta,theta) {
  cons <- lambda_R > lambda_U(delta=delta,theta = theta) 
  y <- cons * C_comply(lambda_R,delta=delta,theta = theta)/C_U(delta=delta,theta = theta) +1-cons
  return(y)
}
# Limit of delta star as lambda_R -> 1 (in paper lambda_R -> chi_R, delta_max -> \bar{\delta}(\tau))
delta_max <- function(tau,theta) {
  (tau^(theta) -1)^(-1/theta)
}
# Cutoff delta for complying
delta.star <- function(lambda_R,tau,theta) {
  ufn <- function(x) C.tilde(lambda_R=lambda_R,delta=x,theta=theta)-tau
  dmax <- delta_max(tau=tau,theta = theta)
  uniroot(ufn,interval=c(0.00001,dmax))$root
}
# Cutoff delta for compliant - unconstrained
delta.circ <- function(lambda_R,theta) {
  (lambda_R^(-1) -1)^(-1/(theta-1))
}

#
# Lambda simulation simplest version ====
#
sim_lambda <- function(RCR,mu,sigma,theta,tau_data,alpha.lo,alpha.hi,alpha.a,alpha.b,N) {
  # draw the three dimensions of heterogeneity
  delta <- rlnorm(N,mu,sigma) #always firm specific
  alpha <- alpha.lo +rbeta(N,alpha.a,alpha.b)*(alpha.hi-alpha.lo)
  lambda.R <- lambda_RCR(RCR,alpha) # firm-specific if alpha het
  # deal with bug in sample() see ?sample documentation
  if(length(tau_data)==1) tau = tau_data else(tau <- sample(tau_data,N,replace=TRUE))  # firm-specific unless tau_data is a scalar
  lambda.U <- lambda_U(delta=delta,theta=theta)
  comply.cost <- C.tilde(lambda_R=lambda.R,delta=delta,theta=theta)
  # comply if cost penalty < tariff penalty and compliance is constrained if ideal lambda < required lambda
  comply.con <- comply.cost <= tau &  lambda.U < lambda.R
  # dummy formulation of if/else
  lambda_model <- lambda.R*comply.con +lambda.U*(1-comply.con)
  RCS <- alpha*(1-lambda_model) + lambda_model
  return(list(lambda_U=lambda.U,lambda_model=lambda_model,lambda_R=lambda.R, RCS = RCS, comply.cost = comply.cost, 
              comply_frac = mean(comply.con),alpha = alpha,alpha_rng =range(alpha),delta_rng =range(delta),tau_rng =range(tau)))
}

#
# Function for generating het alpha constrained between 0 and 1.
#
beta_draws <- function(N,centre,concentration) {
#limits
a = 0
c= 2*centre
mu = centre/c  #0.5
x = rbeta(N,mu*concentration,(1-mu)*concentration)
return(a+x*(c-a)) #rescale to fit between 0 and 1
}

ubeta_draws <- function(N,centre,concentration) {
rbeta(N,centre*concentration,(1-centre)*concentration)
}
# invoke as beta_draws(N,centre=alpha.base,concentration = alpha+beta=number inversely related to spread )  


# This is the main function which gets used for two purposes: 
# 1) calibration to lambda 
# 2) welfare calculations using chi
sim_lambda_alpha <- function(RCR,mu,sigma,theta,tau_data,mu.alpha,conc.alpha,conc.err,N) {
# draw the three dimensions of heterogeneity
#1) delta:
  delta <- rlnorm(N,mu,sigma) #always firm specific
#2) tau, tauQ sampled jointly
  tauDT <- tau_data[sample(nrow(tau_data),N,replace=TRUE)]  #
  tau <- tauDT$tau
  tauQ <- tauDT$tauQ
  #3) alpha:    
   alpha <- ubeta_draws(N,centre=mu.alpha,concentration = conc.alpha)
# compute costs
  lambda.R <- lambda_RCR(RCR,alpha) # firm-specific if alpha het
  lambda.U <- lambda_U(delta=delta,theta=theta)
  C.R <- C_comply(lambda_R=lambda.R,delta=delta,theta=theta)
  C.U <- C_U(delta=delta,theta=theta)
  comply.cost <- C.tilde(lambda_R=lambda.R,delta=delta,theta=theta)^(1-alpha) #modified
  # comply=TRUE if cost penalty < tariff penalty and compliance is constrained if ideal lambda < required lambda
  comply.con <- comply.cost <= tau &  lambda.U < lambda.R
  comply.uncon <- lambda.U > lambda.R
  noncomp <- 1-comply.con-comply.uncon
  compliance <- fcase(
    comply.con==TRUE,"CC",
    comply.uncon==TRUE,"CU",
    noncomp==TRUE,"NC"
      )
  # dummy formulation of if/else
  lambda_true <- lambda.R*comply.con +lambda.U*(1-comply.con) # this is the true lambda (relevant for laffer curve)
  chi_true <- chi_lambda(lambda_R=lambda.R,delta=delta,theta=theta)*comply.con + lambda.U*(1-comply.con) # this is the true chi (building on the fact that lambda_U=chi_U)
  cost_true <- comply.con*C.R^(1-alpha) + comply.uncon*C.U^(1-alpha) +  noncomp*tau*C.U^(1-alpha) # modified, parts factor of total costs
  # add on  error
  lambda_model <- ubeta_draws(N,centre=lambda_true,concentration = conc.err)
  #   
  chi_model <- chi_true # no err because this is not part of calibration
  RCS <- alpha*(1-lambda_true) + lambda_true
  return(list(lambda_U=lambda.U,lambda_R=lambda.R,lambda_model=lambda_model,chi_model=chi_model,RCS = RCS,
              cost_true=cost_true, comply.cost = comply.cost, compliance=compliance,
              CC_frac = mean(comply.con),CU_frac = mean(comply.uncon),alpha_mean =mean(alpha),alpha_rng =range(alpha),
              delta_rng =range(delta),tau_rng =range(tau),alpha=alpha,tau=tau,tauQ=tauQ,delta=delta))
} 
 
# version keeping track of j's V_iso_o
sim_lambda_alpha_o <- function(RCR,mu,sigma,theta,tau_data,mu.alpha,conc.alpha,conc.err,N) {
  # draw the three dimensions of heterogeneity
  #1) delta:
  delta <- rlnorm(N,mu,sigma) #always firm specific
  #2) tau: (# deal with bug in sample() see ?sample documentation)
  tau_o <- tau_data[sample(nrow(tau_data),N,replace=TRUE)]  #
  tau <- tau_o$tau
  tauQ <- tau_o$tauQ
  V_iso_o <- tau_o$V_iso_o
  #3) alpha:    
  alpha <- ubeta_draws(N,centre=mu.alpha,concentration = conc.alpha)
  # compute costs
  lambda.R <- lambda_RCR(RCR,alpha) # firm-specific if alpha het
  lambda.U <- lambda_U(delta=delta,theta=theta)
  C.R <- C_comply(lambda_R=lambda.R,delta=delta,theta=theta)
  C.U <- C_U(delta=delta,theta=theta)
  comply.cost <- C.tilde(lambda_R=lambda.R,delta=delta,theta=theta)^(1-alpha) #modified
  # comply if cost penalty < tariff penalty and compliance is constrained if ideal lambda < required lambda
  comply.con <- comply.cost <= tau &  lambda.U < lambda.R
  comply.uncon <- lambda.U > lambda.R
  noncomp <- 1-comply.con-comply.uncon
  compliance <- fcase(
    comply.con==TRUE,"CC",
    comply.uncon==TRUE,"CU",
    noncomp==TRUE,"NC"
  )
  # dummy formulation of if/else
  lambda_true <- lambda.R*comply.con +lambda.U*(1-comply.con) # this is the true lambda (relevant for laffer curve)
  chi_true <- chi_lambda(lambda_R=lambda.R,delta=delta,theta=theta)*comply.con + lambda.U*(1-comply.con) # this is the true chi (building on the fact that lambda_U=chi_U)
  cost_true <- comply.con*C.R^(1-alpha) + comply.uncon*C.U^(1-alpha) +  noncomp*tau*C.U^(1-alpha) # modified, parts factor of total costs
  # add on  error
  lambda_model <- ubeta_draws(N,centre=lambda_true,concentration = conc.err)
  #   
  chi_model <- chi_true # no err because this is not part of calibration
  RCS <- alpha*(1-lambda_true) + lambda_true
  return(list(lambda_U=lambda.U,lambda_R=lambda.R,lambda_model=lambda_model,chi_model=chi_model,RCS = RCS,cost_true=cost_true, comply.cost = comply.cost, compliance=compliance,
              CC_frac = mean(comply.con),CU_frac = mean(comply.uncon),alpha_mean =mean(alpha),alpha_rng =range(alpha),delta_rng =range(delta),tau_rng =range(tau),
              alpha=alpha,tau=tau,tauQ=tauQ,delta=delta,V_iso_o=V_iso_o))
} 

# function for simulating with relocation (tau DRF) ====
sim_lambda_alpha_DRF <- function(RCR,mu,sigma,theta,tau_data,mu.alpha,conc.alpha,conc.err,omegaF,omegaR,kappa,N) {
  # draw the three dimensions of heterogeneity
  #1) draw N length vector of delta.tilde 
  delta.tilde <- rlnorm(N,mu,sigma) #always firm specific
  delta <- delta.tilde/kappa #delta.tilde = delta*kappa in the relocation section notation
  #2) draw N length vectors of tauD, tauR, tauF: 
  tauDT <- tau_data[sample(nrow(tau_data),N,replace=TRUE)]  # carline-specific draws of sextuples ({tau tauQ} X {DRF}, held together)
  #3) alpha:    
  alpha <- ubeta_draws(N,centre=mu.alpha,concentration = conc.alpha)
  Cost_W <- data.table(id=1:nrow(tauDT),tauDT,alpha,delta)
  # compute costs, n.b. the kappa needs to be multiplied back on! 
  Cost_W[, lambda.R := lambda_RCR(RCR,alpha) ] # parts cost share equivalent of the RCR (which includes assembly costs)
  Cost_W[, C.U := C_U(delta=delta*kappa,theta=theta)] # parts cost of complying domestically *unconstrained*
  Cost_W[, CDC := C.tilde(lambda_R=lambda.R,delta=delta*kappa,theta=theta)^(1-alpha) ] # rel. cost of complying domestically *constrained*
  Cost_W[, NCD := tauD] # rel. cost of non-compliance domestically (stay in MX, pay the MFN to Canada and US)
  Cost_W[, NCR := tauR*omegaR] # rel. cost of non-compliance in the RTA (->USA in fact), where costs are omegaR higher
  Cost_W[, NCF := tauF*omegaF*(kappa*C_U(delta=delta/kappa,theta=theta)/C.U)^(1-alpha)] # rel. cost of non-compliance in F
  idvars = names(Cost_W)[1:13] #id ... chosen_R C.U (unlike in exog location versions, we need to keep  chosen_R)
  Cost_L <- melt(Cost_W,id.vars = idvars,measure.vars =c("CDC","NCD","NCR","NCF"), value.name = "relcost", variable.factor=FALSE,variable.name = "choice")
  setorder(Cost_L,id,relcost)  
  Cost_min <- Cost_L[,.(choice= first(choice),relcost=first(relcost)),by=idvars]
  Cost_min[relcost==1 & choice=="CDC", choice:="CDU"] # important: when relcost is one and choice is CD, then complying is unconstrained
  # costs in levels , modified to include alpha
  Cost_min[, cost_true := relcost*C.U^(1-alpha) ]  
  # compute parts cost share before adding errors
  Cost_min[,lambda_U := fcase(
    choice %in% c("NCD","CDU","NCR","CDC"),lambda_U(delta=delta*kappa,theta=theta),
    choice == "NCF",lambda_U(delta=delta/kappa,theta=theta)
  )]
  #
  Cost_min[,lambda_true := fcase(
    choice %in% c("NCD","CDU","NCR"),lambda_U(delta=delta*kappa,theta=theta),
    choice == "NCF",lambda_U(delta=delta/kappa,theta=theta),
    choice== "CDC",lambda.R
  )]
  # now convert to fractions of parts, requires a special transformation for the constrained carlines
  Cost_min[,chi_true := fcase(
    choice %in% c("NCD","CDU","NCR","NCF"),lambda_true,
    choice== "CDC",chi_lambda(lambda_R=lambda.R,delta=delta*kappa,theta=theta)
  )]
  
  # add on  error to lambda_true, has to be done like this to keep  0<=lambda_model <= 1
  Cost_min[, lambda_model := ubeta_draws(N,centre=lambda_true,concentration = conc.err)]
  #   
  Cost_min[, chi_model := chi_true] # no err because this is not part of calibration
  Cost_min[, RCS := alpha*(1-lambda_true) + lambda_true]
  return(Cost_min)
} 

  
#
#Functions for the Laffer curve ====
#
sim_avg_RCS_alpha <- function(RCR,theta,mu,sigma,tau_data,alpha,conc.alpha,conc.err,N) {
  set.seed(140341)
  sim_out <- sim_lambda_alpha(RCR=RCR,theta=theta,mu=mu,sigma=sigma,
                            alpha,conc.alpha,conc.err,
                            tau_data=tau_data,N=N)
  RCS = 100*sim_out$RCS
  return(avg_RCS=mean(RCS))
}

sim_choice <- function(RCR,theta,mu,sigma,tau_data,alpha,conc.alpha,conc.err,N) {
  set.seed(140341)
  sim_out <- sim_lambda_alpha(RCR=RCR,theta=theta,mu=mu,sigma=sigma,
                              alpha,conc.alpha,conc.err,
                              tau_data=tau_data,N=N)
  sim_out <- data.table(choice=sim_out$compliance)
  choices <- sim_out[, .(N = .N), by=choice]
  return(data.table(RCR=RCR,choices))
}

sim_avg_RCS_alpha_DRF <- function(RCR,theta,mu,sigma,tau_data,alpha,conc.alpha,conc.err,N,omegaR,omegaF,kappa) {
  set.seed(140341)
  sim_out <- sim_lambda_alpha_DRF(RCR=RCR,theta=theta,mu=mu,sigma=sigma,
                              alpha,conc.alpha,conc.err,
                              tau_data=tau_data,N=N,omegaR=omegaR,omegaF=omegaF,kappa=kappa)
  RCS = 100*sim_out$RCS
  return(avg_RCS=mean(RCS))
}

sim_choice_DRF <- function(RCR,theta,mu,sigma,tau_data,alpha,conc.alpha,conc.err,N,omegaR,omegaF,kappa) {
  set.seed(140341)
  sim_out <- sim_lambda_alpha_DRF(RCR=RCR,theta=theta,mu=mu,sigma=sigma,
                                  alpha,conc.alpha,conc.err,
                                  tau_data=tau_data,N=N,omegaR=omegaR,omegaF=omegaF,kappa=kappa)
 
   choices <- sim_out[, .(N = .N, N_USA = sum(chosen_R=="USA"), N_CAN = sum(chosen_R=="CAN"), N_MEX = sum(chosen_R=="MEX")), by=.(V_iso_o,choice)]
  return(data.table(RCR=RCR,choices))
}


#
# Loss functions for  parameters to estimate. theta is now assumed fixed in all of those ====
#
#
loss_fun_alpha <- function(parm.est) {
  set.seed(140341)
  sim_out <- sim_lambda_alpha(RCR=params$RCR,theta=params$theta,mu=parm.est[1],sigma=sqrt(abs(parm.est[2])^2),
                          mu.err = parm.est[3],sigma.err=sqrt(abs(parm.est[4])^2),
                          mu.alpha=params$alpha,conc.alpha=parm.est[5],tau_data=params$tau,N=Num.obs*20)
  lambda_sim = 100*sim_out$lambda_model
  lambda_sim_d = density(lambda_sim)
  # density of simulated lambda
  DS = data.table(x=lambda_sim_d$x,y=lambda_sim_d$y)
  DS[,x_rnd := round(x)]
  DS <- DS[x_rnd %between% c(0,100),.(den_sim = mean(y)),by=x_rnd]
  #data density
  lambda_data_d = DR[!is.na(nafta_shr) & ell %in% CAMUS,density(nafta_shr)]
  #merge the data
  DS <- merge(DD,DS,by="x_rnd")
  fit = L2norm(DS$den_data,DS$den_sim)
#  cat("L2Norm = ",fit,"\n")
  return(fit)
}
#
# Four parameter loss function ====
loss_fun_4par <- function(mu_est,sigma_est,cona_est,cone_est) {
  set.seed(140341)
  sim_out <- sim_lambda_alpha(RCR=params$RCR,theta=params$theta,mu=mu_est,sigma=sigma_est,
                              conc.err=cone_est,
                              mu.alpha=params$alpha,conc.alpha=cona_est,tau_data=params$tau,N=Num.obs*20)
  lambda_sim = 100*sim_out$lambda_model
  lambda_sim_d = density(lambda_sim)
  # density of simulated lambda
  DS = data.table(x=lambda_sim_d$x,y=lambda_sim_d$y)
  DS[,x_rnd := round(x)]
  DS <- DS[x_rnd %between% c(0,100),.(den_sim = mean(y)),by=x_rnd]
  #data density
  lambda_data_d = DR[!is.na(nafta_shr) & ell %in% CAMUS,density(nafta_shr)]
  #merge the data
  DS <- merge(DD,DS,by="x_rnd")
  fit = L2norm(DS$den_data,DS$den_sim)
  return(fit)
}


#
# Brute force functions ====
#
brute_force_4par <- function(mu.val){
  mu.rep <- rep(mu.val,n.vals)
  loss <- mapply(loss_fun_4par,mu.val,param.grid$sigma,param.grid$alphacon,param.grid$errcon)
  return(data.table(mu=mu.rep,sigma=param.grid$sigma,cona=param.grid$alphacon,cone=param.grid$errcon,loss=loss))  
}
