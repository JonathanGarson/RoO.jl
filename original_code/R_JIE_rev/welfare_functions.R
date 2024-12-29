# New features relative to welfare_functions_sept4.R:
# modified because CES price index version had a tau inconsistency
# new functions based on "partial equilibrium/quasi linear" constant price elasticity demand
# this involves that s_hat lines are modified to remove price index adjustment. 
# also: new -eta mean -> tauQ

###
#A function which outputs C.hat for each carline WITH RESPECT TO RCR=0 (therefore drops RCR as an argument)

# EHA without relocation combined Nafta ====

# crucial function combined Nafta ====
C.hat_alpha0 <- function(RCR.prime,theta,mu,sigma,tau_data,mu.alpha,conc.alpha,conc.err,N) {
  #Run simulation with RCR=0
  set.seed(140341)
  sim_out <- sim_lambda_alpha(RCR=0,theta=theta,mu=mu,sigma=sigma,
                              mu.alpha=mu.alpha,conc.alpha=conc.alpha,conc.err=conc.err,
                              tau_data=tau_data,N=N)
  cost <- sim_out$cost_true
  tauQ <- sim_out$tauQ
  alpha <- sim_out$alpha
  lambda_U <- sim_out$lambda_U
  chi_model <- sim_out$chi_model
  chi.U <- lambda_U 
  compliance <- sim_out$compliance
  compliance <- fcase(
    compliance == "CC","Comply-constrained",
    compliance == "CU","Comply-unconstrained",
    compliance == "NC","Non-compliant"
  )
  #Run simulation with RCR.prime
  set.seed(140341)
  sim_out_prime <- sim_lambda_alpha(RCR=RCR.prime,theta=theta,mu=mu,sigma=sigma,
                                    mu.alpha=mu.alpha,conc.alpha=conc.alpha,conc.err=conc.err,
                                    tau_data=tau_data,N=N)
  cost_prime <- sim_out_prime$cost_true
  lambda_U_prime <- sim_out_prime$lambda_U
  chi_model_prime <- sim_out_prime$chi_model
  chi.U_prime <- lambda_U_prime 
  compliance_prime <- sim_out_prime$compliance
  compliance_prime <- fcase(
    compliance_prime == "CC","Comply-constrained",
    compliance_prime == "CU","Comply-unconstrained",
    compliance_prime == "NC","Non-compliant"
  )
  # Individual domestic carline change in cost:
  C_hat_j <- (cost_prime/cost)^(1-alpha)  #C_hat_j is now actually p_hat_j as they now take into account alpha>0
  #
  # Price index change of domestic carlines
  P_hat.D <- (sum(C_hat_j^(1-eta))/N)^(1/(1-eta))
  # Price index (not needed but other functions look for it)
  P_hat <- 1
  #
  # s_hat_j : sales value change of each domestic carline
  s_hat_j <- (C_hat_j)^(1-eta)
  #
  # q_hat_j : quantity change of each domestic carline
  q_hat_j <- fcase(
    compliance_prime != "Non-compliant",C_hat_j^(-eta),
    compliance_prime == "Non-compliant",tauQ^(-eta)
    )
  #new elements of aggregation
  chi_hat_j <- chi_model_prime/chi_model
  L_hat_j <- chi_hat_j*q_hat_j
  LA_hat_j <- q_hat_j
  #
  return(data.table(RCR.prime=RCR.prime,C.hat=C_hat_j, P_hat.D=P_hat.D, P_hat=P_hat, 
                    compliance = compliance, compliance_prime= compliance_prime,
                    alpha=sim_out$alpha,tau=sim_out$tau,delta=sim_out$delta,
                    s_hat=s_hat_j,q_hat=q_hat_j,chi_hat_j,L_hat_j,LA_hat_j
                    ))
}

#function that aggregates  the simulation outputs combined Nafta ====
C.hat.Agg.base.prime <- function(RCR.base,RCR.prime){
  N=Num.obs*Nfactor
  #Run the simulation for the base (62.5 for NAFTA)
  DC.hat.base <- C.hat_alpha0(RCR.prime=RCR.base,theta=params$theta,mu=params$mu,sigma=params$sigma,conc.err=params$conc.err,
                         mu.alpha=params$alpha,conc.alpha=params$conc.alpha,tau_data=params$tau,N=N)
  
  #Run the simulation for RCR.prime
  DC.hat.prime <- C.hat_alpha0(RCR.prime=RCR.prime,theta=params$theta,mu=params$mu,sigma=params$sigma,conc.err=params$conc.err,
                           mu.alpha=params$alpha,conc.alpha=params$conc.alpha,tau_data=params$tau,N=N)
  
  #Creating a unique data.table for both experiences (note the compliances are both primes, under both scenarios)
  DC.hat <- data.table(RCR.base=RCR.base,RCR.prime=RCR.prime,
                       delta=DC.hat.base$delta,tau=DC.hat.base$tau,
                       compliance=DC.hat.base$compliance_prime,compliance_prime=DC.hat.prime$compliance_prime, #key line
                       P.hat.base=DC.hat.base$P_hat,P.hat.prime=DC.hat.prime$P_hat,
                       C.hat.base=DC.hat.base$C.hat,C.hat.prime=DC.hat.prime$C.hat,
                       q_hat.base=DC.hat.base$q_hat,q_hat.prime=DC.hat.prime$q_hat,
                       chi_hat.base=DC.hat.base$chi_hat,chi_hat.prime=DC.hat.prime$chi_hat,
                       L_hat.base=DC.hat.base$L_hat,L_hat.prime=DC.hat.prime$L_hat,
                       LA_hat.base=DC.hat.base$LA_hat,LA_hat.prime=DC.hat.prime$LA_hat
                       )
  #Collapse twice
  DC.hat.c <- DC.hat[,.(P_hat.base = first(P.hat.base),
                        P_hat.prime = first(P.hat.prime), 
                        P_hat.D_i.base = (sum(C.hat.base^(1-eta))/(.N))^(1/(1-eta)),
                        P_hat.D_i.prime = (sum(C.hat.prime^(1-eta))/(.N))^(1/(1-eta)), 
                        X_hat_i.base = mean(chi_hat.base), X_hat_i.prime = mean(chi_hat.prime),
                        L_hat_i.base = mean(L_hat.base), L_hat_i.prime = mean(L_hat.prime),
                        LA_hat_i.base = mean(LA_hat.base), LA_hat_i.prime = mean(LA_hat.prime),
                        share.obs = round(100*(.N/N),1), 
                        meandelta = round(mean(delta),3), meantau = round(mean(tau),3)), 
                     by=.(RCR.base,RCR.prime,compliance,compliance_prime)]
  #
    DC.hat.agg <- DC.hat[,.(P_hat.base = first(P.hat.base),
                          P_hat.prime = first(P.hat.prime), 
                          P_hat.D_i.base = (sum(C.hat.base^(1-eta))/(.N))^(1/(1-eta)),
                          P_hat.D_i.prime = (sum(C.hat.prime^(1-eta))/(.N))^(1/(1-eta)), 
                          X_hat_i.base = mean(chi_hat.base), X_hat_i.prime = mean(chi_hat.prime),
                          L_hat_i.base = mean(L_hat.base), L_hat_i.prime = mean(L_hat.prime),
                          LA_hat_i.base = mean(LA_hat.base), LA_hat_i.prime = mean(LA_hat.prime),
                          share.obs = round(100*(.N/N),1),
                          meandelta = round(mean(delta),3), meantau = round(mean(tau),3)), 
                     by=.(RCR.base,RCR.prime)]
  DC.hat.agg[,`:=` (compliance = "All",compliance_prime = "All")]
  DC.hat.c <- rbind(DC.hat.c,DC.hat.agg)
  #
  #Take ratio
  # later: fix the modify in place <- issue
  DC.hat.c <- DC.hat.c[, `:=`(P_hat.D_i = P_hat.D_i.prime/P_hat.D_i.base,
                              P_hat = P_hat.prime/P_hat.base, 
                              X_hat.D_i = X_hat_i.prime/X_hat_i.base,
                              L_hat.D_i = L_hat_i.prime/L_hat_i.base,
                              LA_hat.D_i = LA_hat_i.prime/LA_hat_i.base
                              )]
  DC.hat.c <- DC.hat.c[, s_hat.D_i := (P_hat.D_i)^(1-eta)] # change in value of sales
   #following theory back out what zeta0 would be without a ROO.
if(RCR.prime==0.625)  zeta0 <<- zeta*DC.hat.c[compliance == "All" & compliance_prime == "All", L_hat.D_i/LA_hat.D_i ]

DC.hat.c <- DC.hat.c[, LT_hat.D_i := (zeta0*LA_hat.D_i + L_hat.D_i)/(zeta0+1)]
  
   #Subtract one from each hat variable, round 
round.prec <- 5
DC.hat.c <- DC.hat.c[, `:=`(P_hat.D_i = round(100*(P_hat.D_i-1),round.prec), 
                              s_hat.D_i = round(100*(s_hat.D_i-1),round.prec),
                              X_hat.D_i = round(100*(X_hat.D_i-1),round.prec),
                              L_hat.D_i = round(100*(L_hat.D_i-1),round.prec),
                              LT_hat.D_i = round(100*(LT_hat.D_i-1),round.prec)
  )]
  #Order 
  DC.hat.c[, order :=  fcase(
    compliance == "Comply-constrained" & compliance_prime == "Non-compliant",1,
    compliance == "Comply-unconstrained" & compliance_prime == "Non-compliant",2,
    compliance == "Comply-constrained" & compliance_prime == "Comply-constrained",3,
    compliance == "Comply-unconstrained" & compliance_prime == "Comply-constrained",4,
    compliance == "Non-compliant" & compliance_prime == "Non-compliant",5,
    compliance == "Comply-unconstrained" & compliance_prime == "Comply-unconstrained",6,
    #Those last two below are special to the case of returning to 0
    compliance == "Non-compliant" & compliance_prime == "Comply-unconstrained",1,
    compliance == "Comply-constrained" & compliance_prime == "Comply-unconstrained",2,
    compliance == "All" & compliance_prime == "All",7
  )]
  setorder(DC.hat.c,RCR.prime,order)
  DC.hat.c[,order := NULL]
  #            
  return(DC.hat.c[,.(compliance,compliance_prime,share.obs,P_hat.D_i,s_hat.D_i,X_hat.D_i,L_hat.D_i,LT_hat.D_i)])
  }

# EHA without relocation by country ====
#
# crucial function by country ====
C.hat_alpha0_o <- function(RCR.prime,theta,mu,sigma,tau_data,mu.alpha,conc.alpha,conc.err,N) {
  #Run simulation with RCR=0
  set.seed(140341)
  sim_out <- sim_lambda_alpha_o(RCR=0,theta=theta,mu=mu,sigma=sigma,
                              mu.alpha=mu.alpha,conc.alpha=conc.alpha,conc.err=conc.err,
                              tau_data=tau_data,N=N)
  V_iso_o <- sim_out$V_iso_o
  cost <- sim_out$cost_true
  tauQ <- sim_out$tauQ
  alpha <- sim_out$alpha
  lambda_U <- sim_out$lambda_U
  chi_model <- sim_out$chi_model
  chi.U <- lambda_U 
  compliance <- sim_out$compliance
  compliance <- fcase(
    compliance == "CC","Comply-constrained",
    compliance == "CU","Comply-unconstrained",
    compliance == "NC","Non-compliant"
  )
  #Run simulation with RCR.prime
  set.seed(140341)
  sim_out_prime <- sim_lambda_alpha_o(RCR=RCR.prime,theta=theta,mu=mu,sigma=sigma,
                                    mu.alpha=mu.alpha,conc.alpha=conc.alpha,conc.err=conc.err,
                                    tau_data=tau_data,N=N)
  cost_prime <- sim_out_prime$cost_true
  lambda_U_prime <- sim_out_prime$lambda_U
  chi_model_prime <- sim_out_prime$chi_model
  chi.U_prime <- lambda_U_prime 
  compliance_prime <- sim_out_prime$compliance
  compliance_prime <- fcase(
    compliance_prime == "CC","Comply-constrained",
    compliance_prime == "CU","Comply-unconstrained",
    compliance_prime == "NC","Non-compliant"
  )
  # Individual domestic carline change in cost:
  C_hat_j <- (cost_prime/cost)^(1-alpha)  #C_hat_j is now actually p_hat_j as they now take into account alpha>0
  #
  # Price index change of domestic carlines by country
  N.MEX <- length(C_hat_j[V_iso_o=="MEX"])
  N.CAN <- length(C_hat_j[V_iso_o=="CAN"])
  N.USA <- length(C_hat_j[V_iso_o=="USA"])
  P_hat.MEX <- (sum(C_hat_j[V_iso_o=="MEX"]^(1-eta))/N.MEX)^(1/(1-eta))
  P_hat.CAN <- (sum(C_hat_j[V_iso_o=="CAN"]^(1-eta))/N.CAN)^(1/(1-eta))
  P_hat.USA <- (sum(C_hat_j[V_iso_o=="USA"]^(1-eta))/N.USA)^(1/(1-eta))
  #
  # Price index 
 P_hat <-  ( share.CAN*P_hat.CAN^(eta-1) +
                          share.MEX*P_hat.MEX^(eta-1) +
                          share.USA*P_hat.USA^(eta-1) + share.F )^(1/(eta-1))
  # we won't use this price index, but too many ripple effects from deleting it here
  # s_hat_j : SALES change of each domestic carline 
  s_hat_j <- C_hat_j^(1-eta)
  #
  # q_hat_j : quantity change of each carline that complies domestic in the CF
  q_hat_j <- fcase(
    compliance_prime != "Non-compliant",C_hat_j^(-eta),
    compliance_prime == "Non-compliant",tauQ^(-eta)
  )
  #new elements of aggregation
  chi_hat_j <- chi_model_prime/chi_model
  L_hat_j <- chi_hat_j*q_hat_j
  LA_hat_j <- q_hat_j
  #
  return(data.table(RCR.prime=RCR.prime,C.hat=C_hat_j,  P_hat=P_hat, #P_hat.MEX=P_hat.MEX,P_hat.CAN=P_hat.CAN,P_hat.USA=P_hat.USA,
                    compliance = compliance, compliance_prime= compliance_prime,
                    alpha=sim_out$alpha,tau=sim_out$tau,delta=sim_out$delta, V_iso_o=V_iso_o,
                    s_hat=s_hat_j,q_hat=q_hat_j,chi_hat=chi_hat_j,L_hat=L_hat_j,LA_hat=LA_hat_j
                    ))
}
#
#function that aggregates  the simulation outputs  by country  ====
C.hat.Agg.base.prime_o <- function(RCR.base,RCR.prime){
  N=Num.obs*Nfactor
  #Run the simulation for the base (62.5 for NAFTA)
  DC.hat.base <- C.hat_alpha0_o(RCR.prime=RCR.base,theta=params$theta,mu=params$mu,sigma=params$sigma,conc.err=params$conc.err,
                              mu.alpha=params$alpha,conc.alpha=params$conc.alpha,tau_data=params$tau,N=N)
  
  #Run the simulation for RCR.prime
  DC.hat.prime <- C.hat_alpha0_o(RCR.prime=RCR.prime,theta=params$theta,mu=params$mu,sigma=params$sigma,conc.err=params$conc.err,
                               mu.alpha=params$alpha,conc.alpha=params$conc.alpha,tau_data=params$tau,N=N)
  
  #Creating a unique data.table for both experiences (note the compliances are both primes, under both scenarios)
  DC.hat <- data.table(RCR.base=RCR.base,RCR.prime=RCR.prime,
                       delta=DC.hat.base$delta,tau=DC.hat.base$tau, V_iso_o=DC.hat.base$V_iso_o,
                       compliance=DC.hat.base$compliance_prime,compliance_prime=DC.hat.prime$compliance_prime, #key line
                       P.hat.base=DC.hat.base$P_hat,P.hat.prime=DC.hat.prime$P_hat,
                       C.hat.base=DC.hat.base$C.hat,C.hat.prime=DC.hat.prime$C.hat,
                       q_hat.base=DC.hat.base$q_hat,q_hat.prime=DC.hat.prime$q_hat,
                       chi_hat.base=DC.hat.base$chi_hat,chi_hat.prime=DC.hat.prime$chi_hat,
                       L_hat.base=DC.hat.base$L_hat,L_hat.prime=DC.hat.prime$L_hat,
                       LA_hat.base=DC.hat.base$LA_hat,LA_hat.prime=DC.hat.prime$LA_hat
                       )
  #Collapse twice
  DC.hat.c <- DC.hat[,.(P_hat.base = first(P.hat.base),
                        P_hat.prime = first(P.hat.prime),
                        P_hat.D_i.base = (sum(C.hat.base^(1-eta))/(.N))^(1/(1-eta)),
                        P_hat.D_i.prime = (sum(C.hat.prime^(1-eta))/(.N))^(1/(1-eta)),
                        X_hat_i.base = mean(chi_hat.base), X_hat_i.prime = mean(chi_hat.prime),
                        L_hat_i.base = mean(L_hat.base), L_hat_i.prime = mean(L_hat.prime),
                        LA_hat_i.base = mean(LA_hat.base), LA_hat_i.prime = mean(LA_hat.prime),
                        share.obs = round(100*(.N/N),1),
                        meandelta = round(mean(delta),3), meantau = round(mean(tau),3)),
                     by=.(RCR.base,RCR.prime,compliance,compliance_prime,V_iso_o)] # key change here: origin specific
  #
  DC.hat.agg <- DC.hat[,.(P_hat.base = first(P.hat.base),
                          P_hat.prime = first(P.hat.prime), 
                          P_hat.D_i.base = (sum(C.hat.base^(1-eta))/(.N))^(1/(1-eta)),
                          P_hat.D_i.prime = (sum(C.hat.prime^(1-eta))/(.N))^(1/(1-eta)), 
                          X_hat_i.base = mean(chi_hat.base), X_hat_i.prime = mean(chi_hat.prime),
                          L_hat_i.base = mean(L_hat.base), L_hat_i.prime = mean(L_hat.prime),
                          LA_hat_i.base = mean(LA_hat.base), LA_hat_i.prime = mean(LA_hat.prime),
                          share.obs = round(100*(.N/N),1),
                          meandelta = round(mean(delta),3), meantau = round(mean(tau),3)), 
                       by=.(RCR.base,RCR.prime,V_iso_o)] # key change here: origin specific
  DC.hat.agg[,`:=` (compliance = "All",compliance_prime = "All")]
  DC.hat.c <- rbind(DC.hat.c,DC.hat.agg)
  #
  #Take ratio
  # 
  DC.hat.c <- DC.hat.c[, `:=`(P_hat.D_i = P_hat.D_i.prime/P_hat.D_i.base,
                              P_hat = P_hat.prime/P_hat.base, 
                              X_hat.D_i = X_hat_i.prime/X_hat_i.base,
                              L_hat.D_i = L_hat_i.prime/L_hat_i.base,
                              LA_hat.D_i = LA_hat_i.prime/LA_hat_i.base
                              )]
  
  DC.hat.c <- DC.hat.c[, s_hat.D_i := (P_hat.D_i)^(1-eta)] #sales value change
  
  #following theory, back out what zeta0 would be without a ROO.
#if(RCR.prime==0.625)  zeta0 <<- zeta*DC.hat.c[compliance == "All" & compliance_prime == "All" & V_iso_o =="USA" ]$X_hat.D_i # key change here; only USA
if(RCR.prime==0.625)  zeta0 <<- zeta*DC.hat.c[compliance == "All" & compliance_prime == "All" & V_iso_o =="USA", L_hat.D_i/LA_hat.D_i ]

DC.hat.c <- DC.hat.c[, LT_hat.D_i := (zeta0*LA_hat.D_i + L_hat.D_i)/(zeta0+1)]
  
  #Subtract one from each hat variable, round 
round.prec <- 5
DC.hat.c <- DC.hat.c[, `:=`(P_hat.D_i = round(100*(P_hat.D_i-1),round.prec), 
                              s_hat.D_i = round(100*(s_hat.D_i-1),round.prec),
                              X_hat.D_i = round(100*(X_hat.D_i-1),round.prec),
                              L_hat.D_i = round(100*(L_hat.D_i-1),round.prec),
                              LT_hat.D_i = round(100*(LT_hat.D_i-1),round.prec)
  )]
  #Order 
  DC.hat.c[, order :=  fcase(
    compliance == "Comply-constrained" & compliance_prime == "Non-compliant",1,
    compliance == "Comply-unconstrained" & compliance_prime == "Non-compliant",2,
    compliance == "Comply-constrained" & compliance_prime == "Comply-constrained",3,
    compliance == "Comply-unconstrained" & compliance_prime == "Comply-constrained",4,
    compliance == "Non-compliant" & compliance_prime == "Non-compliant",5,
    compliance == "Comply-unconstrained" & compliance_prime == "Comply-unconstrained",6,
    #Those last two below are special to the case of returning to 0
    compliance == "Non-compliant" & compliance_prime == "Comply-unconstrained",1,
    compliance == "Comply-constrained" & compliance_prime == "Comply-unconstrained",2,
    compliance == "All" & compliance_prime == "All",7
  )]
  setorder(DC.hat.c,RCR.prime,order)
  DC.hat.c[,order := NULL]
  #            
  return(DC.hat.c[,.(RCR.prime,V_iso_o,compliance,compliance_prime,share.obs,P_hat.D_i,s_hat.D_i,X_hat.D_i,L_hat.D_i,LT_hat.D_i,LA_hat.D_i)])
}


# EHA with relocation ====

# DRF version of crucial function ====

C.hat_alpha0_DRF <- function(RCR.prime,theta,mu,sigma,tau_data,mu.alpha,conc.alpha,conc.err,N,omegaR,omegaF,kappa) {
  #Run simulation with RCR=0
  set.seed(140341)
  sim_out <- sim_lambda_alpha_DRF(RCR=0,theta=theta,mu=mu,sigma=sigma,
                              mu.alpha=mu.alpha,conc.alpha=conc.alpha,conc.err=conc.err,
                              tau_data=tau_data,N=N,
                              omegaR=omegaR,omegaF=omegaF,kappa=kappa)
  V_iso_o <- sim_out$V_iso_o
  chosen_R <- sim_out$chosen_R
  cost <- sim_out$cost_true
  tauD_Q <- sim_out$tauD_Q
  tauR_Q <- sim_out$tauR_Q
  tauF_Q <- sim_out$tauF_Q
  alpha <- sim_out$alpha
  lambda_U <- sim_out$lambda_U
  chi_model <- sim_out$chi_model
  chi.U <- lambda_U 
  compliance <- sim_out$choice
  #
  #Run simulation with RCR.prime
  set.seed(140341)
  sim_out_prime <- sim_lambda_alpha_DRF(RCR=RCR.prime,theta=theta,mu=mu,sigma=sigma,
                                    mu.alpha=mu.alpha,conc.alpha=conc.alpha,conc.err=conc.err,
                                    tau_data=tau_data,N=N,
                                    omegaR=omegaR,omegaF=omegaF,kappa=kappa)
  chosen_R_prime <- sim_out$chosen_R
  cost_prime <- sim_out_prime$cost_true
  lambda_U_prime <- sim_out_prime$lambda_U
  chi_model_prime <- sim_out_prime$chi_model
  chi.U_prime <- lambda_U_prime 
  compliance_prime <- sim_out_prime$choice
  #
  # Individual domestic carline change in cost:
  C_hat_j <- (cost_prime/cost)^(1-alpha)  #C_hat_j is now actually p_hat_j as they now take into account alpha>0
  #
  # count of carlines by assembly country
  N.MEX <- length(C_hat_j[V_iso_o=="MEX"])
  N.CAN <- length(C_hat_j[V_iso_o=="CAN"])
  N.USA <- length(C_hat_j[V_iso_o=="USA"])
  #  Price index change of domestic carlines, by assembly location
  P_hat.MEX <- (sum(C_hat_j[V_iso_o=="MEX"]^(1-eta))/N.MEX)^(1/(1-eta))
  P_hat.CAN <- (sum(C_hat_j[V_iso_o=="CAN"]^(1-eta))/N.CAN)^(1/(1-eta))
  P_hat.USA <- (sum(C_hat_j[V_iso_o=="USA"]^(1-eta))/N.USA)^(1/(1-eta))
  #
  # Price index combining the 3 Nafta origins and foreign assembly which has no price change
    P_hat <-  ( share.CAN*P_hat.CAN^(eta-1) +
                share.MEX*P_hat.MEX^(eta-1) +
                share.USA*P_hat.USA^(eta-1) + share.F )^(1/(eta-1))
  #
  # s_hat_j : sales value  change of each domestic carline
  s_hat_j <- (C_hat_j)^(1-eta)
  #
  # q_hat_j : quantity change of each domestic carline
  #q_hat_j <- s_hat_j/C_hat_j
  #
  q_hat_j <- fcase(
    compliance_prime  %in% c("CDU","CDC"), C_hat_j^(-eta),
    compliance_prime == "NCD",tauD_Q^(-eta),
    compliance_prime == "NCR",tauR_Q^(-eta),
    compliance_prime == "NCF",tauF_Q^(-eta)
  )
  #
  #new elements of aggregation
  chi_hat_j <- chi_model_prime/chi_model
  L_hat_j <- chi_hat_j*q_hat_j
  LA_hat_j <- q_hat_j
  #
  return(data.table(RCR.prime=RCR.prime,C.hat=C_hat_j, P_hat=P_hat, 
                    compliance = compliance, compliance_prime= compliance_prime,
                    alpha=sim_out$alpha,tau=sim_out$tau,delta=sim_out$delta,V_iso_o=V_iso_o,
                    chosen_R=chosen_R,chosen_R_prime=chosen_R_prime,
                    s_hat=s_hat_j,q_hat=q_hat_j,chi_hat=chi_hat_j,L_hat=L_hat_j,LA_hat=LA_hat_j
  ))
}


# DRF version of function that aggregates  the simulation outputs ====
C.hat.Agg.base.prime_DRF <- function(RCR.base,RCR.prime){
  N=Num.obs*Nfactor
  #Run the simulation for the base (62.5 for NAFTA)
  DC.hat.base <- C.hat_alpha0_DRF(RCR.prime=RCR.base,theta=params$theta,mu=params$mu,sigma=params$sigma,conc.err=params$conc.err,
                              mu.alpha=params$alpha,conc.alpha=params$conc.alpha,tau_data=params$taumat,N=N,
                              omegaR=params$omegaR,omegaF=params$omegaF,kappa=params$kappa)
  
  #Run the simulation for RCR.prime
  DC.hat.prime <- C.hat_alpha0_DRF(RCR.prime=RCR.prime,theta=params$theta,mu=params$mu,sigma=params$sigma,conc.err=params$conc.err,
                               mu.alpha=params$alpha,conc.alpha=params$conc.alpha,tau_data=params$taumat,N=N,
                               omegaR=params$omegaR,omegaF=params$omegaF,kappa=params$kappa)
  
  #Creating a unique data.table for both experiences (note the compliances are both primes, under both scenarios)
  DC.hat <- data.table(RCR.base=RCR.base,RCR.prime=RCR.prime,
                       delta=DC.hat.base$delta,V_iso_o = DC.hat.base$V_iso_o, 
                       chosen_R = DC.hat.base$chosen_R_prime, chosen_R_prime = DC.hat.prime$chosen_R_prime, # both prime, relevant when comparing 62.5 to 75 for instance
                       compliance=DC.hat.base$compliance_prime,compliance_prime=DC.hat.prime$compliance_prime, #key line
                       P.hat.base=DC.hat.base$P_hat,P.hat.prime=DC.hat.prime$P_hat,
                       C.hat.base=DC.hat.base$C.hat,C.hat.prime=DC.hat.prime$C.hat,
                       q_hat.base=DC.hat.base$q_hat,q_hat.prime=DC.hat.prime$q_hat,
                       chi_hat.base=DC.hat.base$chi_hat,chi_hat.prime=DC.hat.prime$chi_hat,
                       L_hat.base=DC.hat.base$L_hat,L_hat.prime=DC.hat.prime$L_hat,
                       LA_hat.base=DC.hat.base$LA_hat,LA_hat.prime=DC.hat.prime$LA_hat
  )
  #Chosen origin at base
  DC.hat[, ell := fcase(
    compliance %in% c("CDU","CDC","NCD"), V_iso_o,
    compliance == "NCR",chosen_R,
    compliance == "NCF","ROW"
  )]
  #Specify ell_prime: chosen origin at RCR.prime
  DC.hat[, ell_prime := fcase(
    compliance_prime %in% c("CDU","CDC","NCD"), V_iso_o,
    compliance_prime == "NCR",chosen_R_prime,
    compliance_prime == "NCF","ROW"
  )]
  #Keep only carlines that chose NAFTA at RCR = 0 
  DC.hat <- DC.hat[ell!="ROW"]
  # ell_prime still includes ROW (for those who leave in the CF)
  #
  # Now start aggregating
  # First, compute X_hat and L_hat for each V_iso_o
  DC.hat.agg <- DC.hat[,.(P_hat.base = first(P.hat.base),
                          P_hat.prime = first(P.hat.prime),
                          P_hat.D_i.base = (sum(C.hat.base^(1-eta))/(.N))^(1/(1-eta)),
                          P_hat.D_i.prime = (sum(C.hat.prime^(1-eta))/(.N))^(1/(1-eta)),
                          X_hat_i.base = mean(chi_hat.base), X_hat_i.prime = mean(chi_hat.prime),
                          L_hat_i.base = mean(L_hat.base), L_hat_i.prime = mean(L_hat.prime)
                          ),
                          by=.(RCR.base,RCR.prime,ell)]
  #Ratios
  DC.hat.agg[, `:=`(P_hat.D_i = P_hat.D_i.prime/P_hat.D_i.base,
                              P_hat = P_hat.prime/P_hat.base, 
                              X_hat.D_i = X_hat_i.prime/X_hat_i.base,
                              L_hat.D_i = L_hat_i.prime/L_hat_i.base
                    )]
  #Compute s_hat
  DC.hat.agg[, s_hat.D_i := (P_hat.D_i)^(1-eta)] # value of sales change
  #Select variables for next step (merge with assembly material)
  DC.hat.agg <- DC.hat.agg[,.(RCR.base,RCR.prime,ell,s_hat.D_i,P_hat.D_i,X_hat.D_i,L_hat.D_i)]
  #
  # Second, collapse the assembly needed elements by original and final locations
  DC.hat.c <- DC.hat[,.(P_hat.base = first(P.hat.base),
                        P_hat.prime = first(P.hat.prime), 
                        P_hat.D_i.base = (sum(C.hat.base^(1-eta))/(.N))^(1/(1-eta)),
                        P_hat.D_i.prime = (sum(C.hat.prime^(1-eta))/(.N))^(1/(1-eta)), 
                        N_ell_ell_prime = .N # count by original and final location
                        ), 
                     by=.(RCR.base,RCR.prime,ell,ell_prime)]

  #
  #Take ratio
  DC.hat.c[, `:=`(P_hat.D_i = P_hat.D_i.prime/P_hat.D_i.base,
                  P_hat = P_hat.prime/P_hat.base)]
  #Compute LA_hat
  DC.hat.c[, s_hat.D_i := (P_hat.D_i)^(1-eta)] # value of sales change
  DC.hat.c[, q_hat.D_i := (P_hat.D_i)^(-eta)] # quantity change
  DC.hat.c[, LA_hat.D_i := q_hat.D_i] # assembly employment change
  #Merge with LA_grid, to get LA0
  DC.hat.c <- merge_stata(DC.hat.c,LA_grid,by=c("ell","ell_prime"))
  DC.hat.c <- DC.hat.c[stata_merge!=2] # drop MEX-->CAN (does not happen in the simulation)
  DC.hat.c[, N_shr := N_ell_ell_prime/sum(N_ell_ell_prime), by=ell]  # see EHA_simulation.pdf page 6-7
  DC.hat.c <- DC.hat.c[stata_merge==3] # drop ROW, and MEX-->CAN
  DC.hat.c[, stata_merge := NULL]
  DC.hat.c[, rat_term := fcase(
    ell==ell_prime,N_shr,
    ell!=ell_prime,N_shr*LA_rat0 # new version of equation EHA_simulation.pdf, page7
  )]
  #Collapse to obtain D-level LA (ell_prime, the final location at RCR.prime, see EHA_simulation.pdf, page7)
  DC.hat.ell_prime <- DC.hat.c[,.(LA_hat.D_i = sum(rat_term*LA_hat.D_i)), 
  by=.(RCR.base,RCR.prime,ell_prime)]
  #
  #Merge back with DC.hat.agg (result is a ell dataset)
  DC.hat.merged <- merge(DC.hat.agg,DC.hat.ell_prime,by.x=c("RCR.base","RCR.prime","ell"), by.y=c("RCR.base","RCR.prime","ell_prime"))
  #
  #Compute total employment change (zeta0 from global env, computed previously)
  DC.hat.merged[, LT_hat.D_i := (LA_hat.D_i*zeta0 + L_hat.D_i)/(zeta0+1)]
  #
  #Last computation of price changes only for carlines that end up being CDC in the prime situation
  #
  #DC.hat[compliance %in% c("CDU","CDC"), compliance := "CDU+CDC"]
  DC.hat.CDC <- DC.hat[,.(P_hat.D_i.base = (sum(C.hat.base^(1-eta))/(.N))^(1/(1-eta)),
                          P_hat.D_i.prime = (sum(C.hat.prime^(1-eta))/(.N))^(1/(1-eta)) 
                            ), by=.(RCR.base,RCR.prime,ell,compliance_prime)]
  #DC.hat.CDC <- DC.hat.CDC[compliance == "CDU+CDC" & compliance_prime =="CDC"] #keep ALL the compliers in the initial  who are constrained in the prime
  DC.hat.CDC <- DC.hat.CDC[compliance_prime =="CDC"] #keep ALL the compliers in the initial  who are constrained in the prime
  DC.hat.CDC[, `:=`(P_hat.D_i_CDC = P_hat.D_i.prime/P_hat.D_i.base)]
  DC.hat.CDC <- DC.hat.CDC[,.(RCR.base,RCR.prime,ell,P_hat.D_i_CDC)]
  #Merge back with main file (result is a ell dataset)
  DC.hat.merged <- merge_stata(DC.hat.merged,DC.hat.CDC,by=c("RCR.base","RCR.prime","ell"))
  DC.hat.merged <- DC.hat.merged[stata_merge!=2] # drop if ever
  DC.hat.merged[, stata_merge := NULL]
  #
  #Subtract one from each hat variable, round and save
  round.prec <- 5
  DC.hat.merged[, `:=`(       P_hat.D_i_CDC = round(100*(P_hat.D_i_CDC-1),round.prec), #constrained
                              P_hat.D_i = round(100*(P_hat.D_i-1),round.prec), #all
                              s_hat.D_i = round(100*(s_hat.D_i-1),round.prec),
                              X_hat.D_i = round(100*(X_hat.D_i-1),round.prec),
                              L_hat.D_i = round(100*(L_hat.D_i-1),round.prec),
                              LA_hat.D_i = round(100*(LA_hat.D_i-1),round.prec),
                              LT_hat.D_i = round(100*(LT_hat.D_i-1),round.prec)
  )]
#
    return(DC.hat.merged[,.(RCR.prime,V_iso_o=ell,P_hat.D_i_CDC,P_hat.D_i,s_hat.D_i,X_hat.D_i,L_hat.D_i,LA_hat.D_i,LT_hat.D_i)])
}

# DRF version of function that aggregates  the simulation outputs to the
# North America level====
C.hat.AggNA.base.prime_DRF <- function(RCR.base,RCR.prime){
  N=Num.obs*Nfactor
  #Run the simulation for the base (62.5 for NAFTA)
  DC.hat.base <- C.hat_alpha0_DRF(RCR.prime=RCR.base,theta=params$theta,mu=params$mu,sigma=params$sigma,conc.err=params$conc.err,
                                  mu.alpha=params$alpha,conc.alpha=params$conc.alpha,tau_data=params$taumat,N=N,
                                  omegaR=params$omegaR,omegaF=params$omegaF,kappa=params$kappa)
  
  #Run the simulation for RCR.prime
  DC.hat.prime <- C.hat_alpha0_DRF(RCR.prime=RCR.prime,theta=params$theta,mu=params$mu,sigma=params$sigma,conc.err=params$conc.err,
                                   mu.alpha=params$alpha,conc.alpha=params$conc.alpha,tau_data=params$taumat,N=N,
                                   omegaR=params$omegaR,omegaF=params$omegaF,kappa=params$kappa)
  
  #Creating a unique data.table for both experiences (note the compliances are both primes, under both scenarios)
  DC.hat <- data.table(RCR.base=RCR.base,RCR.prime=RCR.prime,
                       delta=DC.hat.base$delta,V_iso_o = DC.hat.base$V_iso_o, 
                       chosen_R = DC.hat.base$chosen_R_prime, chosen_R_prime = DC.hat.prime$chosen_R_prime, # both prime, relevant when comparing 62.5 to 75 for instance
                       compliance=DC.hat.base$compliance_prime,compliance_prime=DC.hat.prime$compliance_prime, #key line
                       P.hat.base=DC.hat.base$P_hat,P.hat.prime=DC.hat.prime$P_hat,
                       C.hat.base=DC.hat.base$C.hat,C.hat.prime=DC.hat.prime$C.hat,
                       q_hat.base=DC.hat.base$q_hat,q_hat.prime=DC.hat.prime$q_hat,
                       chi_hat.base=DC.hat.base$chi_hat,chi_hat.prime=DC.hat.prime$chi_hat,
                       L_hat.base=DC.hat.base$L_hat,L_hat.prime=DC.hat.prime$L_hat,
                       LA_hat.base=DC.hat.base$LA_hat,LA_hat.prime=DC.hat.prime$LA_hat
  )
  #Chosen origin at base
  #new: aggregate to {NA or ROW}
  DC.hat[, ell := fcase(
    compliance %in% c("CDU","CDC","NCD"), "NA",
    compliance == "NCR","NA",
    compliance == "NCF","ROW"
  )]
  #Specify ell_prime: chosen origin at RCR.prime
  DC.hat[, ell_prime := fcase(
    compliance_prime %in% c("CDU","CDC","NCD"), "NA",
    compliance_prime == "NCR","NA",
    compliance_prime == "NCF","ROW"
  )]
  #Keep only carlines that chose NAFTA at RCR = 0 
  DC.hat <- DC.hat[ell!="ROW"]
  # ell_prime still includes ROW (for those who leave in the CF)
  #
  # Now start aggregating
  # First, compute X_hat and L_hat for each V_iso_o
  DC.hat.agg <- DC.hat[,.(P_hat.base = first(P.hat.base),
                          P_hat.prime = first(P.hat.prime),
                          P_hat.D_i.base = (sum(C.hat.base^(1-eta))/(.N))^(1/(1-eta)),
                          P_hat.D_i.prime = (sum(C.hat.prime^(1-eta))/(.N))^(1/(1-eta)),
                          X_hat_i.base = mean(chi_hat.base), X_hat_i.prime = mean(chi_hat.prime),
                          L_hat_i.base = mean(L_hat.base), L_hat_i.prime = mean(L_hat.prime)
  ),
  by=.(RCR.base,RCR.prime,ell)]
  #Ratios
  DC.hat.agg[, `:=`(P_hat.D_i = P_hat.D_i.prime/P_hat.D_i.base,
                    P_hat = P_hat.prime/P_hat.base, 
                    X_hat.D_i = X_hat_i.prime/X_hat_i.base,
                    L_hat.D_i = L_hat_i.prime/L_hat_i.base
  )]
  #Compute s_hat
  DC.hat.agg[, s_hat.D_i := (P_hat.D_i)^(1-eta)] # value of sales change
  #Select variables for next step (merge with assembly material)
  DC.hat.agg <- DC.hat.agg[,.(RCR.base,RCR.prime,ell,s_hat.D_i,P_hat.D_i,X_hat.D_i,L_hat.D_i)]
  #
  # Second, collapse the assembly needed elements by original and final locations
  DC.hat.c <- DC.hat[,.(P_hat.base = first(P.hat.base),
                        P_hat.prime = first(P.hat.prime), 
                        P_hat.D_i.base = (sum(C.hat.base^(1-eta))/(.N))^(1/(1-eta)),
                        P_hat.D_i.prime = (sum(C.hat.prime^(1-eta))/(.N))^(1/(1-eta)), 
                        N_ell_ell_prime = .N # count by original and final location
  ), 
  by=.(RCR.base,RCR.prime,ell,ell_prime)]
  
  #
  #Take ratio
  DC.hat.c[, `:=`(P_hat.D_i = P_hat.D_i.prime/P_hat.D_i.base,
                  P_hat = P_hat.prime/P_hat.base)]
  #Compute LA_hat
  DC.hat.c[, s_hat.D_i := (P_hat.D_i)^(1-eta)] # value of sales change
  DC.hat.c[, q_hat.D_i := (P_hat.D_i)^(-eta)] # quantity change
  DC.hat.c[, LA_hat.D_i := q_hat.D_i] # assembly employment change
  DC.hat.c[, N_shr := N_ell_ell_prime/sum(N_ell_ell_prime), by=ell]  
  DC.hat.c <- DC.hat.c[ell_prime != "ROW"] # drop ROW
  DC.hat.c[, rat_term := N_shr] #dropped the fcase() involving LA_rat0  because that is country level
  #simplified from country-level on line 492, rat_term is the "stayer share"
  # retain ell_prime, the final location at RCR.prime,  which is always "NA" so no more collapse here
  DC.hat.ell_prime <- DC.hat.c[,.(LA_hat.D_i = rat_term*LA_hat.D_i,RCR.base,RCR.prime,ell_prime)]
  #
  #Merge back with DC.hat.agg (result is a ell dataset)
  DC.hat.merged <- merge(DC.hat.agg,DC.hat.ell_prime,by.x=c("RCR.base","RCR.prime","ell"), by.y=c("RCR.base","RCR.prime","ell_prime"))
  #
  #Compute total employment change (zeta0 from global env, computed previously)
  DC.hat.merged[, LT_hat.D_i := (LA_hat.D_i*zeta0 + L_hat.D_i)/(zeta0+1)]
  #
  #Last computation of price changes only for carlines that end up being CDC in the prime situation
  #
  #DC.hat[compliance %in% c("CDU","CDC"), compliance := "CDU+CDC"]
  DC.hat.CDC <- DC.hat[,.(P_hat.D_i.base = (sum(C.hat.base^(1-eta))/(.N))^(1/(1-eta)),
                          P_hat.D_i.prime = (sum(C.hat.prime^(1-eta))/(.N))^(1/(1-eta)) 
  ), by=.(RCR.base,RCR.prime,ell,compliance_prime)]
  #DC.hat.CDC <- DC.hat.CDC[compliance == "CDU+CDC" & compliance_prime =="CDC"] #keep ALL the compliers in the initial  who are constrained in the prime
  DC.hat.CDC <- DC.hat.CDC[compliance_prime =="CDC"] #keep ALL the compliers in the initial  who are constrained in the prime
  DC.hat.CDC[, `:=`(P_hat.D_i_CDC = P_hat.D_i.prime/P_hat.D_i.base)]
  DC.hat.CDC <- DC.hat.CDC[,.(RCR.base,RCR.prime,ell,P_hat.D_i_CDC)]
  #Merge back with main file (result is a ell dataset)
  DC.hat.merged <- merge_stata(DC.hat.merged,DC.hat.CDC,by=c("RCR.base","RCR.prime","ell"))
  DC.hat.merged <- DC.hat.merged[stata_merge!=2] # drop if ever
  DC.hat.merged[, stata_merge := NULL]
  #
  #Subtract one from each hat variable, round and save
  round.prec <- 5
  DC.hat.merged[, `:=`(       P_hat.D_i_CDC = round(100*(P_hat.D_i_CDC-1),round.prec), #constrained
                              P_hat.D_i = round(100*(P_hat.D_i-1),round.prec), #all
                              s_hat.D_i = round(100*(s_hat.D_i-1),round.prec),
                              X_hat.D_i = round(100*(X_hat.D_i-1),round.prec),
                              L_hat.D_i = round(100*(L_hat.D_i-1),round.prec),
                              LA_hat.D_i = round(100*(LA_hat.D_i-1),round.prec),
                              LT_hat.D_i = round(100*(LT_hat.D_i-1),round.prec)
  )]
  #
  return(DC.hat.merged[,.(RCR.prime,V_iso_o=ell,P_hat.D_i_CDC,P_hat.D_i,s_hat.D_i,X_hat.D_i,L_hat.D_i,LA_hat.D_i,LT_hat.D_i)])
}

