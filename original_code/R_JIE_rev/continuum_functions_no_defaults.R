lambda_C <- function(x,delta,theta) {
 y <- ((1-x)/x)^((1+theta)/theta)
 z <- 1+y*delta
 return(1/z)
}
#
lambda_min <- function(delta,theta) 1/(1+delta^(-theta))
#
chi_U <- lambda_min # new name
#
#analytic form for unconstrained cost function.
C_min <- function(delta,theta) { 
  index <- (1+delta^(-theta))^(-1/theta)
  return(index)
}
#
C_comply <- function(R,delta,theta) {
  k <- (1+theta)/theta
  R^k +delta*(1-R)^k
}
#
C.tilde <- function(R,delta,theta) {
  cons <- R > lambda_min(delta=delta,theta = theta) 
  y <- cons * C_comply(R,delta=delta,theta = theta)/C_min(delta=delta,theta = theta) +1-cons
  return(y)
}
# limit of delta star as lambda_R -> 1 (in paper lambda_R -> chi_R, delta_max -> \bar{\delta}(\tau))
delta_max <- function(tau,theta) {
  (tau^(theta) -1)^(-1/theta)
}
# R = lambda_R
#
delta.star <- function(R,tau,theta) {
  ufn <- function(x) C.tilde(R=R,delta=x,theta=theta)-tau
  dmax <- delta_max(tau=tau,theta = theta)
  uniroot(ufn,interval=c(0.00001,dmax))$root
}
#
delta.circ <- function(R,theta) {
  (R^(-1) -1)^(-1/theta)
}
#
#functions for delta heterogeneity only ====
CDF <- function(x,mu,sigma) plnorm(x,meanlog=mu,sdlog=sigma)
#PDF <- function(x,mu=0,sigma=0.2) dnorm(log(x),mean=mu,sd=sigma) normal
PDF <- function(x,mu,sigma) dlnorm(x,meanlog=mu,sdlog=sigma) #lognormal pdf of delta
integrand <- function(x,mu,sigma,theta) lambda_min(delta=x,theta=theta)*PDF(x,mu=mu,sigma=sigma)
integral.low <- function(x,mu,sigma,theta) integrate(integrand,lower=1e-5,upper=x,mu=mu,sigma=sigma,theta=theta)$val  # log normal so start near zero
integral.high <- function(x,mu,sigma,theta,delta_upper=Inf) integrate(integrand,lower=x,upper=delta_upper,mu=mu,sigma=sigma,theta=theta)$val
integral.all <-  function(x,mu,sigma,theta) integrate(integrand,lower=1e-5,upper=Inf,mu=mu,sigma=sigma,theta=theta)$val
# X(\chi_R) in paper is called lambda.bar here
#
lambda.bar <- function(R,tau,theta,mu,sigma) {
dc <- delta.circ(R,theta=theta)
ds <- delta.star(R,theta=theta,tau=tau)
a <- R*(CDF(dc,mu=mu,sigma=sigma)-CDF(ds,mu=mu,sigma=sigma))
b <- integral.low(ds,mu=mu,sigma=sigma,theta=theta)
c <- integral.high(dc,mu=mu,sigma=sigma,theta=theta)
  return(a+b+c)
}
#
lambda.bar.noAC <- function(R,tau,theta,mu,sigma) {
  dm <- delta_max(tau=tau,theta=theta)
  dc <- min(delta.circ(R,theta=theta),dm)
  ds <- delta.star(R,theta=theta,tau=tau)
  a <- R*(CDF(dc,mu=mu,sigma=sigma)-CDF(ds,mu=mu,sigma=sigma))
  b <- integral.low(ds,mu=mu,sigma=sigma,theta=theta)
  c <- integral.high(dc,mu=mu,sigma=sigma,theta=theta,delta_upper=dm)
  return((a+b+c)/CDF(dm,mu=mu,sigma=sigma))
}
#
# functions for double integration ====
chifun <- function(delta,tau,R,theta) {
  comply.con <- C.tilde(R=R,delta=delta,theta=theta)<tau &  chi_U(delta=delta,theta=theta)<R
return(R*comply.con +chi_U(delta=delta,theta=theta)*(1-comply.con))
}
#tests
# delta_seq = seq(from=0.5,to=2,by=0.01)
# chi =sapply(delta_seq,chifun,R=0.625,tau=1.025,theta=4)
# plot(delta_seq,chi,type="l")
# abline(h=0.625)
#R_seq = seq(from=0,to=1,by=0.01)
#chi =sapply(R_seq,chifun,delta =1,tau=1.1,theta=3)
#plot(R_seq,chi,type="l")
# function to simulation observed cost shares (actually fractions of costs recall lambda?)
X_inner <- function(tau,R,theta,mu,sigma) integrate(function(x) chifun(delta=x,R=R,tau=tau,theta=theta)*PDF(x,mu=mu,sigma=sigma),lower=1e-5,upper=Inf)$val
integrandX <- function(x,R,tau,theta,mu,sigma) chifun(delta=x,R=R,tau=tau,theta=theta)*PDF(x,mu=mu,sigma=sigma)
# X_tau_old <- function(tau,R) {
#     integrate(function(x) chifun(delta=x,R=R,tau=tau,theta=theta)*PDF(x),lower=1e-5,upper=Inf)$val
#  }
X_outer <-  function(tau,R,delta,theta,mu,sigma,a=a,b=b) integrate(function(x) X_inner(tau=x,R=R,theta=theta,mu=mu,sigma=sigma)*PDF_tau(x,a=a,b=b),lower=tau_low,upper=tau_high)$val
#
complycon <-  function(delta,tau,R,theta) {
   C.tilde(R=R,delta=delta,theta=theta)<tau &  chi_U(delta=delta,theta=theta)<R
}
#
mix_sim_v1 <- function(theta,mu,sig,chi_R,tau,mu.err=0,sig.err=0,N) {
  delta = rlnorm(N,mu,sig)
  chi_U = 1/(1+delta^(-theta))
  delta_circ = delta.circ(R=chi_R,theta=theta)
  delta_star = delta.star(R=chi_R,tau=tau,theta=theta)
  complycon <- delta > delta_star & delta < delta_circ # logical for compliers-constrained
  #observed cost shares = rule + comply error
  comply_err  = rnorm(N,mu.err,sig.err)  # normal error
  chi_observed = chi_U*(complycon==FALSE) + (chi_R+comply_err)*(complycon==TRUE)
  return(list(chi_observed=chi_observed,chi_U=chi_U,comply_err=comply_err,comply_frac = mean(complycon),delta_star=delta_star,delta_circ=delta_circ))
}
# revision
mix_sim <- function(theta,mu,sig,chi_R,tau,mu.err,sig.err,N) {
  delta = rlnorm(N,mu,sig)
  chi_U = 1/(1+delta^(-theta))
  delta_circ = delta.circ(R=chi_R,theta=theta)
  delta_star = delta.star(R=chi_R,tau=tau,theta=theta)
  complycon <- delta > delta_star & delta < delta_circ # logical for compliers-constrained
    chi_model = chi_U*(complycon==FALSE) + chi_R*(complycon==TRUE)
  #  lambda_model = lambda_U*(complycon==FALSE) + lambda_R*(complycon==TRUE)
    #observed cost shares = rule + comply error
    #err  = rnorm(N,mu.err,sig.err)  # normal error
    err  = rnorm(N,0,sig.err)  # normal error
     # chi_observed = chi_model +err
    chi_observed = chi_U*(complycon==FALSE)  + (chi_R+mu.err)*(complycon==TRUE) +err
    return(list(chi_observed=chi_observed,chi_U=chi_U,chi_model=chi_model,comply_frac = mean(complycon),delta_star=delta_star,delta_circ=delta_circ))
}
# unused
mix_sim_het_tau_KH <- function(theta,mu,sig,chi_R,tar_mfn,mu.err,sig.err,N) {
  delta = rlnorm(N,mu,sig)
  chi_U = 1/(1+delta^(-theta))
  delta_circ = delta.circ(R=chi_R,theta=theta)
  s = runif(N)
  tau.vec = 1+tar_mfn*s
  delta_star = sapply(tau.vec,delta.star,R=chi_R,theta=theta)
  complycon <- delta > delta_star & delta < delta_circ # logical for compliers-constrained
  chi_model = chi_U*(complycon==FALSE) + chi_R*(complycon==TRUE)
  #observed cost shares = rule + comply error
  #err  = rnorm(N,mu.err,sig.err)  # normal error
  err  = rnorm(N,0,sig.err)  # normal error
  # chi_observed = chi_model +err
  chi_observed = chi_U*(complycon==FALSE)  + (chi_R+mu.err)*(complycon==TRUE) +err
  return(list(chi_observed=chi_observed,chi_U=chi_U,chi_model=chi_model,comply_frac = mean(complycon),delta_star=delta_star))
}
# no delta star or delta circ
mix_sim_het_tau <- function(theta,mu,sig,chi_R,tar_mfn,mu.err,sig.err,N) {
  delta = rlnorm(N,mu,sig)
  chi_U = 1/(1+delta^(-theta))
  s = runif(N)
  tau.vec = 1+tar_mfn*s
  C_tilde = sapply(delta,C.tilde,R=chi_R,theta=theta)
  complycon <- C_tilde < tau.vec & chi_U<chi_R # logical for compliers-constrained
  chi_model = chi_U*(complycon==FALSE) + chi_R*(complycon==TRUE)
  #observed cost shares = rule + comply error
  #err  = rnorm(N,mu.err,sig.err)  # normal error
  err  = rnorm(N,0,sig.err)  # normal error
  # chi_observed = chi_model +err
  chi_observed = chi_U*(complycon==FALSE)  + (chi_R+mu.err)*(complycon==TRUE) +err
  return(list(chi_observed=chi_observed,chi_U=chi_U,chi_model=chi_model,comply_frac = mean(complycon)))
}
#sample from the tau_index empirical distribution. 
#(replace s= runif(N) with tau_vec <- sample(...)
mix_sim_het_tau_sample <- function(theta,mu,sig,chi_R,tau_index_data,mu.err,sig.err,N) {
  delta = rlnorm(N,mu,sig)
  chi_U = 1/(1+delta^(-theta))
  tau.vec = sample(tau_index_data,N,replace=TRUE)
  C_tilde = sapply(delta,C.tilde,R=chi_R,theta=theta)
  complycon <- C_tilde < tau.vec & chi_U<chi_R # logical for compliers-constrained
  chi_model = chi_U*(complycon==FALSE) + chi_R*(complycon==TRUE)
  #observed cost shares = rule + comply error
  #err  = rnorm(N,mu.err,sig.err)  # normal error
  err  = rnorm(N,0,sig.err)  # normal error
  # chi_observed = chi_model +err
  chi_observed = chi_U*(complycon==FALSE)  + (chi_R+mu.err)*(complycon==TRUE) +err
  return(list(chi_observed=chi_observed,chi_U=chi_U,chi_model=chi_model,comply_frac = mean(complycon)))
}

lambda.bar.tau_sample <- function(chi_R,theta,mu,sig,tau_index_data,N) {
  delta = rlnorm(N,mu,sig)
  chi_U = 1/(1+delta^(-theta))
  tau.vec = sample(tau_index_data,N,replace=TRUE)
  C_tilde = sapply(delta,C.tilde,R=chi_R,theta=theta)
  complycon <- C_tilde < tau.vec & chi_U<chi_R # logical for compliers-constrained
  chi_model = chi_U*(complycon==FALSE) + chi_R*(complycon==TRUE)
  return(mean(chi_model))
}

frac_U_beta_tau <- function(theta,mu,sig,beta.a,beta.b,tar_mfn,chi_R,N) {
  delta = rlnorm(N,mu,sig)
  chi_U = 1/(1+delta^(-theta))
  tau.vec = 1+ rbeta(N,beta.a,beta.b)*tar_mfn
  C_tilde = sapply(delta,C.tilde,R=chi_R,theta=4)
  complycon <- C_tilde < tau.vec & chi_U<chi_R # logical for compliers-constrained
  chi_model = chi_U*(complycon==FALSE) + chi_R*(complycon==TRUE)
  #observed cost shares = rule + comply error
  #err  = rnorm(N,mu.err,sig.err)  # normal error
  err  = rnorm(N,0,sig.err)  # normal error
  # chi_observed = chi_model +err
  chi_observed = chi_U*(complycon==FALSE)  + (chi_R+mu.err)*(complycon==TRUE) +err
  return(list(chi_observed=chi_observed,chi_U=chi_U,chi_model=chi_model,comply_frac = mean(complycon)))
}
# analytic density of chi_U (and lambda_U) for unconstrained firms
pdf_U <- function(x,theta,mu,sig,pct=TRUE) {
if(pct==TRUE) x = x/100 
  y = x/(1-x)
g=  dlnorm(y^(1/theta),mu,sig)*(1/(theta*x^2))*y^(1+1/theta)
if(pct==TRUE) return(g/100) else return(g)
}


#specify rho as a parts share, return a cost share (eq 3)
#cost.share <- function(rho,delta,theta) 1/(1+delta*((1-rho)/rho)^((1+theta)/theta))
#specify a parts-cost share, return the corresponding parts share (eq 4)



