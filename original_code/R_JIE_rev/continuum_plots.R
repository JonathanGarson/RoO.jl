library(latex2exp)
library(data.table)
rm(list=ls())
source("R_JIE_rev/continuum_functions_no_defaults.R")
#
L <- 100
params = list(theta=4)
tau_graph = 1.1
#
roos <- seq(0,1,length.out = L)
Ct80 <- sapply(roos,C.tilde,delta=0.8, theta=params$theta)
Ct100 <- sapply(roos,C.tilde,delta=1, theta=params$theta)
Ct120 <- sapply(roos,C.tilde,delta=1.2, theta=params$theta)
#
# Figure 2 upper panel ====
lo <- 0.4
hi <- 1.6
deltas <- seq(lo,hi,length.out = L)
Ctd <- sapply(deltas,C.tilde,R=0.7,theta=params$theta)
pdf("Plots_JIE_rev/C_tilde_delta.pdf",width=8,height=4.5)
par(mar=c(4,5,1,1)+0.1 )
plot(deltas,Ctd,type="l",col=adjustcolor("navy",0.65),lwd=2,ylim=c(0.9,max(Ctd)),
     xlab=TeX("Firm-level home cost advantage ($\\delta$)"),
     ylab=TeX("Cost penalty for ROO compliance ($\\tilde{C}$)"))
abline(h=1,lty="dashed",lwd=1)
abline(h=tau_graph,col=adjustcolor("black",0.5),lwd=2)
text(deltas[50],tau_graph,TeX("$\\tau$"),pos=3)
abline(v=delta.star(R=0.7,theta=params$theta,tau=tau_graph),lty="dotted",col="blue")
abline(v=delta.circ(R=0.7,theta=params$theta),lty="dotted",col="blue")
#mtext("Non-compliant",side=3,at=(lo+delta.star(R=0.7))/2)
mtext("Non-compliant",side=1,at=(lo+delta.star(R=0.7,theta=params$theta,tau=tau_graph))/2,line=-1,col="blue")
mtext("Compliant-Constrained",side=1,at=(delta.star(R=0.7,theta=params$theta,tau=tau_graph)+delta.circ(R=0.7,theta=params$theta))/2,line=-1,col="blue")
mtext("Compliant-Unconstrained",side=1,at=(delta.circ(R=0.7,theta=params$theta)+hi)/2,line=-1,col="blue")
mtext(side=3,at=delta.star(R=0.7,theta=params$theta,tau=tau_graph),TeX("$\\delta^*$"))
mtext(side=3,at=delta.circ(R=0.7,theta=params$theta),TeX("$\\delta^{\\degree}$"))
dev.off()
#
# Figure 2 lower panel ====
L <- 1000
deltas <- seq(lo,hi,length.out = L)
lambda.uncon <- sapply(deltas,lambda_min,theta=params$theta)
lambda.con <- fifelse(deltas>delta.star(R=0.7,theta=params$theta,tau=tau_graph) &deltas <delta.circ(R=0.7,theta=params$theta),0.7,lambda.uncon)
pdf("Plots_JIE_rev/lambda_delta.pdf",width=8,height=4.5)
par(mar=c(4,5,1,1)+0.1 )
plot(deltas,lambda.uncon,type="l",col=adjustcolor("navy",0.75),lwd=2,ylim=range(lambda.uncon),
     xlab=TeX("Firm-level home cost advantage ($\\delta$)"),
     ylab=TeX("Regional content share ($\\chi$)"))
lines(deltas,lambda.con,col=adjustcolor("orange",0.65),lwd=3)
abline(h=0.7,lty="dashed")
abline(v=delta.star(R=0.7,theta=params$theta,tau=tau_graph),lty="dotted",col="blue")
abline(v=delta.circ(R=0.7,theta=params$theta),lty="dotted",col="blue")
mtext("Non-compliant",side=1,at=(lo+delta.star(R=0.7,theta=params$theta,tau=tau_graph))/2,line=-1,col="blue")
mtext("Compliant-Constrained",side=1,at=(delta.star(R=0.7,theta=params$theta,tau=tau_graph)+delta.circ(R=0.7,theta=params$theta))/2,line=-1,col="blue")
mtext("Compliant-Unconstrained",side=1,at=(delta.circ(R=0.7,theta=params$theta)+hi)/2,line=-1,col="blue")
text(1,0.7,TeX("$\\chi = \\chi_R = 0.7$"),pos=3)
text(1,lambda_min(1,theta=params$theta),TeX("$\\chi_{U}(\\delta) $"),pos=4)
mtext(side=3,at=delta.star(R=0.7,theta=params$theta,tau=tau_graph),TeX("$\\delta^*$"))
mtext(side=3,at=delta.circ(R=0.7,theta=params$theta),TeX("$\\delta^{\\degree}$"))
dev.off()
#
# Figure 3 ====
params = list(theta=4,mu=0,sigma=0.2)
L <- 1000
roos <- seq(0.001,0.999,length.out = L)
ds <- sapply(roos,delta.star,theta=params$theta,tau=tau_graph)
dc <- sapply(roos,delta.circ,theta=params$theta)
S_NC <- sapply(ds,CDF, mu=params$mu, sigma=params$sigma) 
S_C <- sapply(dc,CDF, mu=params$mu, sigma=params$sigma)
S_CC <- S_C-S_NC  
S_CU <- 1-S_C  
rm(S_C)  #
pdf("Plots_JIE_rev/shares_roo.pdf",width=8,height=4.5)
par(mar=c(4,4,1,1)+0.1 )
plot(roos,S_CU,type="l",col=adjustcolor("olivedrab",0.75),
     lwd=2,ylim=c(0,1),yaxs="i",
     ylab=TeX("Share of firms"),
     xlab=TeX("Regional content requirement ($\\chi_R$)"))
lines(roos,S_CC,col=adjustcolor("orange",0.75),lwd=2)
lines(roos,S_NC,col=adjustcolor("navy",0.75),lwd=2)
text(roos[300],S_CU[300],"Compliant-Unconstrained",pos=4)
text(roos[730],S_CC[730],"Compliant- \n Constrained",pos=1)
text(roos[L],S_NC[L],"Non-Compliant",pos=2)
dev.off()
#
# Figure 4  ====
L <- 1000
roos <- seq(0.001,0.999,length.out = L)
lamb <- sapply(roos,function(x) lambda.bar(R=x,theta=params$theta,tau=tau_graph,mu=params$mu, sigma=params$sigma))
X <- sapply(roos,function(x) X_inner(tau=tau_graph,R=x,theta=params$theta,mu=params$mu, sigma=params$sigma))
roos.noAC <- seq(0.001,0.9999,length.out = 10*L) # you need this precision to ensure comes back to dashed line
lamb.noAC <- sapply(roos.noAC,lambda.bar.noAC,theta=params$theta,tau=tau_graph,mu=params$mu, sigma=params$sigma)
roo.laffer.range <- range(lamb.noAC,lamb)
pdf("Plots_JIE_rev/roo_laffer_noAC.pdf",width=8,height=4.5)
par(mar=c(4,5,1,0)+0.1 )
plot(roos,lamb,type="l",col="blue",lwd=2,ylim=roo.laffer.range,
     ylab=TeX("Average regional content share ($X$)"),
     xlab=TeX("Regional content requirement ($\\chi_R$)"))
lines(roos.noAC,lamb.noAC,col="orange",lwd=2)
abline(h=min(lamb.noAC),lty="dashed",col="orange")
abline(h=min(lamb),lty="dashed",col="blue")
text(0,min(lamb.noAC)+0.01,"Excluding \n Always Compliers",pos=4)
text(0,min(lamb)+0.007,"All Firms",pos=4)
dev.off()
max(lamb)
lamb[1000]
#
#
# Figure 4 alternatives (C.1 in the OA) ====
params = list(theta=4,mu=0,sigma=0.2)
tau_graph = 1.1
#
# Vary mu
lamb <- sapply(roos,lambda.bar,mu=params$mu,sigma=params$sigma,theta=params$theta,tau=tau_graph)
lamb.lo <- sapply(roos,lambda.bar,mu=-0.115,sigma=params$sigma,theta=params$theta,tau=tau_graph)
lamb.hi <- sapply(roos,lambda.bar,mu=0.115,sigma=params$sigma,theta=params$theta,tau=tau_graph)
roo.laffer.range <- range(lamb.hi+0.01,lamb.lo,lamb)
pdf("Plots_JIE_rev/roo_laffer_mu.pdf",width=4.5,height=4)
par(mar=c(4,5,1,0)+0.1 )
plot(roos,lamb,type="l",col="blue",lwd=2,ylim=roo.laffer.range,
     ylab=TeX("Average regional content share ($X$)"),
     xlab=TeX("Regional content requirement ($\\chi_R$)"))
lines(roos,lamb.lo,col="olivedrab",lwd=2)
lines(roos,lamb.hi,col="orange",lwd=2)
abline(h=c(0.4,0.5,0.6),lty="dashed")
text(-0.01,c(0.4,0.5,0.6)+.014,c("High-cost \n home","Symmetric \n countries","Low-cost \n home"),pos=4,cex=.8)
text(0.7,max(lamb.lo)-0.01, TeX(sprintf("$\\mu = %g$",-0.115)),pos=3,cex=.8)
text(0.785,max(lamb)-0.01, TeX(sprintf("$\\mu = %g$",0)),pos=3,cex=.8)
text(0.89,max(lamb.hi)-0.01, TeX(sprintf("$\\mu = %g$",0.115)),pos=3,cex=.8)
dev.off()
#
# Vary sigma
lamb.lo <- sapply(roos,lambda.bar,sigma=0.1,mu=params$mu,theta=params$theta,tau=tau_graph)
lamb.hi <- sapply(roos,lambda.bar,sigma=0.3,mu=params$mu,theta=params$theta,tau=tau_graph)
roo.laffer.range <- range(lamb.hi,lamb.lo,lamb)
pdf("Plots_JIE_rev/roo_laffer_sigma.pdf",width=4.5,height=4)
par(mar=c(4,5,1,0)+0.1 )
plot(roos,lamb,type="l",col="blue",lwd=2,ylim=roo.laffer.range,
     ylab=TeX("Average regional content share ($X$)"),
     xlab=TeX("Regional content requirement ($\\chi_R$)"))
lines(roos,lamb.lo,col="olivedrab",lwd=2)
lines(roos,lamb.hi,col="orange",lwd=2)
abline(h=0.5,lty="dashed")
text(0.78,max(lamb.lo), TeX(sprintf("$\\sigma = %g$",0.1)),pos=2)
text(0.78,max(lamb)-0.01, TeX(sprintf("$\\sigma = %g$",0.2)),pos=1)
text(0.78,max(lamb.hi), TeX(sprintf("$\\sigma = %g$",0.3)),pos=1)
dev.off()
#
# Vary theta
lamb.lo <- sapply(roos,lambda.bar,theta=2,mu=params$mu,sigma=params$sigma,tau=tau_graph)
lamb.hi <- sapply(roos,lambda.bar,theta=8,mu=params$mu,sigma=params$sigma,tau=tau_graph)
roo.laffer.range <- range(lamb.hi,lamb.lo,lamb)
pdf("Plots_JIE_rev/roo_laffer_theta.pdf",width=4.5,height=4)
par(mar=c(4,5,1,0)+0.1 )
plot(roos,lamb,type="l",col="blue",lwd=2,ylim=roo.laffer.range,
     ylab=TeX("Average regional content share ($X$)"),
     xlab=TeX("Regional content requirement ($\\chi_R$)"))
lines(roos,lamb.lo,col="olivedrab",lwd=2)
lines(roos,lamb.hi,col="orange",lwd=2)
abline(h=0.5,lty="dashed")
text(0.39,0.55, TeX(sprintf("$\\theta = %g$",4)),pos=1)  #benchmark
text(0.40,0.58, TeX(sprintf("$\\theta = %g$",8)),pos=2)  #hi
text(0.53,0.55, TeX(sprintf("$\\theta = %g$",2)),pos=4) #lo
dev.off()
#
# Vary tau
taus <- c(1.025, 1.1, 1.25)
lamb.lo <- sapply(roos,lambda.bar,tau=1.025,mu=params$mu,sigma=params$sigma,theta=params$theta)
lamb.hi <- sapply(roos,lambda.bar,tau=1.25,mu=params$mu,sigma=params$sigma,theta=params$theta)
roo.laffer.range <- range(lamb.hi,lamb.lo,lamb)
pdf("Plots_JIE_rev/roo_laffer_tau.pdf",width=4.5,height=4)
par(mar=c(4,5,1,0)+0.1 )
plot(roos,lamb,type="l",col=adjustcolor("blue",0.7),lwd=2,ylim=roo.laffer.range,
     ylab=TeX("Average regional content share ($X$)"),
     xlab=TeX("Regional content requirement ($\\chi_R$)"))
lines(roos,lamb.lo,col=adjustcolor("olivedrab",0.7),lwd=2)
lines(roos,lamb.hi,col=adjustcolor("orange",0.7),lwd=2)
abline(h=0.5,lty="dashed")
abline(v=0.625,lty="dashed")
text(0.625,0.8,"NAFTA ROO",pos=2)
text(0.78,max(lamb), TeX(sprintf("$\\tau = %g$",1.1)),pos=3)  #benchmark
text(1,lamb.hi[L]-0.01, TeX(sprintf("$\\tau = %g$",taus[3])),pos=2)  #hi 25% tariffs
text(0.72,max(lamb.lo), TeX(sprintf("$\\tau = %g$",taus[1])),pos=3) #lo 2.5% tariffs
dev.off()
#


