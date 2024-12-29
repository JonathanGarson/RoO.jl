#estimate the distributions, assuming they are beta distributed.
using DataFrames
using RData
using DataFramesMeta
using Plots
using StatsPlots 
using LaTeXStrings
using Distributions
DC = load("Data/RDS_JIE_rev/tau_index_DRF.rds")
tau_MX = @subset(DC,:V_iso_o .== "MEX").tauD
tau_US = @subset(DC,:V_iso_o .== "USA").tauD
tau_CA = @subset(DC,:V_iso_o .== "CAN").tauD
tau_all = DC.tauD
estLN =fit_mle(LogNormal,tau_all)
d_est = LogNormal(estLN.μ,estLN.σ)
# 5 densities
p=StatsPlots.density(tau_CA,fill=(0, .5,:red),label="Canada",linecolor=:red)
p=StatsPlots.density!(tau_MX,fill=(0, .5,:green),label="Mexico",linecolor=:green)
p=StatsPlots.density!(tau_US,fill=(0, .5,:blue),label="USA",linecolor="blue")
p=StatsPlots.density!(tau_all,linecolor=:black,linewidth=2,label="US+MX+CA")
p = plot!(d_est,label="LogNormal fit",xlim =(1.0,1.25))
p= plot!(size=(725,400))

savefig(p,"Plots_JIE_rev/tau_index_densities.pdf")

