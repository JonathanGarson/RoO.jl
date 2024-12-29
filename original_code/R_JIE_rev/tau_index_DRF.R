#
library(HeadR)
library(countrycode)
rm(list=ls())
eta <- 4
#
# Wrappers for groups of countries
#base <- "MEX" # for testing
USMCA <- c("USA","CAN","MEX")
# Start Looping over the 3 base countries (NAFTA)
for(base in USMCA){
cat("Starting Base= ", base, "\n")
RoRTA <- USMCA[USMCA %ni% base] # Rest of RTA (depends on base)
#
# Get sales data for share calc ====
DS <- readRDS("_Data_notforweb/RDS/sales_clean.rds") #loading IHS sales data (source sales_clean.R)
DS[V_brand=="Audi" & year ==2016,.(sales=sum(sales)),by=V_iso_o][,shr := round(100*sales/sum(sales))][order(-shr)]
DS[,HS_head := fifelse(V_type=="LCV","8704","8703")] # commercial vehicles are trucks: 8704
#
# Get tariff data by brand model ====
DV <- readRDS("_Data_notforweb/RDS/V_sourcing_regfile.rds") # 
DV[,unique(V_HS2007)] # includes 8704## codes
DV[,HS_head := substr(V_HS2007,1,4)]
DV[,table(HS_head)] # 2 codes only, vast majority of obs in 8703
#
# Keep only info on MFNs as maximum tariffs by HS_head
MFN <- DV[year %in% 2011:2018,.(tar_mfn=max(TARIFF_RATE/100)),by=.(V_iso_d,year,HS_head)] #n.b. no brand model info
#
# Cars produced in USMCA: collapsed sales data
# This file can be used for tauD and tauR
DSc <- DS[year %in% 2011:2018 & V_iso_o %in% USMCA,.(sales=sum(sales)),.(V_brand,V_model,V_iso_o,V_iso_d,HS_head,year)]
# hold this object unchanged
#
# Start of tauD computation ====
#
# Generate a variable that keeps a separate observation for the Rest of RTA countries
DN <- DSc[V_iso_o==base]
DN[,V_reg_d := fcase(
  V_iso_d %in% USMCA & V_iso_d !=V_iso_o, V_iso_d,
  V_iso_d %ni% USMCA | V_iso_d ==V_iso_o, "DW" #domestic + restofworld = "K" in the note
)]
#
# Collapse sales by V_reg_d, new method involves generating 0s to avoid losing those observations later.
DN[,table(V_reg_d)]
dest_dcast <- DN[,unique(V_reg_d)]
merge_vars <- c("V_brand","V_model","V_iso_o","year","HS_head")
DNwide <- dcast(DN[year %in% 2011:2018 & V_iso_o %in% USMCA],V_brand+V_model+year+V_iso_o+HS_head~V_reg_d,fun.aggregate = sum,value.var = "sales")
DNlong <- melt(DNwide,id.vars = merge_vars,measure.vars = dest_dcast, value.name = "sales", variable.name ="V_reg_d", variable.factor=FALSE)
DNlong[,sales:=sales+1] # NEW: add 1 car to all sales to avoid division by 0
DNlong[,table(V_reg_d)]
DN <- copy(DNlong)
setnames(DN,old = "sales",new= "qs")
rm(DNlong,DNwide)
#
# Separate DW from the others to compute ratio
DN.DW <- DN[V_reg_d=="DW"] # Domestic + rest of world  
setnames(DN.DW,old="qs",new="qs_DW")
DN.DW[,V_reg_d:=NULL]
DN.R <- DN[V_reg_d!="DW"] # rest of RTA sales set "R"
setnames(DN.R,old=c("qs","V_reg_d"),new=c("qs_R","V_iso_d"))
DN.M <- merge_stata(DN.R,DN.DW,by=merge_vars) # merge == 3 only now
DN.M <- DN.M[stata_merge ==3]
DN.M[,stata_merge := NULL]
# Compute ratio and summarize
DN.M[, r_d := qs_R/qs_DW] # see Notes/tau_heterogeneity
DN.M[V_iso_o==base, summary(r_d)] # 4 NAs, some ifnty
DN.M[qs_DW == 0, .N] # 20 pbs
DN.M <- DN.M[qs_DW != 0]
DN.M[V_iso_o==base, summary(r_d)] 
#
# Merge back tariffs ====
DN.M <- merge_stata(DN.M,MFN,by=c("year","V_iso_d","HS_head"))
DN.M <- DN.M[stata_merge==3]
DN.M[,stata_merge := NULL]
#
# Calculate the tau_index_D ====
# tariff penalty:
DN.M[, tau_d := 1+tar_mfn]
# produce a table of statistics
DN.M[,.(md = median(tau_d), mn = round(mean(tau_d),3),m75=quantile(tau_d,0.75),mx = max(tau_d),ms = sum(is.na(tau_d))),by=.(V_iso_o,V_iso_d)]
#
DN.M[,rts := sum(r_d*tau_d^(1-eta),na.rm=TRUE) ,by=.(V_brand,V_model,V_iso_o,year)]  # numerator summation
DN.M[,rs := sum(r_d,na.rm=TRUE) ,by=.(V_brand,V_model,V_iso_o,year)]        #denominator summation
DN.M[,tau_index := ((1+rts)/(1+rs))^(1/(1-eta))]     #tau index
# Q version (only powers are different, from 1-eta to -eta):
DN.M[,rts := sum(r_d*tau_d^(-eta),na.rm=TRUE) ,by=.(V_brand,V_model,V_iso_o,year)]  # numerator summation Q version
DN.M[,rs := sum(r_d,na.rm=TRUE) ,by=.(V_brand,V_model,V_iso_o,year)]        #denominator summation Q version
DN.M[,tau_index_Q := ((1+rts)/(1+rs))^(1/(-eta))]     #tau index Q version
#
# One obs per brand-model-origin-year-HS
TAUD <- DN.M[, .(tauD = first(tau_index),tauD_Q = first(tau_index_Q)), by =.(V_brand,V_model,V_iso_o,year,HS_head)]     
#
TAUD[,.(tau_med=median(tauD),tau_max=max(tauD),
        tau_med_Q=median(tauD_Q),tau_max=max(tauD_Q)), by =.(V_iso_o,HS_head)][order(V_iso_o,HS_head)] #descriptive only, no change in data
#
rm(DN,DN.M,DN.DW,DN.R)
#
# Start of tauR computation ====
#
# Generate a variable that keeps a separate observation for the Rest of RTA countries
DN <- DSc[V_iso_o==base]
DN[, V_reg_d := fcase(
  V_iso_d %in% USMCA , V_iso_d,
  V_iso_d %ni% USMCA , "ROW" # restofworld = "K^F" in the note
)]
#
# Collapse sales by V_reg_d, new method involves generating 0s to avoid losing those observations later.
DN[,table(V_reg_d)]
dest_dcast <- DN[,unique(V_reg_d)]
merge_vars <- c("V_brand","V_model","V_iso_o","year","HS_head")
DNwide <- dcast(DN[year %in% 2011:2018 & V_iso_o %in% USMCA],V_brand+V_model+year+V_iso_o+HS_head~V_reg_d,fun.aggregate = sum,value.var = "sales")
DNwide[, `:=` (CAN=CAN+1, MEX=MEX+1, USA=USA+1, ROW=ROW+1)] #  NEW: add one car to all sales destinations to avoid division by 0 (do it wide to save output line 118)
DNlong <- melt(DNwide,id.vars = merge_vars,measure.vars = dest_dcast,value.name = "sales",variable.name ="V_reg_d",variable.factor=FALSE)
DNlong[,table(V_reg_d)]
DN <- copy(DNlong)
#
#DsalesBASEwide <- copy(DNwide) # save for later use 
#saveRDS(DsalesBASEwide,paste0("Data/Dsaleswide_",base,".rds"))
#
setnames(DN,old = "sales",new= "qs")
rm(DNlong,DNwide)
#
# Separate ROW from the others to compute ratio
DN.ROW <- DN[V_reg_d=="ROW"] #  Rest of world  
setnames(DN.ROW,old="qs",new="qs_ROW")
DN.ROW[,V_reg_d:=NULL]
DN.R <- DN[V_reg_d!="ROW"] # USMCA sales (includes MEX as a dest)
setnames(DN.R,old=c("qs","V_reg_d"),new=c("qs_R","V_iso_d"))
DN.M <- merge_stata(DN.R,DN.ROW,by=c("V_brand","V_model","year","V_iso_o","HS_head")) # all 3
DN.M[,stata_merge := NULL]
#
# Compute ratio and summarize
DN.M[, r_d := qs_R/qs_ROW] # see Notes/tau_heterogeneity
DN.M[V_iso_o==base, summary(r_d)] # 60 NAs, some ifnty
DN.M[qs_ROW == 0, .N] # 108 pbs
DN.M <- DN.M[qs_ROW != 0]
DN.M[V_iso_o==base, summary(r_d)] # 
#
#
# Merge back tariffs ====
DN.M <- merge_stata(DN.M,MFN,by=c("year","V_iso_d","HS_head"))
DN.M <- DN.M[stata_merge==3] #1014 (no _merge == 1)
DN.M[,stata_merge := NULL]
#
# Calculate the tau_index_R ====
# tariff penalty:
DN.M[, tau_d1 := 1+tar_mfn]
DN.M[, tau_d2 := 1+tar_mfn]
DN.M[V_iso_d == RoRTA[1], tau_d1 := 1] 
DN.M[V_iso_d == RoRTA[2], tau_d2 := 1] 
#
DN.M[,rts1 := sum(r_d*tau_d1^(1-eta),na.rm=TRUE) ,by=.(V_brand,V_model,V_iso_o,year)]  # numerator summation for R1
DN.M[,rts2 := sum(r_d*tau_d2^(1-eta),na.rm=TRUE) ,by=.(V_brand,V_model,V_iso_o,year)]  # numerator summation for R2
DN.M[,rs := sum(r_d,na.rm=TRUE) ,by=.(V_brand,V_model,V_iso_o,year)]        #denominator summation for R1 and R2
DN.M[,tau_index1 := ((1+rts1)/(1+rs))^(1/(1-eta))]     #tau index for R1 
DN.M[,tau_index2 := ((1+rts2)/(1+rs))^(1/(1-eta))]     #tau index for R2
DN.M[,tau_index := fifelse(tau_index1 <= tau_index2, tau_index1, tau_index2)] # select low tau_index origin among R1,R2
DN.M[,chosen_o := fifelse(tau_index1 <= tau_index2, RoRTA[1], RoRTA[2])] # keep track of the chosen origin
# Q version (only powers are different, from 1-eta to -eta):
DN.M[,rts1 := sum(r_d*tau_d1^(-eta),na.rm=TRUE) ,by=.(V_brand,V_model,V_iso_o,year)]  # numerator summation for R1
DN.M[,rts2 := sum(r_d*tau_d2^(-eta),na.rm=TRUE) ,by=.(V_brand,V_model,V_iso_o,year)]  # numerator summation for R2
DN.M[,rs := sum(r_d,na.rm=TRUE) ,by=.(V_brand,V_model,V_iso_o,year)]        #denominator summation for R1 and R2
DN.M[,tau_index1 := ((1+rts1)/(1+rs))^(1/(-eta))]     #tau index for R1 
DN.M[,tau_index2 := ((1+rts2)/(1+rs))^(1/(-eta))]     #tau index for R2
DN.M[,tau_index_Q := fifelse(tau_index1 <= tau_index2, tau_index1, tau_index2)] # select low tau_index origin among R1,R2
#
# One obs per brand-model-origin-year-HS
TAUR <- DN.M[, .(tauR = first(tau_index), tauR_Q = first(tau_index_Q), chosen_R = first(chosen_o)), by =.(V_brand,V_model,V_iso_o,year,HS_head)]     
#
TAUR[,.(tau_med=median(tauR),tau_max=max(tauR),shr1=mean(chosen_R==RoRTA[1]),shr2=mean(chosen_R==RoRTA[2]),
        tau_med_Q=median(tauR_Q),tau_max_Q=max(tauR_Q)), by =.(V_iso_o,HS_head)][order(V_iso_o,HS_head)] #descriptive only, no change in data
RoRTA[1]
#
rm(DN,DN.M,DN.ROW,DN.R)
#
# Start of tauF computation ====
#
# list all possible origins (without USMCA) and destinations in DS
origins <- unique(DS[V_iso_o %ni% USMCA]$V_iso_o)
destinations <- unique(DS$V_iso_d)
# get RTAs 
readRDS("Data/Gravdata/Gravity_V202102.rds")[,.(V_iso_o=iso3_o,V_iso_d = iso3_d,year,rta,eu_o,eu_d)] |> unique() |> na.omit() -> GRTA
GRTA <- GRTA[(V_iso_o %in% origins | V_iso_o ==base) & V_iso_d %in% destinations & year %in% 2011:2018]
GRTA[, eu := eu_d==1 & eu_o==1]
# get the raw distances
readRDS("Data/Gravdata/Gravity_V202102.rds")[,.(V_iso_o=iso3_o,V_iso_d = iso3_d,distw)] |> unique() |> na.omit() -> GD
# the two distance matrices are used to create the ratio (dims: od , but not USMCA for o)
Dist_od <- GD[V_iso_o %in% origins & V_iso_d %in% destinations]
Dist_BASEd <- GD[V_iso_o == base & V_iso_d %in% destinations]
Dist_ratio <- merge_stata(Dist_od[,.(V_iso_o,V_iso_d,Dist_od=distw)],Dist_BASEd[,.(V_iso_d,Dist_BASEd=distw)],by=("V_iso_d")) # all 3
Dist_ratio[,stata_merge := NULL]
Dist_ratio[, Dist_ratio := Dist_od/Dist_BASEd]
Dist_ratio <- Dist_ratio[, .(V_iso_o,V_iso_d,Dist_ratio)]
rm(Dist_BASEd,Dist_od)
# get the overall matrix of quantities. This file is different from the one used for tauD and tauR, because keeps all origins
DScF <- DS[year %in% 2011:2018,.(sales=sum(sales)),.(V_brand,V_model,V_iso_o,V_iso_d,HS_head,year)]
# compute ratio of quantities BASEd to BASEBASE (dims: V_brand,V_model,V_iso_d,HS_head,year)
DScF1 <- DScF[V_iso_o==base]
DScF1 <- DScF1[,.(V_brand,V_model,V_iso_o,V_iso_d,HS_head,year,q_BASEd=sales)]
DScF2 <- DScF[V_iso_o==base&V_iso_d==base]
DScF2 <- DScF2[,.(V_brand,V_model,V_iso_o,HS_head,year,q_BASEBASE=sales)]
q_ratio <- merge_stata(DScF1, DScF2,by=c(merge_vars)) # some merge == 1 (ie not sold in BASE), rest 3
q_ratio[stata_merge==1, q_BASEBASE:=0]
q_ratio[, stata_merge := NULL]
q_ratio[,q_ratio := (q_BASEd+1)/(q_BASEBASE+1)] # add 1 to avoid division by zero
q_ratio <- q_ratio[,.(V_brand,V_model,V_iso_d,HS_head,year,q_ratio)]
rm(DScF1,DScF2)
# cross join creating DSmat
q_ratio[, combo := .GRP, by=c("V_brand","V_model","V_iso_d","year","HS_head")]
DSmat <- q_ratio[,CJ(combo,origins,unique = TRUE)] 
DSmat <- merge_stata(DSmat,q_ratio,by=("combo")) 
DSmat[,V_iso_o := origins]
DSmat[, stata_merge := NULL]
# merge back distances
DSmat <- merge_stata(DSmat,Dist_ratio,by=c("V_iso_o","V_iso_d"))
DSmat <- DSmat[stata_merge==3] 
DSmat[, stata_merge := NULL]
# merge back tariffs
DSmat <- merge_stata(DSmat,MFN,by=c("year","V_iso_d","HS_head"))
DSmat <- DSmat[stata_merge==3] 
DSmat[,stata_merge := NULL]
# merge back RTAs
DSmat <- merge_stata(DSmat,GRTA,by=c("year","V_iso_o","V_iso_d"))
DSmat <- DSmat[stata_merge==3] 
DSmat[,stata_merge := NULL]

# Calculate the tau_index_F ====
# tariff penalty:
DSmat[, tau_od := 1+tar_mfn]
DSmat[eu == 1 | (V_iso_o==V_iso_d) , tau_od := 1] # numerator: we account for EU because customs union. Assumption= carline relocated to o would not comply with RoOs of od RTA 
DSmat[, tau_BASEd := 1+tar_mfn] 
DSmat[V_iso_d %in% USMCA , tau_BASEd := 1] # denominator: Assumption = BASE-made carline does comply in USMCA but does not comply with other RTAs of BASE  
dist_coeff <- -0.323 # from HM2019AER
#
DSmat[,rts := sum(q_ratio * Dist_ratio^dist_coeff * tau_od^(1-eta),na.rm=TRUE) ,by=.(V_brand,V_model,V_iso_o,year)]  # numerator summation
DSmat[,rs := sum(q_ratio * tau_BASEd^(1-eta),na.rm=TRUE) ,by=.(V_brand,V_model,V_iso_o,year)]        #denominator summation
DSmat[,tau_index := (rts/rs)^(1/(1-eta))]     #tau index
# Q Version
DSmat[,rts := sum(q_ratio * Dist_ratio^dist_coeff * tau_od^(-eta),na.rm=TRUE) ,by=.(V_brand,V_model,V_iso_o,year)]  # numerator summation
DSmat[,rs := sum(q_ratio * tau_BASEd^(-eta),na.rm=TRUE) ,by=.(V_brand,V_model,V_iso_o,year)]        #denominator summation
DSmat[,tau_index_Q := (rts/rs)^(1/(-eta))]     #tau index
#
# One obs per brand-model-origin-year-HS
setorder(DSmat,V_brand,V_model,year,HS_head,tau_index)
# get the minimum tau_o and its corresponding iso (chosen_F new name of V_iso_o once minimized)
TAUF <- DSmat[, .(tauF = first(tau_index), tauF_Q = first(tau_index_Q), chosen_F = first(V_iso_o)), by =.(V_brand,V_model,year,HS_head)]     
TAUF[,V_iso_o := base] # 
#
TAUF[,.(tau_med=median(tauF),tau_min=min(tauF),tau_max=max(tauF),
        tau_med_Q=median(tauF_Q),tau_min_Q=min(tauF_Q),tau_max_Q=max(tauF_Q)), by =.(V_iso_o,HS_head)][order(V_iso_o,HS_head)] #descriptive only, no change in data
#
#merge TAUD, TAUR, TAUF to create "data combined" => DC
#
DC <- merge_stata(TAUD,TAUR,by = merge_vars) 
DC[,stata_merge := NULL]
DC <- merge_stata(DC,TAUF,by = merge_vars) 
DC[,stata_merge := NULL]
#
assign(paste0("DC_",base),DC)
}
DC <- rbind(DC_CAN,DC_USA,DC_MEX)
# save the three tau (+ 3 Q versions) for each model and anonymize
V_id_vars <- c("V_brand","V_model")
DC[, V_id := .GRP, by=V_id_vars] # numerical
DC[, c("V_brand","V_model") := NULL]
setcolorder(DC,"V_id")
saveRDS(DC,"Data/RDS_JIE_rev/tau_index_DRF.rds")




