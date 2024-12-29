#Tables for EHA, endogenous location, showing choices, including RCR-chosen, price indexes, X, L, LT, 
library(HeadR)
library(parallel)
#rm(list=ls()) # This is done in master_JIE.R
#
eta <- 4
# choose which kappa here, this is set in the master_JIE.R
kappa_AVE <- if (!exists("kappa_AVE")) 20 else kappa_AVE
# choose which RCR to keep here (can include 0 in RCRkeep for diagnosis, not needed here) 
RCRkeep <- c(62.5,75.0,85.0)
#
# if directory is absent, create it
tabdir <- paste0("Tables_JIE_rev/kappa",kappa_AVE)
if(!file.exists(tabdir)) {
  dir.create(tabdir)
}
# function to read RDS of simulation results
doit <- function(chosen.country,chosen.HS,chosen.kappa_AVE) {
  lafferRDS <- paste0("Data/RDS_JIE_rev/kappa",chosen.kappa_AVE,"/XLT_laffer_", chosen.country, "_HS",chosen.HS,"_DRF.rds")
  XLT <- readRDS(lafferRDS)
  XLT <- XLT[RCR %in% RCRkeep]
# X,L,LT need to  renamed to be _DRF variables to distinguish from X,L,LT, which are coming for exog location
  setnames(XLT,c("X","L","LT"),c("X_DRF","L_DRF","LT_DRF"))
  choicesRDS <- paste0("Data/RDS_JIE_rev/kappa",chosen.kappa_AVE,"/roo_choice_", chosen.country, "_HS",chosen.HS,"_DRF.rds")
  choice <- readRDS(choicesRDS)
  choice[, RCR := 100*RCR]
  choice <- choice[RCR %in% RCRkeep]
  XLT <- merge(XLT,choice,by="RCR")
#prices from relocation case (DRF)
  priceRDS <- paste0("Data/RDS_JIE_rev/kappa",kappa_AVE,"/price_change_",chosen.country,"_HS",chosen.HS,"_DRF.rds")
  P.hat <- readRDS(priceRDS)
#rename the DRF variables with _DRF to indicate these are the endog location versions of P
  setnames(P.hat,c("P","P_CDC"),c("P_DRF","P_CDC_DRF"))
  XLT <- merge(XLT,P.hat,by="RCR",all.x=TRUE)
# prices from exog location case
  priceRDS <- paste0("Data/RDS_JIE_rev/price_change_",chosen.country,"_HS",chosen.HS,".rds")
  P.hat <- readRDS(priceRDS)
  XLT <- merge(XLT,P.hat,by="RCR",all.x=TRUE)
  # add the XLT stuff from the non-DRF (exog location) case, re-using file name
  lafferRDS <- paste0("Data/RDS_JIE_rev/XLT_laffer_",chosen.country,"_HS",chosen.HS,".rds")
  XLTexog <- readRDS(lafferRDS)
  XLTexog <- XLTexog[RCR %in% RCRkeep]
# merge it on as well with XL non-relocation vars
  XLT <- merge(XLT,XLTexog,by="RCR",all.x=TRUE)
    return(data.table(HS=chosen.HS,V_iso_o=chosen.country,XLT))
}
# new table for XLT_DRF with kappa = 1.1 and 1.3 side by side
doit2 <- function(chosen.country,chosen.HS) {
  kappa_AVE_lo <- 10
  lafferRDS <- paste0("Data/RDS_JIE_rev/kappa",kappa_AVE_lo,"/XLT_laffer_", chosen.country, "_HS",chosen.HS,"_DRF.rds")
  XLT_lo <- readRDS(lafferRDS)
  XLT_lo <- XLT_lo[RCR %in% RCRkeep]
  # X,L,LT need to  renamed to be _DRF variables to distinguish from X,L,LT, which are coming for exog location
  setnames(XLT_lo,c("X","L","LA","LT"),c("X_lo","L_lo","LA_lo","LT_lo"))
  kappa_AVE_hi <- 30
  lafferRDS <- paste0("Data/RDS_JIE_rev/kappa",kappa_AVE_hi,"/XLT_laffer_", chosen.country, "_HS",chosen.HS,"_DRF.rds")
  XLT_hi <- readRDS(lafferRDS)
  XLT_hi <- XLT_hi[RCR %in% RCRkeep]
  # X,L,LT need to  renamed to be _DRF variables to distinguish from X,L,LT, which are coming for exog location
  setnames(XLT_hi,c("X","L","LA","LT"),c("X_hi","L_hi","LA_hi","LT_hi"))
  XLT <- merge(XLT_lo,XLT_hi,by="RCR",all.x=TRUE)
  return(data.table(HS=chosen.HS,V_iso_o=chosen.country,XLT))
}

# function to check that all files are there
complete.files <- function(HS) {
  dir_path <- paste0("Data/RDS_JIE_rev/kappa",kappa_AVE)
  files <- list.files(dir_path, pattern = HS)
  num_files <- length(files)
return(num_files==10)
}

#
logical.HS <- sapply(c("8703","8704"),complete.files)
# Set the directory path to the folder you want to search in
# List all files in the directory that match the search string
# Count the number of files
available.HS <- names(logical.HS[logical.HS==TRUE]) 
# print the result
#cat("Number of files in", dir_path, "with '", search_string, "' in their filename:", num_files)
#
# Finally iterate over all combinations to extract results and do the table
iter <- CJ(chosen.country = c("CAN","MEX","USA"),chosen.HS = available.HS,chosen.kappa_AVE=kappa_AVE)
DT <- Map(doit,iter$chosen.country,iter$chosen.HS,iter$chosen.kappa_AVE)
DT <- rbindlist(DT,fill=TRUE)
setnames(DT,c("frac_CDU","frac_CDC","frac_NCD","frac_NCF","frac_NCR","frac_USA_NCR","frac_CAN_NCR","frac_MEX_NCR"),
                      c("CDU","CDC","NCD","NCF","NCR","USA_NCR","CAN_NCR","MEX_NCR"))
NAto0 <- function(x) fifelse(is.na(x),0,x)
DT[,NCD := NAto0(NCD)]
DT[,CD := 100*(CDC+CDU)] #all compliers
DT[, RoO := fcase(RCR==62.5,"NAFTA",
                  RCR==75,"USMCA",
                  RCR==85,"US ask")]
# Multirow for all variables except price change
DT[, obs := rank(RCR), by=.(V_iso_o,HS)]
DT[, V_iso_o_multi := fcase(obs==1, paste0("\\multirow{3}{*}{",V_iso_o,"}"), 
                            obs>1,"")]
#
# output XLT; 
DT[, out := texout(.(V_iso_o_multi,RCR,X,L,LT,X_DRF,L_DRF,LT_DRF),digits=1)]
DT[, out := fifelse(RoO == "US ask",paste0(out," [0.67ex] "),out)]

# output share of NCR going to USA
DT[, out1 := texout(.(V_iso_o_multi,RCR,NCR*100,round(CAN_NCR*100,1),round(MEX_NCR*100,1),round(USA_NCR*100,1)),digits=1)]
DT[, out1 := fifelse(RoO == "US ask",paste0(out1," [0.67ex] "),out1)]

# output price change 
DT[, CS := 100*((1+P/100)^(1-eta)-1)]
DT[, CS_DRF := 100*((1+P_DRF/100)^(1-eta)-1)]
DT[, out2 := texout(.(V_iso_o_multi,as.character(RCR),P,CS,P_DRF,CS_DRF),digits=2)]
DT[, out2 := fifelse(RoO == "US ask",paste0(out2," [0.67ex] "),out2)]

# Create the tex raw files
# upper (cars) and lower (trucks) panels for the tables.
for(hs in available.HS) {
# X, L, T main table
tbl <- paste0("Tables_JIE_rev/kappa",kappa_AVE,"/XL_USMCA_HS",hs,".tex")
writeLines(DT[HS==hs]$out,tbl)
#price index changes corresponding to the rise in RoO from no RoO to {nafta,uscma,us ask}
tbl2 <- paste0("Tables_JIE_rev/kappa",kappa_AVE,"/Phat_USMCA_HS",hs,".tex")
writeLines(DT[HS==hs]$out2,tbl2)
}
cat("Completed kappa ",kappa_AVE ,"\n")
# the above kappa should be 1.2
# for kappa 1.1 and 1.3 do the side-by-side table
iter2 <- CJ(chosen.country = c("CAN","MEX","USA"),chosen.HS = available.HS)
DT2 <- Map(doit2,iter2$chosen.country,iter2$chosen.HS)
DT2 <- rbindlist(DT2,fill=TRUE)
#
DT2[, RoO := fcase(RCR==62.5,"NAFTA",
                  RCR==75,"USMCA",
                  RCR==85,"US ask")]
#
DT2[, obs := rank(RCR), by=.(V_iso_o,HS)]
DT2[, V_iso_o_multi := fcase(obs==1, paste0("\\multirow{3}{*}{",V_iso_o,"}"), 
                            obs>1,"")]
DT2[, out := texout(.(V_iso_o_multi,RCR,X_lo,L_lo,LT_lo,X_hi,L_hi,LT_hi),digits=1)]
DT2[, out := fifelse(RoO == "US ask",paste0(out," [0.67ex] "),out)]
#
for(hs in available.HS) {
  # X, L, T main table
  tbl <- paste0("Tables_JIE_rev/XL_USMCA_lohi_HS",hs,".tex")
  writeLines(DT2[HS==hs]$out,tbl)
 }

# tables on choice 0
kappa_AVE <- 20
#
chosen.HS <- 8703
choice0RDS <- paste0("Data/RDS_JIE_rev/Choice0/roo_choice0_HS",chosen.HS,"_DRF.rds")
assign(paste0("choice0_",chosen.HS),data.table(readRDS(choice0RDS),HS=chosen.HS))
#
chosen.HS <- 8704
choice0RDS <- paste0("Data/RDS_JIE_rev/Choice0/roo_choice0_HS",chosen.HS,"_DRF.rds")
assign(paste0("choice0_",chosen.HS),data.table(readRDS(choice0RDS),HS=chosen.HS))
choice0 <- rbind(choice0_8703,choice0_8704)
choice0[, frac := N/sum(N), by = .(V_iso_o,HS)]
choice0_wide <- dcast(choice0, V_iso_o + HS ~choice, value.var = c("frac"))

