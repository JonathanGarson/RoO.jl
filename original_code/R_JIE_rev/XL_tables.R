#tables for EHA, endogenous location, showing choices, including RCR-chosen, price indexes, X, L, LT, 
library(HeadR)
library(parallel)
rm(list=ls())
#
eta <- 4
# choose which RCR to keep here (can include 0 in RCRkeep for diagnosis, not needed here) 
RCRkeep <- c(62.5,75.0,85.0)
#
# function to read RDS of simulation results
doit <- function(chosen.country,chosen.HS,chosen.kappa_AVE) {
  lafferRDS <- paste0("Data/RDS_JIE_rev/XLT_laffer_",chosen.country,"_HS",chosen.HS,".rds")
  XLT <- readRDS(lafferRDS)
  XLT <- XLT[RCR %in% RCRkeep]
  choicesRDS <- paste0("Data/RDS_JIE_rev/roo_choice_",chosen.country,"_HS",chosen.HS,".rds")
  choice <- readRDS(choicesRDS)
  choice <- choice[RCR %in% RCRkeep]
  XLT <- merge(XLT,choice,by="RCR")
  return(data.table(HS=chosen.HS,V_iso_o=chosen.country,XLT))
}
# function to check that all 10 files are there for each HS: 3 countries * 3 files (XLT, roo_choice, price) 
#+1 (XLT for North America) => 10
complete.files <- function(HS) {
  dir_path <- paste0("Data/RDS_JIE_rev")
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
iter <- CJ(chosen.country = c("CAN","MEX","USA"),chosen.HS = available.HS)
DT <- Map(doit,iter$chosen.country,iter$chosen.HS)
DT <- rbindlist(DT,fill=TRUE)
#
NAto0 <- function(x) fifelse(is.na(x),0,x)
#
DT[,CD := (CC+CU)] #all compliers
DT[, RoO := fcase(RCR==62.5,"NAFTA",
                  RCR==75,"USMCA",
                  RCR==85,"US ask")]
# Multirow for all variables except price change
DT[, obs := rank(RCR), by=.(V_iso_o,HS)]
DT[, V_iso_o_multi := fcase(obs==1, paste0("\\multirow{3}{*}{",V_iso_o,"}"), 
                            obs>1,"")]
#
# output XLT
DT[, out := texout(.(V_iso_o_multi,RCR,X,L,LT,CD,NC),digits=1)]
DT[, out := fifelse(RoO == "US ask",paste0(out," [0.67ex] "),out)]
#
for(hs in available.HS) {
# X, L, T main table
tbl <- paste0("Tables_JIE_rev/XL_USMCA_HS",hs,".tex")
writeLines(DT[HS==hs]$out,tbl)
}
