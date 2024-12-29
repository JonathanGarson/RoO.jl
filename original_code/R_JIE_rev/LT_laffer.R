# Last figures and tables for JIE revision 2
library(HeadR)
library(latex2exp)
rm(list=ls())
cat0 <- function(...) cat(...,sep="") 
#
# Those figures are run only for baseline kappa
kappa_AVE <- 20 # AVE= ad valorem equivalent, integers for folder names
#
# Figures ====
for(chosen.HS in 8703:8704) {
      fixedRDS <- paste0("Data/RDS_JIE_rev/XLT_laffer_HS",chosen.HS,".rds")
      Fixed <- readRDS(fixedRDS)
      endoRDS <- paste0("Data/RDS_JIE_rev/kappa",kappa_AVE,"/XLT_laffer_HS",chosen.HS,"_DRF.rds")
      Endo <- readRDS(endoRDS)
      #
      #final laffer curve figure L_T with and without relocation ====
      lafferPDF <- paste0("Plots_JIE_rev/AALA_calib/kappa",kappa_AVE,"/LT_laffer_HS",chosen.HS,"_fixed_endo.pdf")
      pdf(lafferPDF,width=5,height=5)
      par(mar=c(4,4,1,0.5)+0.1 )
      Endo[,plot(RCR,LT,type = "l",lwd=2,col="maroon",
               xlab= "Regional content requirement (%)",
               ylab = TeX("Change  in regional employment (%)"),
               ylim=range(c(Fixed$LT,Endo$LT)),xaxt="n",las=2)]
      xaxmarks =c(0,25,50,60,75,85,100)
      axis(side = 1, at=xaxmarks,labels=xaxmarks)
      Fixed[,lines(RCR,LT,lwd=2,lty="dashed",col="maroon")]
      abline(h=0,lty="dotted")
      abline(v=75,lty="dashed")
      abline(v=62.5,lty="dashed")
      mtext("NAFTA",side=3,at=62.5,cex=0.8,adj=1)
      mtext("USMCA",side=3,at=75,cex=0.8,adj=0)
      if(abs(min(Fixed$LT))>abs(max(Fixed$LT))) legpos <- "bottomleft" else legpos <- "topleft" 
      legend(legpos,legend=c(TeX("$\\hat{L}^D_T$: fixed location"),TeX("$\\hat{L}^D_T$: endog. location")),
             col = c("maroon","maroon"),lty=c("dashed","solid"),
             bty="n")
      dev.off()
}

# Tables ====
#
# choose which RCR to keep here (can include 0 in RCRkeep for diagnosis, not needed here) 
RCRkeep <- c(62.5,75.0,85.0)
#
# function to read RDS of simulation results
doit <- function(chosen.HS,chosen.kappa_AVE) {
  endoRDS <- paste0("Data/RDS_JIE_rev/kappa",chosen.kappa_AVE,"/XLT_laffer_HS",chosen.HS,"_DRF.rds")
  XLT <- readRDS(endoRDS)
  XLT <- XLT[RCR %in% RCRkeep]
  # X,L,LT need to  renamed to be _DRF variables to distinguish from X,L,LT, which are coming for exog location
  setnames(XLT,c("X","L","LA","LT"),c("X_DRF","L_DRF","LA_DRF","LT_DRF"))
  # add the XLT stuff from the non-DRF (exog location) case, re-using file name
  fixedRDS <- paste0("Data/RDS_JIE_rev/XLT_laffer_HS",chosen.HS,".rds")
  XLTexog <- readRDS(fixedRDS)
  XLTexog <- XLTexog[RCR %in% RCRkeep]
  # merge it on as well with XL non-relocation vars
  XLT <- merge(XLT,XLTexog,by="RCR",all.x=TRUE)
  return(data.table(HS=chosen.HS,V_iso_o="NAFTA",XLT))
}

# Finally iterate over all combinations to extract results and do the table
available.HS <-  c("8703","8704")
iter <- CJ(chosen.HS = available.HS,chosen.kappa_AVE=kappa_AVE)
DT <- Map(doit,iter$chosen.HS,iter$chosen.kappa_AVE)
DT <- rbindlist(DT,fill=TRUE)
# RoO name 
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
#
# Upper (cars) and lower (trucks) panels for the tables.
for(hs in available.HS) {
  # X, L, T main table
  tbl <- paste0("Tables_JIE_rev/kappa",kappa_AVE,"/XL_USMCA_NAFTA_HS",hs,".tex")
  writeLines(DT[HS==hs]$out,tbl)
}

