#create choice and laffer figures for the relocation case
library(HeadR)
library(latex2exp)
rm(list=ls())
cat0 <- function(...) cat(...,sep="") # like paste0, add to HeadR
#
#Kappa should take 3 values : 20 (base), 10 and 30
#
for(kappa_AVE in c(10,20,30)) {
for(chosen.HS in 8703:8704) {
for(chosen.country in c("CAN","MEX","USA")) {
  #
  lafferRDS <- paste0("Data/RDS_JIE_rev/kappa",kappa_AVE,"/XLT_laffer_",chosen.country,"_HS",chosen.HS,"_DRF.rds")
  XL <- readRDS(lafferRDS)
  #final laffer curve figure XLT with relocation ====
  lafferPDF <- paste0("Plots_JIE_rev/AALA_calib/kappa",kappa_AVE,"/XLT_laffer_",chosen.country,"_HS",chosen.HS,"_DRF.pdf")
  pdf(lafferPDF,width=5,height=5)
  par(mar=c(4,4,1,0.5)+0.1 )
  XL[,plot(RCR,X,type = "l",lwd=2,col="black",
           xlab= "Regional content requirement (%)",
           ylab = TeX("Change  in regional content, employment (%)"),
           ylim=range(L,LT,X),xaxt="n",las=2)]
  xaxmarks =c(0,25,50,60,75,85,100)
  axis(side = 1, at=xaxmarks,labels=xaxmarks)
  XL[,lines(RCR,L,type = "l",lwd=2,col="orange")]
  XL[,lines(RCR,LT,type = "l",lwd=2,col="maroon")]
  abline(h=0,lty="dotted")
  abline(v=75,lty="dashed")
  abline(v=62.5,lty="dashed")
  mtext("NAFTA",side=3,at=62.5,cex=0.8,adj=1)
  mtext("USMCA",side=3,at=75,cex=0.8,adj=0)
  if(abs(min(XL$LT))>abs(max(XL$LT))) legpos <- "bottomleft" else legpos <- "topleft" 
  legend(legpos,legend=c(TeX("$\\hat{X}^D$: parts share"),
                         TeX("$\\hat{L}^D$: parts employment"),
                         TeX("$\\hat{L}^D_T$: parts+assembly emp.")),
         col = c("black","orange","maroon"),lty=c("solid","solid","solid"),
         bty="n")
  dev.off()
  #
  #
  cat("Completed HS ",chosen.HS," for country ",chosen.country," and kappa",kappa_AVE ,"\n")
}
#
}
}