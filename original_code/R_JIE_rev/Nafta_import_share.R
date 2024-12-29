library(HeadR)
rm(list=ls())
#
Nafta <- c("USA","CAN","MEX")
# get sales data for share calc ====
DS = readRDS("_Data_notforweb/RDS/sales_clean.rds") #loading IHS sales data (source sales_clean.R)
DS[,foreign := V_iso_o %ni% Nafta]
DS[foreign==TRUE,unique(V_iso_o)]
QN = DS[V_iso_d %in% Nafta,.(sales = sum(sales)),by=.(foreign,year)]
QN = dcast(QN,year~foreign,value.var = "sales")
setnames(QN,old=c("FALSE","TRUE"),new=c("Domestic","Import"))
QN[,importshare := Import/(Import+Domestic)]
QN[order(year),plot(year,importshare,type="l")]
QN[year==2018,importshare]
# 
# Also allowing to compute shares with domestic 
DS = readRDS("_Data_notforweb/RDS/sales_clean.rds") #loading IHS sales data (source sales_clean.R)
DS[, V_reg_o := fcase(
  V_iso_o %in% Nafta , V_iso_o,
  V_iso_o %ni% Nafta , "ROW" # 
)]
#
QN = DS[V_iso_d %in% Nafta,.(sales = sum(sales)),by=.(V_reg_o,year)]
QN = dcast(QN,year~V_reg_o,value.var = "sales")
#
QN[,shCAN:=CAN/(CAN+MEX+USA+ROW)]
QN[,shMEX:=MEX/(CAN+MEX+USA+ROW)]
QN[,shUSA:=USA/(CAN+MEX+USA+ROW)]
QN[,shROW:=ROW/(CAN+MEX+USA+ROW)]
QN[,c("CAN","MEX","USA","ROW") := NULL]
QN[order(year),plot(year,shROW,type="l")]
QN[year==2018,shROW]
round(QN[year==2018],2)
round(QN[year==2019],2)


