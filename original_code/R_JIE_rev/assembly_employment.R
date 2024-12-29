#data from Employment, Hours, and Earnings from the Current Employment Statistics survey (National)
#https://data.bls.gov/timeseries/CEU3133600101?amp%253bdata_tool=XGtable&output_view=data&include_graphs=true
#NAICS Code:	3361,2,3
#L_parts = c(605.9,607.1,605.1,598.9,596.9,598.9,588.0,590.2,590.5,573.4,586.0,587.8)
# mean(L_parts) # 594.0583
library(data.table)
dpath = "Data/Mfg_surveys/USA"
CES.files = list.files(path=dpath,pattern="xlsx")
getsheet <- function(i) {
  DT =  as.data.table(readxl::read_excel(file.path(dpath,CES.files[i]),skip=11))
  idvars = as.matrix(readxl::read_excel(file.path(dpath,CES.files[i]),range="A8:B9",col_names=FALSE))
  cat(idvars,"\n")
  DT = data.table(DT,NAICS=idvars[2,2])
  return(DT)
}

DPm = lapply(1:3,getsheet) |> rbindlist()
DPm = melt(DPm,id.vars=c(1,14),measure.vars = 2:13,variable.name="Month",value.name = "emp_ths")
DPm <- DPm[!is.na(emp_ths)]
#reshape wide, prefixing the NAICS numbers with "E" for employment
DPa <- DPm[,.(emp_ths = mean(emp_ths)),by=.(Year,NAICS)][order(NAICS,Year)]
DPm = dcast(DPm,Year+Month~paste0("E",NAICS),value.var = "emp_ths")
DPm[,ym := lubridate::ymd(paste(Year,Month,15))] 
DPm[,plot(ym,E3363,type="l",ylim=c(100,700),log="y",ylab="Emp (1000s)",xlab="")]
DPm[,lines(ym,E3361,col="blue")]
DPm[,lines(ym,E3362,col="orange")]
legend("topleft",legend=c("Parts","Assembly","Bodies"),col=c("black","blue","orange"),lty="solid")
DPa = dcast(DPa,Year~paste0("E",NAICS),value.var = "emp_ths")
DPa[,zeta_1 := E3361/E3363]
DPa[,zeta_12 := (E3361+E3362)/E3363]
saveRDS(DPa[Year<2020,.(zeta = mean(zeta_12))],"Data/RDS_JIE_rev/zeta.rds")

