library(HeadR)
library(countrycode)
#
rm(list=ls())
# Read in the raw csv files ===
#(saved from the IHS provided xlsb and xlsx files)
options(datatable.integer64="numeric")
# sales data, file save as from the Sales spreadsheet in Cars_Parts
DS <- fread("_Data_notforweb/IHS_sales/Sales2019/Vehicle_Sales_Import_2000_2018.csv",encoding = "UTF-8")

# reshape, rename ====
DF <- melt(DS,id=1:49,measure=patterns("^CY"))
DF[,year := as.integer(substr(variable,4,7))]
#sales data has production and sales brand 
#(we focus on production brand for merge with VP: Production Brand and Nameplate with the powertrain module)
old1=c("VSI: Production Country","VS: Country","VSI: Manufacturer Group","VSI: Production Plant")
new1=c("V_country_o","V_country_d","V_mfr_group","V_plant")
old2 = c("VSI: Production Brand","VSI: Production Nameplate","VS: Sales Brand","VS: Sales Nameplate","VS: Platform","VS: Car/Truck","VS: Production Type","VS: Global Sales Sub-segment","VS: Bodytype")
new2 = c("V_brand","V_model","V_brand_S","V_model_S","V_platform","V_car_truck","V_type","V_subsegment","V_bodytype")
setnames(DF,old=c(old1,old2,"value"),
         new=c(new1,new2,"sales"),skip_absent = TRUE)
DF[,variable := NULL] # from the collapse
# there is one á that was read in UTF-8 as "\x87"
DF[,V_model := gsub("\x87","a",V_model)]
DF[, V_iso_o := countrycode(V_country_o,origin = "country.name",destination = "iso3c")]
DF[, V_iso_d := countrycode(V_country_d,origin = "country.name",destination = "iso3c")]
DF[, V_option := paste(V_iso_o,V_mfr_group,V_plant,sep="_")] # unique

# There are a number of issues in V_option that can be fixed by looking at DPT 
# Fix names of V_option in DPT
DF[V_option %like% "URY_Lifan",V_plant := "San José"]
# better to fix V_mfr_group and V_plant individually rather than V_option
DVopt.fix <- fread("
V_option, V_option_fixed
CZE_N/A_Kolin,CZE_Toyota_Kolin
DEU_N/A_Eisenach,DEU_PSA_Eisenach 
GBR_N/A_Luton IBC #2,GBR_PSA_Luton IBC #2
IDN_Hyundai_Bekasi,IDN_Hyundai_Bekasi I
RUS_N/A_Vsevolozhsk,US_Ford_Vsevolozhsk
THA_Ford_Rayong,THA_Ford_Rayong I
UZB_N/A_Asaka,UZB_General Motors_Asaka 
CHN_Honda_Guangzhou,CHN_Honda_Guangzhou Huangpu
")

DF <- merge_stata(DF, DVopt.fix, by=c("V_option"))
DF[stata_merge==3, V_option:= V_option_fixed]
DF[,c("V_option_fixed","stata_merge") := NULL]
#collapse sales around the chosen identifiers
DSc <- DF[sales>0 & !is.na(V_iso_d) & !is.na(V_iso_o),.(sales = sum(sales)),
           by=.(year,V_option,V_iso_o,V_iso_d,V_mfr_group,V_plant,V_brand,V_model,V_platform,V_type,V_subsegment,V_bodytype)]
saveRDS(DSc,"_Data_notforweb/RDS/sales_clean.rds")



