library(HeadR)
#
# Load raw data 
DR <- fread("Data/AALA/data_aala_raw.csv")
DR[,table(year)] #
#2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 
#379  347  386  370  432  385  576  640  586  618 
#
# Now starts fixing problems in raw data
#
#-> start out by fixing some shifted columns in 2011 and 2017
#fixes group 1
DR[ source_T1 == "" & source_T2 !="" ,flag := TRUE]
DR[ flag==TRUE ,`:=` (source_T1=source_T2,source_T2="")]
DR[ flag==TRUE & carlines=="Renegade" ,`:=` (final_assembly1="IT")] #Melfi factory, *why* listed as US in "raw"?
DR[ flag==TRUE & carlines=="ATS" ,`:=` (source_T2=source_T1,source_T1=source_E2,source_E2=final_assembly2,final_assembly2="")] #last 4 vars all shifted
DR[,"flag" := NULL] #end of fix group 1
#2 problem w/ columns being shifted from one page of the VW raw data for 2017
DR[ final_assembly1=="" & final_assembly2 != "" & source_E1 =="" ,flag := TRUE]
DR[flag == TRUE,`:=` (final_assembly1=final_assembly2,final_assembly2="",source_E1 = source_E2,source_E2="")]
DR[,"flag" := NULL] #end of fix group 2
#3, page 12 in 2011 has a column shift, need to deal with this in two pieces.
DR[source_E1=="" & final_assembly2 != "" &year==2011, flag := TRUE]
DR[flag == TRUE,`:=` (source_E1=final_assembly2,final_assembly2="",source_T1 = source_E2,source_E2="")]
DR[,"flag" := NULL] 
#same page, special case
DR[year==2011 & carlines=="Routan",flag := TRUE]
DR[flag == TRUE,`:=` (source_E1=final_assembly2,final_assembly2="",source_E2 = source_E1,source_T1 = source_E2)]
DR[,"flag" := NULL] 
#end of fix group 3
#4 Verano, Regal and CTS all appear to be shifted in 2017
DR[ source_E1=="" & final_assembly2 != "" &year==2017, flag :=TRUE]
DR[flag == TRUE,`:=` (source_E1=final_assembly2,final_assembly2="",source_T1 = source_E2,source_E2="")]
DR[flag == TRUE & carlines=="Verano",percent_content_other1 := "15.00% M"] # Verano has MEX content in other years
DR[,"flag" := NULL] 
#5 second source is same as first source
DR[final_assembly1==final_assembly2 & final_assembly1!="", flag := TRUE] 
DR[flag==TRUE,final_assembly2 := ""] # 3 lines for differen engine sizes of F150 in 2020 and 1 line for Toyota Land Cruiser made in Japn
#View(DR[flag==TRUE])
DR[,"flag" := NULL] 
#done with fixes (for now)
DR[,mfg_HQ := "ROW"]
#mfg %like% "^Chrysler" not included since owned by Fiat throughout
DR[  mfg %like% "^General" | mfg=="GM LLC" | mfg %like% "^Ford" | mfg %like% "^Tesla" ,mfg_HQ := "USA" ]
DR[ mfg %like% "^Hyundai" | mfg %like% "^Kia" ,mfg_HQ := "KOR" ]
DR[ mfg %like% "^Toyota" | mfg %like% "^Mazda" | mfg %like% "Honda"| mfg %like% "^Nissan" | mfg %like% "Sukuki"  | mfg %like% "^Fuji" |  mfg %like% "^Subaru" | mfg %like% "^Mitsub",mfg_HQ := "JPN"]
DR[mfg=="PoyrscheAG",mfg := "Porsche AG"] #mfg = manufacturing group
DR[mfg %like% "^Volks" | mfg %like% "^Merc" | mfg %like% "^Porsche"| mfg %like% "^BMW" | mfg %like% "^Audi" ,mfg_HQ := "DEU" ]
# source of transmission and engine
DR[,E_US := source_E1 %like% "US" | source_E2 %like% "US"]
DR[,E_CA := source_E1 %like% "CN" | source_E2 %like% "CN" |
     source_E1 %like% "CAN" | source_E2 %like% "CAN" |
     (source_E1 %like% "^C"  & source_E1 %excludes% "CH")| (source_E2 %like% "^C" & source_E2 %excludes% "CH")]
#View(unique(DR[,.(source_E1,source_E2,E_US,E_CA)])[order(E_US,E_CA)])
DR[,T_US := source_T1 %like% "US" | source_T2 %like% "US"]
DR[,T_CA := source_T1 %like% "CN" | source_T2 %like% "CN" |
     source_T1 %like% "CAN" | source_T2 %like% "CAN" |
     (source_T1 %like% "^C"  & source_T1 %excludes% "CH")| (source_T2 %like% "^C" & source_T2 %excludes% "CH")]
#
DR[,E_MX := source_E1 %like% "MX" | source_E2 %like% "MX" | source_E1 %like% "^M" | source_E2 %like% "^M"]
DR[,T_MX := source_T1 %like% "MX" | source_T2 %like% "MX" | source_T1 %like% "^M" | source_T2 %like% "^M"]
# content shares ====
DR[,us_ca_shr := as.numeric(gsub("%","",percent_content_USA_CAN))]  # NAs by coercion (i think this is expected)
DR[is.na(us_ca_shr),table(percent_content_USA_CAN)]  # there are 52 blanks and one set of 3 values concatenated.
DR[,table(final_assembly1)]
DR[,other1_shr := as.numeric(st_left(percent_content_other1,"%"))]
DR[,other1_who := st_right(percent_content_other1,"%")]
#hand for full of data not in %
DR[percent_content_other1 %excludes% "%",other1_shr := round(100*as.numeric(substr(percent_content_other1,1,4)),0)]
DR[percent_content_other1 %excludes% "%",other1_who := substr(percent_content_other1,5,6)]
DR[other1_who %like% "^M",table(other1_who)]
DR[other1_who %like% "M",table(other1_who)]  # picks up additional cases that look like mexico
# 
DR[percent_content_other2!="",.N] # 1335 obs
DR[percent_content_other2 %like% "M",table(percent_content_other2)] # these all look like Mexico
DR[,other2_shr := as.numeric(st_left(percent_content_other2,"%"))]
DR[,summary(other2_shr)]
DR[,other2_who := st_right(percent_content_other2,"%")]
DR[other2_who %like% "M",table(other2_who)]
DR[other2_who %like% "M" & is.na(other2_shr)]
#one fix for Hyundai Accent and Hyundai Accent (Manual Transmission) in 2018 where columns are messed up
DR[carlines == "Accent" & year ==2018,`:=`(other1_shr = 47,other1_who = "M",other2_who = "")]
DR[carlines == "Accent (Manual Transmission)" & year ==2018,`:=`(other1_shr = 44,other1_who = "M",other2_who = "")]
#
# Now starts encoding of assembly 1 and assembly 2
#
# fix country codes
US <- c("US","USA")
CA <- c("CN","C","CAN")
USCA <- c(CA,US,"US, CN")  #AALA doesn't do ISO...
MX <- c("M","MX")
USCAMX <- c(USCA,MX,"M, US")
# final assembly location 2
MX2 <- DR[final_assembly2 %like% "M",unique(final_assembly2)]
CA2 <- DR[final_assembly2 %like% "C" & final_assembly2 != "M (Reg\nCab)",unique(final_assembly2)]
US2 <- DR[final_assembly2 %like% "US" ,unique(final_assembly2)] #caution: %like% "U" includes UK
USCAMX2 <- union(union(US2,CA2),MX2) # union instead of c() because repeats e.g "CN, US"
#
DR[ final_assembly1 %in%  USCAMX,summary(us_ca_shr)]
DR[ final_assembly2 %in%  USCAMX2,summary(us_ca_shr)]
DR[other1_who %like% "M" &other2_who %like% "M",.N] # mex is never listed under BOTH others.
DR[other1_who %like% "M", mx_shr :=  other1_shr]
DR[final_assembly1 %in%  USCAMX,summary(mx_shr)] #
DR[final_assembly1 %in%  MX,summary(mx_shr)] #
DR[other2_who %like% "M", mx_shr :=  other2_shr]
DR[final_assembly1 %in%  MX,summary(mx_shr)] # from 40 NAs down to just 8.
DR[final_assembly2 %in%  MX2,summary(mx_shr)] # 15 NAs
# from 40 NAs down to just 8.
#
#Nafta assembly location 1 countries
DR[,ell := fcase(
  final_assembly1 %in% US, "US",
  final_assembly1 %in% CA, "CA",
  final_assembly1 %in% MX, "MX",
  final_assembly1 %ni% USCAMX, "ROW"
)]
DR[ell %in% c("CA","US","MX") & is.na(us_ca_shr),.N]  #2
DR[ell %ni% c("CA","US","MX") & is.na(us_ca_shr),.N]  #51
DR[ell %ni% c("CA","US","MX") & !is.na(us_ca_shr),.N]#2949
DR <- DR[!is.na(us_ca_shr)]# drop the missing us_ca_shr data
#
# Now starts the treatment of second assembly sites that are in NAFTA
#
# second assembly location logicals
DR[,ell2CA :=   final_assembly2 %in% CA2]
DR[,ell2US :=   final_assembly2 %in% US2]
DR[,ell2MX :=   final_assembly2 %in% MX2]
# melt in the style of a Stata reshape long. Note that the expands dataset by the number of countries : 4666*3 = 13998
D2 <- melt(DR,measure=patterns("^ell2"),value.name="add_plant",variable.name ="ell2",variable.factor = FALSE)
D2[, ell2 := gsub("ell2","",ell2)]
D2 <- D2[add_plant==TRUE] # just keep the non-primary (added) sites (122)
# drop primary assembly site (ell) and then rename the additional sites as ell, so we can append
D2[, "ell" := NULL] 
setnames(D2,"ell2","ell")
# get rid of the location 2 logicals
cols_to_delete <- grep("^ell2", names(DR), value = TRUE) # removes all the ell2* columns without naming them
DR[, (cols_to_delete) := NULL]
#append the additional sites, fill in add_plant as false for the final_assembly1 sites: 4788 obs now : 4666+122
DR <- rbind(DR,D2,fill=TRUE)
DR[is.na(add_plant),add_plant := FALSE]
#
# Now starts the (complex) bounding of Mexican share (which is not a variable per se)
#
# there are two is.na(ell), that have multiple assembly countries in north america
DR[,.(meanmx = mean(mx_shr,na.rm=TRUE),meanusca = mean(us_ca_shr,na.rm=TRUE)) ,by=ell]  
# none of these will catch cases where ell2 %in% c("US","CA","MX") but but ell (location 1) is not
nafta_assembly <- substitute(ell %in% c("US","CA","MX")  )
DR[eval(nafta_assembly) & is.na(other1_shr) , rem_shr := 100- us_ca_shr]
DR[eval(nafta_assembly)  & !is.na(other1_shr) & other1_who %like% "M" , rem_shr := 100- us_ca_shr]
DR[eval(nafta_assembly) & !is.na(other1_shr) &  !(other1_who %like% "M") , rem_shr := 100- us_ca_shr-other1_shr]
DR[eval(nafta_assembly), mx_shr_rem := mx_shr/rem_shr]
DR[eval(nafta_assembly),summary(mx_shr_rem)]
con.means <- DR[eval(nafta_assembly) & mx_shr_rem<1,.(mx_shr_rem_mn = mean(mx_shr_rem,na.rm=TRUE)),by=ell]  
# use this later for lib
DR[eval(nafta_assembly),median(mx_shr_rem,na.rm=TRUE),by=ell]  #medians are similar
#we use the these medians to justify why we set MX at 0.5 of the remaining share
mx_md_mx <- DR[ell == "MX",median(mx_shr,na.rm=TRUE)]
DR <- merge_stata(DR,con.means,by="ell")
DR[, "stata_merge" := NULL]
# fix mexico shares, with conservative (con) and liberal (lib) assumptions
DR[,mx_shr_con := mx_shr]
DR[is.na(mx_shr), mx_shr_con :=0] # conservative guess is zero
DR[, mx_shr_lib := mx_shr_con]
# liberal assumption for mexican assembly 
DR[eval(nafta_assembly) & is.na(mx_shr) & is.na(other1_shr), mx_shr_lib := (100- us_ca_shr)*mx_shr_rem_mn]
DR[eval(nafta_assembly) & is.na(mx_shr) & !is.na(other1_shr), mx_shr_lib := (100- us_ca_shr-other1_shr)*mx_shr_rem_mn]
DR[eval(nafta_assembly) & is.na(mx_shr) & mx_shr_lib>=15, mx_shr_lib := 14] #cap it at reporting threshold
# do the same for ell2? add it as an "or" in lines 130-132, see what TM thinks
it = CJ(ell = c("US","CA","MX"),assump=c("con","lib"))
sumit =function(x,y) DR[ell ==x,summary(get(paste0("mx_shr_",y)))]
sink(file="Tables_JIE_rev/nafta_shr_lib_con_rev.txt")
writeLines("Mexico shares by assembly location, conservative and then liberal")
Map(sumit,it$ell,it$assump)
DR[,nafta_shr_lib := us_ca_shr + mx_shr_lib]
DR[,nafta_shr_con := us_ca_shr + mx_shr_con]
sumit = function(x,y) DR[ell ==x,summary(get(paste0("nafta_shr_",y)))]
writeLines("Nafta shares by assembly location")
Map(sumit,it$ell,it$assump)
sink()
#
DR <- DR[,.(year,mfg,makes, carlines, type, final_assembly1,ell,final_assembly2,add_plant,us_ca_shr,mx_shr,nafta_shr_con,nafta_shr_lib,mfg_HQ)]
saveRDS(DR,"Data/RDS_JIE_rev/AALA_rev.rds")
