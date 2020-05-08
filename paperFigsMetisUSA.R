#----------------------------
# Paper metis USA data
#----------------------------

# Paper Repo: https://github.com/zarrarkhan/paperMetisUSA


#----------------------------
# Load Libraries
#----------------------------

library(metis); library(tidyr); library(dplyr)

#----------------------------
# Global Directories
#----------------------------


dirOutputs_i = "C:/Z/projects/metisGCAMUSA/metisOutputs"


#------------------------------
# Base Maps: STATES, GCAM BASINS, GCAM LAND, GRIDS 0.25, GRIDS 0.5, COUNTIES, HUC2
#-------------------------------

if(T){
folder_i = "baseMaps"
# GCAM
metis.map(dataPolygon=raster::crop(metis::mapGCAMBasins,metis::mapUS49),fillColumn = "subRegion",labels=F,
printFig=T,facetsON=F, dirOutputs = dirOutputs_i, fileName="US49GCAMBasin", folderName=folder_i)
metis.map(dataPolygon=raster::crop(metis::mapGCAMBasins,metis::mapUS49),fillColumn = "subRegion",labels=T,
          printFig=T,facetsON=F, dirOutputs = dirOutputs_i, fileName="US49GCAMBasinLabel", folderName=folder_i)
metis.map(dataPolygon=raster::crop(metis::mapGCAMLand,metis::mapUS49),fillColumn = "subRegion",labels=F,
printFig=T,facetsON=F, dirOutputs = dirOutputs_i, fileName="US49GCAMGLU",folderName=folder_i)
# US
metis.map(dataPolygon=metis::mapUS49,fillColumn = "subRegion",labels=F,
          printFig=T,facetsON=F, dirOutputs = dirOutputs_i, fileName="US49State",folderName=folder_i)
metis.map(dataPolygon=metis::mapUS49County,fillColumn = "subRegion",labels=F,
          printFig=T,facetsON=F, dirOutputs = dirOutputs_i, fileName="US49County",folderName=folder_i)
# USGS HUC
metis.map(dataPolygon=raster::crop(metis::mapUS49HUC2,metis::mapUS49),fillColumn = "subRegion",labels=F,
          printFig=T,facetsON=F, dirOutputs = dirOutputs_i, fileName="US49HUC2",folderName=folder_i)
metis.map(dataPolygon=raster::crop(metis::mapUS49HUC4,metis::mapUS49),fillColumn = "subRegion",labels=F,
          printFig=T,facetsON=F, dirOutputs = dirOutputs_i, fileName="US49HUC4",folderName=folder_i)
# Plot the grids over states
}

#------------------------------
# GCAM data (Water-Energy-Food by States-Basin-GLU)
#-------------------------------

if(T){

gcamdatabase_i <-paste("C:/Z/projects/metisGCAMUSA/gcam-core/output/metisUSAOld",sep="")
#rgcam::localDBConn("C:/Z/projects/metisGCAMUSA/gcam-core/output","metisUSAOld") # Note names of scenarios
dataProjFile_i <- "metisUSA_dataProj.proj"
scenOrigNames_i <-c("GCAMUSARef","GCAMUSAWatConst")
scenNewNames_i <-c("Ref","WatConst")
paramsSelect_i <- c("elecByTechTWh", "elecCapByFuel", "pop",
                    "watWithdrawByCrop","watWithdrawBySec","watConsumBySec","watSupRunoffBasin",
                 "landAlloc","landAllocByCrop")
regionsSelect_i <- c(metis.assumptions()$US52,"USA", "Pakistan")

dataGCAM<-metis.readgcam(#gcamdatabase = gcamdatabase_i,
                         dataProjFile = dataProjFile_i,
                         scenOrigNames = scenOrigNames_i,
                         scenNewNames = scenNewNames_i,
                         regionsSelect = regionsSelect_i ,
                         paramsSelect=paramsSelect_i,
                         dirOutputs = dirOutputs_i)




df <- dataGCAM$data
unique(df$scenario)
unique(df$param)

if(F){ # Check outputs against known data
#---------------------------------------------
# Check Data:
# Water supply Runoff
# http://www.fao.org/nr/water/aquastat/data/query/results.html
# US - GCAM 3629 km3
# FAO Total renewable surface water = 2900 km3)
(df%>%dplyr::filter(param=="watSupRunoffBasin"))$region%>%unique()
df%>%filter(param=="watSupRunoffBasin", scenario=="WatConst",x==2015,region=="USA")%>% dplyr::summarize(valSum=sum(value,na.rm=T))

#------------------------------------------------
# Water Demands (US/CA/TX)
# GCAM 2015 WatConst
# Total = 384 km3
# US Ag + livestock = 138 + 2.93 = 140.93 km3
# US industry, electricity, mining = 21.1 + 156 + 4.97 = 182.07 km3
# US municipal = 61.7 km3
# FAO water demands US 2015
# http://www.fao.org/nr/water/aquastat/data/query/results.html
# Total = 450 km3
# Ag = 176.2 km3
# Industrial = 209.7 km3
# Municpal = 58.93 km3
(df%>%dplyr::filter(param=="watWithdrawBySec"))$region%>%unique()
df%>%filter(param=="watWithdrawBySec", scenario=="WatConst",x==2015,region %in% metis.assumptions()$US52)%>%
  dplyr::group_by(scenario, param, x) %>% dplyr::summarize(valSum=sum(value,na.rm=T))
df%>%filter(param=="watWithdrawBySec", scenario=="WatConst",x==2015,region %in% metis.assumptions()$US52)%>%
  dplyr::group_by(scenario, param, class1,x) %>% dplyr::summarize(valSum=sum(value,na.rm=T))
# Check Water withdrawals by Crops summed for US
df%>%filter(param=="watWithdrawByCrop", scenario=="WatConst",x==2015,region=="USA")%>%
  dplyr::group_by(scenario, param, x) %>% dplyr::summarize(valSum=sum(value,na.rm=T))


#--------------------------------------------------
# Electricity Generation (US/CA/TX)
# GCAM US 2015 = 4348 TWh
# GCAM TX 2015 = 446 TWh
# GCAM CA 2015 = 200 TWh
# EIA
# US 2018 = 4180 TWh https://www.eia.gov/electricity/state/unitedstates/
# TX 2018 = 477 TWh https://www.eia.gov/electricity/state/Texas/index.php
# CA 2018 = 195 TWh https://www.eia.gov/electricity/state/california/index.php
df%>%dplyr::filter(param=="elecByTechTWh")%>%as.data.frame()%>%head()
(df%>%dplyr::filter(param=="elecByTechTWh"))$region%>%unique()
df%>%filter(param=="elecByTechTWh",scenario=="WatConst",x==2015)%>%dplyr::summarize(valSum=sum(value,na.rm=T))
df%>%filter(param=="elecByTechTWh",scenario=="WatConst",x==2015, region=="TX")%>%dplyr::summarize(valSum=sum(value,na.rm=T))
df%>%filter(param=="elecByTechTWh",scenario=="WatConst",x==2015, region=="CA")%>%dplyr::summarize(valSum=sum(value,na.rm=T))

#--------------------------------------------
# Electricity Consumption (US/CA/TX)


#-------------------------------------------
# Agriculture Production (US/CA/TX)
# Agriculture Demand (US/CA/TX)
# Food Production (US/CA/TX)
# Food demand (US/CA/TX)
} # Check outputs against known data

}


#------------------------------
# Maps Process GCAM Regions
#-------------------------------

# Read Output data from metis.readGCAM aggregated by param
dfparam <- data.table::fread(paste(dirOutputs_i,"/readGCAM/Tables_gcam/gcamDataTable_aggParam.csv",sep=""))
# Read Output data from metis.readGCAM aggregated by class
dfclass <- data.table::fread(paste(dirOutputs_i,"/readGCAM/Tables_gcam/gcamDataTable_aggClass1.csv",sep=""))


#-------------------------------------
# Data by GCAMBasin (watSupRunoff)

# Plot Maps
metis.mapsProcess(polygonDataTables=dfparam,
                  subRegShape=metis::mapGCAMBasinUS49,
                  xRange=c(2010,2020),
                  dirOutputs = dirOutputs_i,
                  extension =T)

#------------- Data by States

# Shape Data
shapex <- metis::mapUS49  # Shape file Crop GCAM basins to US49 border
shapex@data$subRegion%>%unique();

# Prep Data
dfx <- df %>% dplyr::filter(param %in% c('watWithdrawBySec','elecByTechTWh',"watConsumBySec")) %>%
  dplyr::select(scenario, subRegion=region, param, class=class1, x, value,
                units, classPalette=classPalette1, classLabel=classLabel1) %>%
  dplyr::mutate(class="total") %>%
  dplyr::group_by(scenario, subRegion, param, class, x,
                  units, classPalette, classLabel) %>%
  dplyr::summarize(value=sum(value,na.rm=T))%>%
  dplyr::ungroup() %>%
  dplyr::bind_rows(df %>% dplyr::filter(param %in% c('pop',"gdpPerCapita")) %>%
                     dplyr::select(scenario, subRegion=region, param, class=class1, x, value,
                                   units, classPalette=classPalette1, classLabel=classLabel1))


#dfx <- df %>% dplyr::filter(param %in% c('pop',"gdpPerCapita","watWithdrawBySec","watConsumBySec",
#                                         "elecByTechTWh")) # GCAM data
dfx <- dfx %>% dplyr::filter(subRegion!="USA")
dfx <- dfx  %>%
  dplyr::mutate(classPalette=case_when(param=="pop"~"pal_hot",
                                       param=="watWithdrawBySec"~"pal_wet",
                                       param=="gdpPerCapita"~"pal_hot",
                                       param=="elecByTechTWh"~"pal_hot",
                                       param=="watConsumBySec"~"pal_wet",
                                       TRUE~classPalette))
dfx$subRegion%>%unique(); dfx$scenario%>%unique(); dfx$param%>%unique()
# Plot Maps

scenRefDiffIndv_i = list(param=list(c('pop',"gdpPerCapita","watWithdrawBySec","watConsumBySec",
                                        "elecByTechTWh")),
                         scenRef=list(c("Ref")),
                         scenDiff=list(c("WatConst")),
                         scenIndv=list(c("Ref","WatConst")))

metis.mapsProcess(polygonDataTables=dfx,
                  xRange=c(2010,2020,2030,2040,2050),
                  folderName="metisUSA",
                  subRegShape=shapex,
                  subRegCol="subRegion",
                  nameAppend="_US49",
                  animateOn=F,
                  fps=1,
                  extension=F,
                  #scenRefDiffIndv=scenRefDiffIndv_i,
                  diffOn = F)


#----------------------- Data by GCAM Land

df$param%>%unique()
# Prep Data
dfx <- df %>% dplyr::filter(param=="landAllocByCrop") # GCAM data
shapex <- raster::crop(metis::mapGCAMLand,metis::mapUS49)  # Shape file Crop GCAM basins to US49 border
# Harmonize Basin Names
shapex@data <- shapex@data %>% droplevels()
shapex@data$subRegion%>%unique();
dfx$region%>%unique()
dfx <- dfx %>% dplyr::mutate(region=gsub(" ","_",region),region=paste(region,"_Basin",sep=""),
                             classPalette1="pal_wet")
dfx <- dfx %>% dplyr::select(scenario, subRegion=region, param, class=class1, x, value,
                             units, classPalette=classPalette1, classLabel=classLabel1)
dfx$subRegion%>%unique(); dfx$scenario%>%unique()
# Plot Maps

metis.mapsProcess(polygonDataTables=dfx,
                  xRange=c(2010,2020,2030,2040,2050),
                  folderName="metisUSA",
                  subRegShape=shapex,
                  subRegCol="subRegion",
                  nameAppend="",
                  animateOn=T,
                  fps=1,
                  extension=F,
                  diffOn = F)



#------------------------------
# Downscaled  (Xanthos/Tethys/BIa/Demeter)
#-----------------------------


#------------------------------
# Upscaled (Counties, HUC2)
#-----------------------------


#------------------------------
# I/O
#-----------------------------
