#----------------------------
# Paper metis USA data
#----------------------------

# Paper Repo: https://github.com/zarrarkhan/paperMetisUSA

#----------------------------
# Plan
#----------------------------
# Plots for water scarcity by Basin, State, County, Grid (Ref, Wat Const, Diff)
# GCAM gives: Wat withdraw by State, Basin, Wat Sup by Basin (Scarcity by Basin can be Calculated)
# Gridded Data: Wat withdraw and supply
# By State, county (need to upscale wat supply), By county need to upscale wat withdraw



#----------------------------
# Load Libraries
#----------------------------

library(metis); library(tidyr); library(dplyr)

#----------------------------
# Global Inputs
#----------------------------


dirOutputs_i = "C:/Z/projects/metisGCAMUSA/metisOutputs"
xRange_i = c(2010,2015,2020,2025)


#------------------------------
# Base Maps: STATES, GCAM BASINS, GCAM LAND, GRIDS 0.25, GRIDS 0.5, COUNTIES, HUC2
#-------------------------------

if(T){
folder_i = "baseMaps"
# GCAM
metis.map(dataPolygon=metis::mapGCAMBasinsUS49,fillColumn = "subRegion",labels=F,
printFig=T,facetsON=F, dirOutputs = dirOutputs_i, fileName="US49GCAMBasin", folderName=folder_i)
metis.map(dataPolygon=metis::mapGCAMBasinsUS49,fillColumn = "subRegion",labels=T,
          printFig=T,facetsON=F, dirOutputs = dirOutputs_i, fileName="US49GCAMBasinLabel", folderName=folder_i)
# metis.map(dataPolygon=raster::crop(metis::mapGCAMLand,metis::mapUS49),fillColumn = "subRegion",labels=F,
# printFig=T,facetsON=F, dirOutputs = dirOutputs_i, fileName="US49GCAMGLU",folderName=folder_i)
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
regionsSelect_i <- c(metis.assumptions()$US52,"USA")

dataGCAM<-metis.readgcam(gcamdatabase = gcamdatabase_i,
                         dataProjFile = dataProjFile_i,
                         scenOrigNames = scenOrigNames_i,
                         scenNewNames = scenNewNames_i,
                         regionsSelect = regionsSelect_i ,
                         paramsSelect=paramsSelect_i,
                         dirOutputs = dirOutputs_i)


unique(dataGCAM$data$scenario)
unique(dataGCAM$data$param)

if(T){ # Check outputs against known data

df <-  dataGCAM$data

#---------------------------------------------
# Check Data:
# Water supply Runoff
# http://www.fao.org/nr/water/aquastat/data/query/results.html
# US - GCAM 3629 km3
# FAO Total renewable surface water = 2900 km3)
(df%>%dplyr::filter(param=="watSupRunoffBasin"))$region%>%unique()
runoffGCAM2015 <- df%>%filter(param=="watSupRunoffBasin", scenario=="WatConst",x==2015,region=="USA")%>% dplyr::summarize(valSum=sum(value,na.rm=T))
runoffGCAM2015

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
  dplyr::group_by(scenario, param, x) %>% dplyr::summarize(valSum=sum(value,na.rm=T)) -> wwithdrawGCAM2015
wwithdrawGCAM2015
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

runoffGCAM2015
wwithdrawGCAM2015

}


#------------------------------
# Maps Process GCAM Data
#-------------------------------

if(T){
# Read Output data from metis.readGCAM aggregated by param
#dfparam <- data.table::fread(paste(dirOutputs_i,"/readGCAM/Tables_gcam/gcamDataTable_aggParam.csv",sep=""))
#dfclass <- data.table::fread(paste(dirOutputs_i,"/readGCAM/Tables_gcam/gcamDataTable_aggClass1.csv",sep=""))
dfparam <- dataGCAM$dataAggParam
dfclass <- dataGCAM$dataAggClass1

unique(dfparam$subRegType);unique(dfparam$param);

#-------------------------------------
# Data by US States - Total

metis.mapsProcess(polygonDataTables=dfparam %>% filter(param %in% c("watWithdrawBySec","watSupRunoffBasin"),
                                                       !subRegion %in% c("AK","PR","HI",
                                                                         "Hawaii","Caribbean","Pacific_and_Arctic_Coast")),
                  xRange=xRange_i,
                  dirOutputs = dirOutputs_i,
                  nameAppend = "total",
                  scenRef = "Ref")

# Waterwithdrawals by Sec by Basins
cols_i <- names(dataGCAM$data)[]
dfparamWatWithBasinParam <- dataGCAM$data%>%
  dplyr::filter(param=="watWithdrawBySec", !region %in% c("AK","PR","HI"), !class2 %in% c("Caribbean","Pacific and Arctic Coast"))%>%
  dplyr::mutate(region=gsub(" ","_",gsub("-","_",class2)),
                subRegion=region,
                param="watWithdrawBySecBasin")%>%
  dplyr::select(scenario, region, subRegion, param, sources, x, xLabel, vintage, units, value,
                aggregate)%>%
  dplyr::group_by(scenario, region, subRegion,    param, sources, x, xLabel, vintage, units,
                  aggregate)%>%dplyr::summarize_at(dplyr::vars("value"),list(~sum(.,na.rm = T)))%>%
  dplyr::ungroup(); head(dfparamWatWithBasinParam)

a<-unique(dfparamWatWithBasinParam$subRegion); b<-unique(metis::mapGCAMBasinsUS49$subRegion)
a[!a %in% b];

metis.mapsProcess(polygonDataTables=dfparamWatWithBasinParam,
                  xRange=xRange_i,
                  dirOutputs = dirOutputs_i,
                  nameAppend = "total",
                  scenRef = "Ref")

}

#------------------------------
# Grid prep downscaled data metis.prepGrid.R
#-------------------------------

if(T){

  gridMetis <- metis.prepGrid (tethysFolders=c("C:/Z/projects/downscaling/tethys/example/Output/gcamMetisUSA_Ref",
                                               "C:/Z/projects/downscaling/tethys/example/Output/gcamMetisUSA_WatConst"),
                  tethysScenarios=c("Ref","WatConst"),
                  tethysUnits=c("km3"),
                  tethysFiles=paste(c("wddom","wdelec","wdirr","wdliv","wdmfg","wdmin","wdnonag","wdtotal"),"_km3peryr",sep=""),
                  dirOutputs = dirOutputs_i,
                  xanthosCoordinatesPath=paste(getwd(),"/dataFiles/grids/xanthosReference/coordinates.csv",sep=""),
                  xanthosGridAreaHecsPath=paste(getwd(),"/dataFiles/grids/xanthosReference/Grid_Areas_ID.csv",sep="")
                  )


  grid_i <- list.files(paste(dirOutputs_i,"/prepGrid/",sep=""),full.names = T);
  grid_i <- grid_i[grepl(".rds",grid_i)];grid_i
  paramScenariosFile <- paste(dirOutputs_i,"/prepGrid/paramScenarios.csv",sep="");
  paramScenarios_i <- data.table::fread(paramScenariosFile);paramScenarios_i
  paramsSelect_i = unique(paramScenarios_i$param); paramsSelect_i
  scenariosSelect_i = unique(paramScenarios_i$scenario); scenariosSelect_i

  g1 <- readRDS(grid_i[1]); head(g1)

}

#------------------------------
# Grid to Poly
#-------------------------------

if(T){

  subRegShape_i = metis::mapUS49County
  subRegCol_i = "subRegion"
  subRegType_i = "county"
  nameAppend_i = "_vol"
  aggType_i = "vol"

  grid2polyX<-metis.grid2poly(gridFiles=grid_i[1],
                              subRegShape =subRegShape_i,
                              subRegCol=subRegCol_i,
                              subRegType = subRegType_i,
                              aggType=aggType_i,
                              nameAppend=nameAppend_i,
                              paramsSelect = paramsSelect_i,
                              scenariosSelect = scenariosSelect_i,
                              #paramScenariosFixed=paramScenarios_i,
                              calculatePolyScarcity=F)

  head(grid2polyX)

  unique(grid2polyX$scenario)

  grid2polyX %>% group_by(param,scenario,units,class,region,x)%>%
    dplyr::summarize(value=sum(value,na.rm=T))

}

#------------------------------
# Plot Grid
#-------------------------------

if(T){}

#------------------------------
# Plot New Polys
#-------------------------------

if(T){}




