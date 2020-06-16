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
# Load Libraries and initial inputs
#----------------------------

if(T){

  library(metis); library(tidyr); library(dplyr)

  dirOutputs_i = "C:/Z/projects/metisGCAMUSA/metisOutputsSSP"
  xRange_i = c(2010,2050)
  gcamdatabase_i <-paste("C:/Z/projects/gcam-v5.2-Windows-Release-Package/output/exampleSSP2100",sep="")
  #rgcam::localDBConn("C:/Z/projects/metisGCAMUSA/gcam-core/output","metisUSAOld") # Note names of scenarios
  dataProjFile_i <- "SSP2100_dataProj.proj"
  dataProjFile_saved <- "C:/Z/projects/metisGCAMUSA/metisOutputsSSP/readGCAM/SSP2100_dataProj.proj"
}

#------------------------------
# Base Maps: STATES, GCAM BASINS, GCAM LAND, GRIDS 0.25, GRIDS 0.5, COUNTIES, HUC2
#-------------------------------

if(F){
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

if(F){

paramsSelect_i <- c("elecByTechTWh", "elecCapByFuel", "pop", #"elecFinalbySecTWh",
                    "watWithdrawByCrop","watWithdrawBySec","watConsumBySec","watSupRunoffBasin",
                 "landAlloc","landAllocByCrop")
regionsSelect_i <- c("USA")

dataGCAM<-metis.readgcam(#gcamdatabase = gcamdatabase_i,
                         dataProjFile = dataProjFile_saved,
                         regionsSelect = regionsSelect_i ,
                         paramsSelect=paramsSelect_i,
                         dirOutputs = dirOutputs_i)

unique(dataGCAM$data$scenario)
unique(dataGCAM$data$param)
unique(dataGCAM$data$x)
dataGCAM$data%>%filter(x==2050,param=="watWithdrawBySec")

if(F){ # Check outputs against known data

df <-  dataGCAM$data

#---------------------------------------------
# Check Data:
# Water supply Runoff
# http://www.fao.org/nr/water/aquastat/data/query/results.html
# US - GCAM 3629 km3
# FAO Total renewable surface water = 2900 km3)
(df%>%dplyr::filter(param=="watSupRunoffBasin"))$region%>%unique()
df%>%filter(param=="watSupRunoffBasin", x %in% c(2015,2040),region=="USA")%>%
  dplyr::group_by(scenario,x,param)%>%
  dplyr::summarize(valSum=sum(value,na.rm=T))


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
df%>%filter(param=="watWithdrawBySec",x %in% c(2015,2040),region %in% "USA")%>%
  dplyr::group_by(scenario, param, x) %>% dplyr::summarize(valSum=sum(value,na.rm=T))
df%>%filter(param=="watWithdrawBySec", x %in% c(2015,2040),region %in% "USA")%>%
  dplyr::group_by(scenario, param, class1,x) %>% dplyr::summarize(valSum=sum(value,na.rm=T))

#---------------------------------------------
# Electricity Generation (US/CA/TX)
# US: 4000 TWh https://www.eia.gov/electricity/monthly/epm_table_grapher.php?t=epmt_1_01
# CA: 196 TWh https://www.eia.gov/electricity/data/state/
# TX: 450 TWh https://www.eia.gov/electricity/data/state/

} # Check outputs against known data


}


#------------------------------
# Run Downscaling tools independently
#-------------------------------

if(T){

  # Bia -------------------------------
  if(F){

    regionsSelect_i <- c("USA")
    gridChoice_i = "grid_050" # Default = "grid_050"
    diagnosticsON_i = T
    subsectorNAdistribute_i = "even"
    nameAppend_i=paste("_even",sep="")

    dataBia<-metis.bia(
      dirOutputs = dirOutputs_i,
      regionsSelect=regionsSelect_i, # Default Value is NULL
      dataProjFile=dataProjFile_saved, # Default Value is "dataProj.proj"
      #gcamdatabase=gcamdatabase_i,
      gridChoice = gridChoice_i, # Default = "grid_050"
      diagnosticsON = diagnosticsON_i,
      subsectorNAdistribute = subsectorNAdistribute_i,
      nameAppend=nameAppend_i)

    # dirOutputs = dirOutputs_i
    # regionsSelect=regionsSelect_i # Default Value is NULL
    # dataProjFile=dataProjFile_saved # Default Value is "dataProj.proj"
    # #gcamdatabase=gcamdatabase_i,
    # gridChoice = gridChoice_i # Default = "grid_050"
    # diagnosticsON = diagnosticsON_i
    # subsectorNAdistribute = subsectorNAdistribute_i
    # nameAppend=nameAppend_i

    dataBia$scenario%>%unique()

  }

  # Tethys -------------------------------
  if(F){
  system('python "C:/Z/projects/downscaling/tethys/example/example_GCAMSSP2.py"')
  system('python "C:/Z/projects/downscaling/tethys/example/example_GCAMSSP3.py"')
  system('python "C:/Z/projects/downscaling/tethys/example/example_GCAMSSP5.py"')
  }

  # Xanthos -------------------------------
  # https://docs.google.com/document/d/1NuUg9LB6qnHRr8VncV4x-orAcaCsYEvqU5JOs5cJKys/edit#heading=h.cssp0x15n55k


  # Electricity Demand Downscale -------------------------------
  if(T){

    elecDem <- data.table::fread(paste(dirOutputs_i,"/readGCAM/Tables_gcam/gcamDataTable_aggParam.csv",sep=""))
    unique(elecDem$param)

    elecDem <- elecDem %>%
      dplyr::filter(param=="elecByTechTWh", region=="USA")%>%
      dplyr::mutate(param="elecDemTWh"); elecDem

    downScaleElec <- metis::metis.downscale(polygonTable = elecDem,
                                            gridTable = "pop",
                                            dirOutputs = dirOutputs_i)
  }

  }

#------------------------------
# Grid prep downscaled data metis.prepGrid.R
#-------------------------------

if(T){

  gridMetis <- metis.prepGrid (
                               downscaleFolder=paste(dirOutputs_i,"/downscale",sep=""),
                               downscaleFiles="downscale_elecDemTWh.csv",
                               # biaFolder=paste(dirOutputs_i,"/Bia",sep=""),
                               # biaFiles="biaOutput_elecByTechTWh_total_even.csv",
                               # tethysFolders=c("C:/Z/projects/downscaling/tethys/example/Output/gcamSSP2_2100",
                               #                 "C:/Z/projects/downscaling/tethys/example/Output/gcamSSP3_2100",
                               #                 "C:/Z/projects/downscaling/tethys/example/Output/gcamSSP5_2100"),
                               # tethysScenarios=c("GCAM_SSP2","GCAM_SSP3","GCAM_SSP5"),
                               # tethysUnits=c("km3"),
                               # tethysFiles=paste(c("wdtotal"),"_km3peryr",sep=""),
                               # xanthosFiles=c(
                               #   "C:/Z/projects/downscaling/xanthos/example/Output/pm_abcd_mrtm_watch_wfdei_1970_2010/q_km3peryear_pm_abcd_mrtm_watch_wfdei_1970_2010.csv"
                               #   ),
                               # xanthosScenarios=c("Ref"),
                               dirOutputs = dirOutputs_i,
                               xanthosCoordinatesPath=paste(getwd(),"/dataFiles/grids/xanthosReference/coordinates.csv",sep=""),
                               xanthosGridAreaHecsPath=paste(getwd(),"/dataFiles/grids/xanthosReference/Grid_Areas_ID.csv",sep=""),
                               filterYears = c(1990:2050))

  # xanthosFiles=c(
  #   "C:/Z/projects/downscaling/xanthos/example/Output/pm_abcd_mrtm_watch_wfdei_1970_2010/q_km3peryear_pm_abcd_mrtm_watch_wfdei_1970_2010.csv"
  # )
  # xanthosScenarios=c("Ref")
  # dirOutputs = dirOutputs_i
  # xanthosCoordinatesPath=paste(getwd(),"/dataFiles/grids/xanthosReference/coordinates.csv",sep="")
  # xanthosGridAreaHecsPath=paste(getwd(),"/dataFiles/grids/xanthosReference/Grid_Areas_ID.csv",sep="")
  # filterYears = xRange_i
}

#------------------------------
# Grid to Poly
#-------------------------------

if(T){

  grid_i <- list.files(paste(dirOutputs_i,"/prepGrid/",sep=""),full.names = T);
  grid_i <- grid_i[grepl(".rds",grid_i)];grid_i
  grid_i <- grid_i[grepl("downscale",grid_i)]; grid_i
  #grid_i <- grid_i[grepl("total",grid_i)];grid_i
  paramScenariosFile <- paste(dirOutputs_i,"/prepGrid/paramScenarios.csv",sep="");
  paramScenarios_i <- data.table::fread(paramScenariosFile)%>%as.data.frame();paramScenarios_i
  paramsSelect_i = unique(paramScenarios_i$param); paramsSelect_i
  scenariosSelect_i = unique(paramScenarios_i$scenario); scenariosSelect_i

  g1 <- readRDS(grid_i[1]);
  head(g1%>%as.data.frame())

  aggType_i = "depth"

  # By State
  subRegShape_i = metis::mapUS49
  subRegCol_i = "subRegion"
  subRegType_i = "state"
  nameAppend_i = "_state"
  grid2polyX<-metis.grid2poly(gridFiles=grid_i,
                              subRegShape =subRegShape_i,
                              subRegCol=subRegCol_i,
                              subRegType = subRegType_i,
                              aggType=aggType_i,
                              nameAppend=nameAppend_i,
                              paramsSelect = paramsSelect_i,
                              scenariosSelect = scenariosSelect_i,
                              #paramScenariosFixed=paramScenarios_i,
                              dirOutputs = dirOutputs_i)


  # By BASIN
  subRegShape_i = metis::mapGCAMBasinsUS49
  subRegCol_i = "subRegion"
  subRegType_i = "GCAMBasinUS49"
  nameAppend_i = "_GCAMBasinUS49"
  grid2polyX<-metis.grid2poly(gridFiles=grid_i,
                              subRegShape =subRegShape_i,
                              subRegCol=subRegCol_i,
                              subRegType = subRegType_i,
                              aggType=aggType_i,
                              nameAppend=nameAppend_i,
                              paramsSelect = paramsSelect_i,
                              scenariosSelect = scenariosSelect_i,
                              #paramScenariosFixed=paramScenarios_i,
                              dirOutputs = dirOutputs_i)

  # By County
  subRegShape_i = metis::mapUS49County
  subRegCol_i = "subRegion"
  subRegType_i = "county"
  nameAppend_i = "_county"
  grid2polyX<-metis.grid2poly(gridFiles=grid_i,
                              subRegShape =subRegShape_i,
                              subRegCol=subRegCol_i,
                              subRegType = subRegType_i,
                              aggType=aggType_i,
                              nameAppend=nameAppend_i,
                              paramsSelect = paramsSelect_i,
                              scenariosSelect = scenariosSelect_i,
                              #paramScenariosFixed=paramScenarios_i,
                              dirOutputs = dirOutputs_i)


  # gridFiles=grid_i[1]
  # subRegShape =subRegShape_i
  # subRegCol=subRegCol_i
  # subRegType = subRegType_i
  # aggType=aggType_i
  # nameAppend=nameAppend_i
  # paramsSelect = paramsSelect_i
  # scenariosSelect = scenariosSelect_i
  # #paramScenariosFixed=paramScenarios_i,
  # dirOutputs = dirOutputs_i

}

#------------------------------
# Plot Params by Poly
#-------------------------------

if(T) {

  # List of Poly Files
  data_i <- list.files(paste(dirOutputs_i, "/Grid2Poly/", sep = ""), full.names = T); data_i
  data_i <- data_i[grepl(".csv", data_i)];  data_i
  data_i <- data_i[grepl("poly", data_i)];  data_i
  data_i <- data_i[grepl("elecDem", data_i)];  data_i
  data_i <- data_i[!grepl("template", data_i, ignore.case = T)];  data_i

  d1 <- data.table::fread(data_i[1])
  head(d1 %>% as.data.frame())
  unique(d1$scenario)
  unique(d1$param)

  # Read in All files
  dpolyx <- tibble::tibble()
  for (j in data_i) {
    dpolyx_j <- data.table::fread(j)
    dpolyx <- dpolyx %>%
      dplyr::bind_rows(dpolyx_j)
  }

  dpolyx
  unique(dpolyx$scenario)
  unique(dpolyx$param)

  # Get Range by SubRegType
  dpolyRange <- dpolyx %>%
    dplyr::filter(x %in% c(2010, 2015, 2050)) %>%
    dplyr::group_by(param, subRegType) %>%
    dplyr::summarize(max = round(max(value, na.rm =T)), min = round(min(value, na.rm = T)))%>%
    dplyr::ungroup()

  dpolyRange

  # Plot for Each SubRegType
  for (i in unique(dpolyx$subRegType)) {
    dpolyx_i <- dpolyx %>% dplyr::filter(subRegType == i)
    dpolyx
    dpolyRange_i <-
      dpolyRange %>% dplyr::filter(subRegType == i)
    dpolyRange

    paramMaps <- metis.mapsProcess(
      polygonTable = dpolyx_i,
      xRange = c(2010, 2015, 2050),
      dirOutputs = dirOutputs_i,
      nameAppend = "params",
      scenRef = "GCAM_SSP2",
      legendFixedBreaks = 7,
      scaleRange = dpolyRange_i,
      scaleRangeDiffPrcnt = c(-80, 80)
    )
  }
}

#----------------------------------------------------
# Calculate Indices
#---------------------------------------------------

if(T){

  # Params and Scenarios
  paramScenariosFile <- paste(dirOutputs_i,"/prepGrid/paramScenarios.csv",sep="");
  paramScenarios_i <- data.table::fread(paramScenariosFile)%>%as.data.frame();paramScenarios_i
  paramsSelect_i = unique(paramScenarios_i$param); paramsSelect_i
  scenariosSelect_i = unique(paramScenarios_i$scenario); scenariosSelect_i

  # List of Poly Files
  data_i <- list.files(paste(dirOutputs_i, "/Grid2Poly/", sep = ""), full.names = T); data_i
  data_i <- data_i[grepl(".csv", data_i)];  data_i
  data_i <- data_i[grepl("poly", data_i)];  data_i
  #data_i <- data_i[grepl("elecDem", data_i)];  data_i
  data_i <- data_i[!grepl("template", data_i, ignore.case = T)];  data_i

  d1 <- data.table::fread(data_i[1])
  head(d1 %>% as.data.frame())
  unique(d1$scenario)
  unique(d1$param)

  # Read in All files
  dpolyx <- tibble::tibble()
  for (j in data_i) {
    dpolyx_j <- data.table::fread(j)
    dpolyx <- dpolyx %>%
      dplyr::bind_rows(dpolyx_j)
  }

  dpolyx
  unique(dpolyx$scenario)
  unique(dpolyx$param)

  # Water Scarcity
  # By SubReg Type (Basin (GCAM), State (Gird2Poly), County(Gird2Poly)), Withdrawals, Supply, Scarcity
  dfPoly <- metis::metis.index(
    data=dpolyx,
    numerators=c("tethysWatWithdraw_total"),
    denominators=c("xanthosRunoff"),
    indexName="scarcityWat",
    nameAppend="wat",
    saveFile=T,
    dirOutputs=dirOutputs_i
    )

  # Elec Scarcity
  # By SubReg Type (Basin (GCAM), State (Gird2Poly), County(Gird2Poly)), Withdrawals, Supply, Scarcity
  dfPoly <- metis::metis.index(
    data=dpolyx,
    numerators=c("elecDemTWh"),
    denominators=c("elecByTechTWh"),
    indexName="scarcityElec",
    nameAppend="elec",
    saveFile=T,
    dirOutputs=dirOutputs_i
  )


}

#------------------------------
# Plot Indices
#-------------------------------

# Water Index
if(T){

  numeric2Cat_param <- list("scarcityWat")
  numeric2Cat_breaks <- list(c(-Inf, 0.1, 0.2, 0.4,Inf))
  numeric2Cat_labels <- list(c("None (0<WSI<0.1)","Low (0.1<WSI<0.2)","Moderate (0.2<WSI<0.4)","Severe (WSI>0.4)"))
  numeric2Cat_palette <- list(c("pal_ScarcityCat")) # Can be a custom scale or an R brewer paletter or a metis.pal
  numeric2Cat_legendTextSize <- list(c(0.7))
  numeric2Cat_list_i <-list(numeric2Cat_param=numeric2Cat_param,
                          numeric2Cat_breaks=numeric2Cat_breaks,
                          numeric2Cat_labels=numeric2Cat_labels,
                          numeric2Cat_palette=numeric2Cat_palette,
                          numeric2Cat_legendTextSize=numeric2Cat_legendTextSize)


  dpoly = data.table::fread(paste(dirOutputs_i,"/indexwat/indexwat.csv",sep="")); dpoly
  dpoly <- dpoly %>% dplyr::filter(!is.infinite(value),
                                   !is.na(value))%>%
    dplyr::mutate(scenario = case_when(scenario=="GCAM_SSP2_watch_wfdei"~"SSP2",
                                       scenario=="GCAM_SSP3_watch_wfdei"~"SSP3",
                                       scenario=="GCAM_SSP5_watch_wfdei"~"SSP5",
                                       TRUE~scenario))
  unique(dpoly$scenario);unique(dpoly$subRegType)

  for(subRegType_i in unique(dpoly$subRegType)){

    dpolyx <- dpoly%>%dplyr::filter(subRegType==subRegType_i)

  indexMaps <- metis.mapsProcess(polygonTable=dpolyx,
                                 xRange=c(2010,2020,2030,2040,2050),
                                 dirOutputs = dirOutputs_i,
                                 nameAppend = "IndexCatWat",
                                 numeric2Cat_list = numeric2Cat_list_i)
  }

  for(subRegType_i in unique(dpoly$subRegType)){

    dpolyx <- dpoly%>%dplyr::filter(subRegType==subRegType_i)

    indexMaps <- metis.mapsProcess(polygonTable=dpolyx,
                                   xRange=c(2015,2020,2030,2040,2050),
                                   dirOutputs = dirOutputs_i,
                                   nameAppend = "IndexNumWat",
                                   scaleRangeDiffPrcnt = c(-15,25),
                                   scenRef = "SSP2")
  }

}


# Elec Index
if(T){

  numeric2Cat_param <- list("scarcityElec")
  numeric2Cat_breaks <- list(c(-Inf, 0.5, 1, 2,Inf))
  numeric2Cat_labels <- list(c("None (0<WSI<0.5)","Low (0.5<WSI<1)","Moderate (1<WSI<2)","Severe (WSI>2)"))
  numeric2Cat_palette <- list(c("pal_ScarcityCat")) # Can be a custom scale or an R brewer paletter or a metis.pal
  numeric2Cat_legendTextSize <- list(c(0.7))
  numeric2Cat_list_i <-list(numeric2Cat_param=numeric2Cat_param,
                            numeric2Cat_breaks=numeric2Cat_breaks,
                            numeric2Cat_labels=numeric2Cat_labels,
                            numeric2Cat_palette=numeric2Cat_palette,
                            numeric2Cat_legendTextSize=numeric2Cat_legendTextSize)


  dpoly = data.table::fread(paste(dirOutputs_i,"/indexelec/indexelec.csv",sep="")); dpoly
  unique(dpoly$scenario);unique(dpoly$subRegType)
  dpoly <- dpoly %>% dplyr::filter(scenario %in% c("GCAM_SSP2_GCAM_SSP2","GCAM_SSP3_GCAM_SSP3","GCAM_SSP5_GCAM_SSP5"),
                                   !is.infinite(value),
                                   !is.na(value)) %>%
    dplyr::mutate(scenario = case_when(scenario=="GCAM_SSP2_GCAM_SSP2"~"GCAM_SSP2",
                                       scenario=="GCAM_SSP3_GCAM_SSP3"~"GCAM_SSP3",
                                       scenario=="GCAM_SSP5_GCAM_SSP5"~"GCAM_SSP5",
                                       TRUE~scenario))
  unique(dpoly$scenario);unique(dpoly$subRegType)


  for(subRegType_i in unique(dpoly$subRegType)){

    dpolyx <- dpoly%>%dplyr::filter(subRegType==subRegType_i)

    indexMaps <- metis.mapsProcess(polygonTable=dpolyx,
                                   xRange=c(2010,2020,2030,2040,2050),
                                   dirOutputs = dirOutputs_i,
                                   nameAppend = "IndexCatElecNew",
                                   numeric2Cat_list = numeric2Cat_list_i,
                                   animateOn = T)
  }

  for(subRegType_i in unique(dpoly$subRegType)){

    dpolyx <- dpoly%>%dplyr::filter(subRegType==subRegType_i)

    indexMaps <- metis.mapsProcess(polygonTable=dpolyx,
                                   xRange=c(2015,2020,2030,2040,2050),
                                   dirOutputs = dirOutputs_i,
                                   nameAppend = "IndexNumElec",
                                   scaleRangeDiffPrcnt = c(-15,25),
                                   scenRef = "GCAM_SSP2",
                                   animateOn = T)
  }


}




