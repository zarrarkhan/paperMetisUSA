#............................
# Scripts to produce figures for khan_et_al_2021_interconnectivity_usa
#............................

# Repo: https://github.com/zarrarkhan/khan_et_al_2021_interconnectivity_usa

#................................
# Load Libraries and initial inputs
#............................

# Install packages as needed
library(rgcam); library(metis); library(tidyr); library(dplyr);library(ggplot2); library(ggrepel);
library(ggExtra); library(plotly); library(htmlwidgets); library(circlize)

#................................
# Initial inputs
#............................

if(T){
  # Download Raw GCAM Outputs from Zenodo:
  # Set dirOutputs_i to the unzipped downloaded folder.
  gcamdatabase_i <-paste("C:/Z/projects/current/00_metisGCAMUSA/gcam-core/output/metisUSA2100",sep="")
  #rgcam::localDBConn("C:/Z/projects/metisGCAMUSA/gcam-core/output","metisUSAOld") # Note names of scenarios
  dataProjFile_i <- "metisUSA_dataProj.proj"
  
  dirOutputs_i = paste(getwd(),"/paperFigures",sep="")
  if(!dir.exists(paste(dirOutputs_i,sep=''))){dir.create(paste(dirOutputs_i,sep=''))}

  xRange_i = c(2015,2020,2030,2040,2050,2060,2070,2080,2090,2100)
  
  colorsSubregType_i <- c("Basin"="olivedrab4","State"="goldenrod3","County"="darkgray","National"="indianred4")
  orderSubregType_i <- c("County","State","Basin","National")

  colorsICType_i <- c("ic"="black","ic_sec"="blue","ic_spread"="red")
  orderICType_i <- c("ic","ic_sec","ic_spread")

  colorsScenarios_i <- c("Ref"="red","SSP3"="goldenrod3","SSP5"="olivedrab4","Runoff"="blue","Carbon"="black",
                         "diffAbs_Ref"="red","diffAbs_SSP3"="goldenrod3","diffAbs_SSP5"="olivedrab4","diffAbs_Runoff"="blue","diffAbs_Carbon"="black",
                         "diffPrcnt_Ref"="red","diffPrcnt_SSP3"="goldenrod3","diffPrcnt_SSP5"="olivedrab4","diffPrcnt_Runoff"="blue","diffPrcnt_Carbon"="black")
  orderScenarios_i <- c("Ref","SSP3","SSP5","Runoff","Carbon",
                        "diffAbs_Ref","diffAbs_SSP3","diffAbs_SSP5","diffAbs_Runoff","diffAbs_Carbon",
                        "diffPrcnt_Ref","diffPrcnt_SSP3","diffPrcnt_SSP5","diffPrcnt_Runoff","diffPrcnt_Carbon")

  scenOrigXanthos_i <- c(
                       "SSP2WatConst_watch_wfdei",
                       "SSP5WatConst_watch_wfdei",
                       "SSP2NoWatConst_watch_wfdei",
                       "SSP2WatConstRunoff_GFDL-ESM2M_rcp8p5",
                       "SSP2WatConstCarbon_watch_wfdei")
  scenOrigNames_i <- c(
                "SSP2WatConst",
                "SSP3WatConst",
                "SSP5WatConst",
                #"SSP2",
                #"SSP2WatConstRunoff",
                #"SSP2WatConstCarbon25",
                "SSP2WatConstCarbon")
  scenNewNames_i <- c("Reference","Low Pop/GDP","High Pop/GDP",#"NoWatConst","Carbon25", "Runoff",
                     "Low Carbon")
  scenOrigNewdf_i <- data.frame(scenario=scenOrigNames_i,scenNew=scenNewNames_i)
  regionsSelect_i <- c(metis.assumptions("US52"),"USA")
  paramsSelect_i <- c("pop","gdp",
                      "elecByTechTWh", "elecCapByFuel",
                      "energyFinalConsumBySecEJ",
                      "energyPrimaryByFuelEJ",
                      "watWithdrawByCrop","watWithdrawBySec","watConsumBySec","watSupRunoffBasin","waterWithdrawROGW",
                      "agProdByCrop","agProdBiomass","energyFinalByFuelBySectorEJ",
                      "landAlloc","landAllocByCrop","landAllocDetail",
                      "inputs", "outputs")
  scenariosSelect_i <- c("Ref","Runoff","Carbon","GFDL-ESM2M_rcp8p5","watch_wfdei")
  indexSingles <- c("elecDemTWh","elecByTechTWh")

  subRegionsRemove <- c("AK","PR","HI",
                       "Hawaii","Caribbean","Pacific_and_Arctic_Coast","Baja_California")

}

#..............................
# Base Maps
#...............................

if(T){
  folder_i = "baseMaps"
  # GCAM
  metis.map(dataPolygon=metis::mapGCAMBasinsUS49,fillColumn = "subRegion",labels=F,
            printFig=T,facetsOn=F, dirOutputs = dirOutputs_i, fileName="US49GCAMBasin", folderName=folder_i)
  metis.map(dataPolygon=metis::mapGCAMBasinsUS49,fillColumn = "subRegion",labels=T,
            printFig=T,facetsOn=F, dirOutputs = dirOutputs_i, fileName="US49GCAMBasinLabel", folderName=folder_i)
  # metis.map(dataPolygon=raster::crop(metis::mapGCAMLand,metis::mapUS49),fillColumn = "subRegion",labels=F,
  # printFig=T,facetsOn=F, dirOutputs = dirOutputs_i, fileName="US49GCAMGLU",folderName=folder_i)
  # US
  metis.map(dataPolygon=metis::mapUS49,fillColumn = "subRegion",labels=F,
            printFig=T,facetsOn=F, dirOutputs = dirOutputs_i, fileName="US49State",folderName=folder_i)
  metis.map(dataPolygon=metis::mapUS49,fillColumn = "subRegion",labels=T,
            printFig=T,facetsOn=F, dirOutputs = dirOutputs_i, fileName="US49StateLabel",folderName=folder_i)
  metis.map(dataPolygon=metis::mapUS49County,fillColumn = "subRegion",labels=F,
            printFig=T,facetsOn=F, dirOutputs = dirOutputs_i, fileName="US49County",folderName=folder_i)
  # USGS HUC
  metis.map(dataPolygon=raster::crop(metis::mapUS49HUC2,metis::mapUS49),fillColumn = "subRegion",labels=F,
            printFig=T,facetsOn=F, dirOutputs = dirOutputs_i, fileName="US49HUC2",folderName=folder_i)
  metis.map(dataPolygon=raster::crop(metis::mapUS49HUC4,metis::mapUS49),fillColumn = "subRegion",labels=F,
            printFig=T,facetsOn=F, dirOutputs = dirOutputs_i, fileName="US49HUC4",folderName=folder_i)
  # Plot the grids over states
}

#..............................
# GCAM data (Water-Energy-Food by States-Basin-GLU)
#...............................

if(T){

  dataGCAM<-metis.readgcam(gcamdatabase = gcamdatabase_i,
                           dataProjFile = dataProjFile_i,
                           scenOrigNames = scenOrigNames_i,
                           scenNewNames = scenNewNames_i,
                           regionsSelect = regionsSelect_i ,
                           paramsSelect=paramsSelect_i,
                           dirOutputs = dirOutputs_i)

  # gcamdatabase = gcamdatabase_i
  # dataProjFile = dataProjFile_i
  # scenOrigNames = scenOrigNames_i
  # scenNewNames = scenNewNames_i
  # regionsSelect = regionsSelect_i
  # paramsSelect=paramsSelect_i
  # dirOutputs = dirOutputs_i

  unique(dataGCAM$data$scenario)
  unique(dataGCAM$data$param)

  if(F){ # Check outputs against known data

    df <-  dataGCAM$data

    #.............................................
    # Check Data:
    # Population/GDP

    df %>%
      dplyr::filter(param=="pop",scenario=="Ref",x=="2015",subRegion=="USA") %>%
      dplyr::select(scenario,region,subRegion,param,class1,class2,x,units,value)

    df %>%
      dplyr::filter(param=="pop",scenario=="Ref",x=="2015",subRegion %in% metis.assumptions("US52")) %>%
      dplyr::group_by(scenario,param,class1,class2,x,units) %>%
      dplyr::summarize(value=sum(value,na.rm=T))

    df %>%
      dplyr::filter(param=="gdp",scenario=="Ref",x=="2015",subRegion=="USA") %>%
      dplyr::select(scenario,region,subRegion,param,class1,class2,x,units,value)

    df %>%
      dplyr::filter(param=="gdp",scenario=="Ref",x=="2015",subRegion %in% metis.assumptions("US52")) %>%
      dplyr::group_by(scenario,param,class1,class2,x,units) %>%
      dplyr::summarize(value=sum(value,na.rm=T))

    #.............................................
    # Check Data:
    # Water supply Runoff
    # http://www.fao.org/nr/water/aquastat/data/query/results.html
    # US - GCAM 3629 km3
    # FAO Total renewable surface water = 2900 km3)
    (df%>%dplyr::filter(param=="watSupRunoffBasin"))$region%>%unique()
    runoffGCAM2015 <- df%>%filter(param=="watSupRunoffBasin", scenario=="SSP2",x==2015,region=="USA")%>% dplyr::summarize(valSum=sum(value,na.rm=T))
    runoffGCAM2015

    #................................................
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

    #..................................................
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

    #............................................
    # Electricity Consumption (US/CA/TX)


    #...........................................
    # Agriculture Production (US/CA/TX)
    # Agriculture Demand (US/CA/TX)
    # Food Production (US/CA/TX)
    # Food demand (US/CA/TX)


    runoffGCAM2015
    wwithdrawGCAM2015

  } # Check outputs against known data

}

#..............................
# GCAM Data Charts
#...............................

if(T){
  # National by Class
  rTable_i_class = data.table::fread(paste(dirOutputs_i,"/readGCAM/Tables_gcam/gcamDataTable_aggClass1.csv",sep="")) %>%
    tibble::as_tibble() %>%
    dplyr::filter(scenario %in% scenNewNames_i)%>%
    dplyr::mutate(classPalette="pal_metis",
                  class=case_when((param=="watConsumBySec" & class=="primary")~"mining",
                                  (param=="watConsumBySec" & class=="irrigation")~"agriculture",
                                  (param=="watConsumBySec" & class=="electric")~"electricity",
                                  (param=="watConsumBySec" & class=="domestic")~"municipal",
                                  (param=="watConsumBySec" & class=="animal")~"livestock",
                                  (param=="landAllocDetail" & grepl("pasture",class,ignore.case = T))~"Pasture",
                                  (param=="landAllocDetail" & grepl("biomass",class,ignore.case = T))~"Biomass",
                                  (param=="landAllocDetail" & grepl("shrub",class,ignore.case = T))~"Shrub",
                                  (param=="landAllocDetail" & grepl("grassland",class,ignore.case = T))~"Grass",
                                  (param=="landAllocDetail" & grepl("fodder",class,ignore.case = T))~"Fodder",
                                  (param=="landAllocDetail" & grepl("forest",class,ignore.case = T))~"Forest",
                                  (param=="landAllocDetail" & grepl("Urban",class,ignore.case = T))~"Urban",
                                  (param=="landAllocDetail" & grepl("Tundra|Shrubland|ROckIceDesert|Other",class,ignore.case = T))~"Other",
                                  TRUE~class))%>%
    dplyr::mutate(scenario = case_when(scenario==scenOrigNames_i[1]~scenNewNames_i[1],
                                       scenario==scenOrigNames_i[2]~scenNewNames_i[2],
                                       scenario==scenOrigNames_i[3]~scenNewNames_i[3],
                                       scenario==scenOrigNames_i[4]~scenNewNames_i[4],
                                       scenario==scenOrigNames_i[5]~scenNewNames_i[5],
                                       TRUE~scenario)); rTable_i_class;
  unique(rTable_i_class$param); unique(rTable_i_class$scenario)
  unique((rTable_i_class%>%filter(param=="agProdByCrop"))$class)

  # Remove non ag crops from land allocation detail
  rTable_i <- rTable_i_class %>%
    dplyr::filter(!(param=="landAllocDetail" & class %in% c("Grass","Forest","Urban","Pasture","Shrub","Other")));

  unique((rTable_i%>%filter(param=="landAllocDetail"))$class)

 params2Agg <- (rTable_i%>%filter(subRegion=="USA"))$param%>%unique();
 params2Agg <- unique(rTable_i$param)[!unique(rTable_i$param) %in% params2Agg];params2Agg
  rTable_iAggsums<-rTable_i%>%
    dplyr::filter(param %in% params2Agg)%>%
    dplyr::mutate(scenario=as.character(scenario))%>%
    dplyr::filter(aggregate=="sum")%>%
    dplyr::select(-subRegion,-region)%>%
    dplyr::group_by_at(dplyr::vars(-value))%>%
    dplyr::summarize_at(c("value"),list(~sum(.)))
  rTable_iAggmeans<-rTable_i%>%
    dplyr::filter(param %in% params2Agg)%>%
    dplyr::mutate(scenario=as.character(scenario))%>%
    dplyr::filter(aggregate=="mean")%>%
    dplyr::select(-subRegion,-region)%>%
    dplyr::group_by_at(dplyr::vars(-value))%>%
    dplyr::summarize_at(c("value"),list(~mean(.)))
  rTable_iUSA <-dplyr::bind_rows(rTable_iAggsums,rTable_iAggmeans)%>%dplyr::ungroup()%>%
    dplyr::mutate(region="USA", subRegion="USA")

  rTable_iAgg <- rTable_i %>%
    dplyr::filter(!param %in% params2Agg)%>%
    dplyr::bind_rows(rTable_iUSA); rTable_iAgg

  # Convert Population and GDP from USD 1990 to USD 2015 (1.64)
  # https://stats.areppim.com/calc/calc_usdlrxdeflator.php
  # BEA https://apps.bea.gov/iTable/iTable.cfm?reqid=19&step=3&isuri=1&1921=survey&1903=13#reqid=19&step=3&isuri=1&1921=survey&1903=13

  rTable_iAgg <- rTable_iAgg %>%
    dplyr::mutate(value = case_when(param=="gdp"~value*1.64/1000,
                                    TRUE~value),
                  units = case_when(param=="gdp"~"Trillion 2015 USD",
                                    TRUE~units));


  mp_i_Mod<-list(paramSet=list(
    #c("paramsAll"),
    c("paramsWaterAg"),
    c("socioEcon")),
    param=list(
      #c("watWithdrawBySec","watConsumBySec","landAllocDetail",
      #  "elecByTechTWh","energyFinalByFuelBySectorEJ"),
      c("watWithdrawBySec","landAllocDetail"),
      c("pop","gdp")),
    nColMax=list(
      #c(3),
      c(2),
      c(2)))

  charts<-metis.chartsProcess(rTable=rTable_iAgg %>% dplyr::filter(subRegion=="USA"), # Default is NULL
                              #dataTables=dataTables_i, # Default is NULL.
                              paramsSelect=mp_i_Mod$param%>%unlist(), # Default is "All"
                              #regionsSelect=regionsSelect_i, # Default is "All"
                              xCompare=c(2015,2025,2050,2075,2100), # Default is c("2015","2030","2050","2100")
                              scenRef="Ref", # Default is NULL
                              colOrder1=scenNewNames_i,
                              colOrderName1="scenario",
                              xRange = xRange_i,
                              dirOutputs=dirOutputs_i,
                              regionCompareOnly=0,
                              scenarioCompareOnly=0,
                              folderName="chartsNational",
                              multiPlotFigsOnly = T,
                              mp = mp_i_Mod,
                              multiPlotOn = T,
                              multiPlotFigLabels=F,
                              figWidth = 13,
                              figHeight = 9)

if(F){
  charts<-metis.chartsProcess(rTable=rTable_i %>% filter(region!="USA",
                                                         region %in% c("TX","CA","MO"),
                                                         scenario=="Ref"), # Default is NULL
                              #dataTables=dataTables_i, # Default is NULL.
                              paramsSelect=paramsSelect_i, # Default is "All"
                              #regionsSelect=regionsSelect_i, # Default is "All"
                              xCompare=c(2015,2025,2050,2075,2100), # Default is c("2015","2030","2050","2100")
                              #scenRef="Ref", # Default is NULL,
                              colOrder1=scenNewNames_i,
                              colOrderName1="scenario",
                              xRange = xRange_i,
                              dirOutputs=dirOutputs_i,
                              regionCompareOnly=1,
                              scenarioCompareOnly=0,
                              folderName="chartsSubNat",
                              multiPlotFigsOnly = F,
                              mp = mp_i_Mod,
                              multiPlotOn = T,
                              multiPlotFigLabels=F,
                              figWidth = 9,
                              figHeight = 7)
}


}

#..............................
# GCAM Data Maps
#...............................

if(T){
  # Read Output data from metis.readGCAM aggregated by param
  dfparam <- data.table::fread(paste(dirOutputs_i,"/readGCAM/Tables_gcam/gcamDataTable_aggParam.csv",sep=""))
  dfclass <- data.table::fread(paste(dirOutputs_i,"/readGCAM/Tables_gcam/gcamDataTable_aggClass1.csv",sep=""))

  # Convert Population and GDP from USD 1990 to USD 2015 (1.64)
  # https://stats.areppim.com/calc/calc_usdlrxdeflator.php
  # BEA https://apps.bea.gov/iTable/iTable.cfm?reqid=19&step=3&isuri=1&1921=survey&1903=13#reqid=19&step=3&isuri=1&1921=survey&1903=13

  dfparam <- dfparam %>% dplyr::mutate(value = case_when(param=="gdp"~value*1.64/1000,TRUE~value),
                                       units = case_when(param=="gdp"~"Trillion 2015 USD",TRUE~units));
  dfclass <- dfclass %>% dplyr::mutate(value = case_when(param=="gdp"~value*1.64/1000,TRUE~value),
                                       units = case_when(param=="gdp"~"Trillion 2015 USD",TRUE~units));


  # Data by US States - Total
  unique((dfparam%>%filter(region!="USA"))$param)
  dfp <- dfparam %>% dplyr::filter(region!="USA",
                                   x %in% c(min(xRange_i),max(xRange_i)),
                                   !subRegion %in% subRegionsRemove,
                                   param %in% c("pop","gdp","watWithdrawBySec"))



  metis.mapsProcess(polygonTable=dfp,
                    xRange=xRange_i,
                    dirOutputs = dirOutputs_i,
                    nameAppend = "paramsGCAM",
                    scenRef = "Ref",
                    xRef=2015,
                    animateOn = F)

  # Data by US States - Water for Elec
  unique((dfclass%>%filter(region!="USA"))$param)
  dfc=dfclass %>%
    dplyr::filter(region!="USA",
                  param=="watWithdrawBySec",
                  #class=="electricity",
                  !subRegion %in% subRegionsRemove,
                  x %in% c(min(xRange_i),max(xRange_i))) %>%
    dplyr::mutate(param="watForElec"); dfc
  metis.mapsProcess(polygonTable=dfc,
                    xRange=xRange_i,
                    dirOutputs = dirOutputs_i,
                    nameAppend = "classGCAM",
                    scenRef = "Ref",
                    xRef=2015,
                    xDiff=c(2100))
}

#..............................
# Interconnectivity
#...............................

# Create Folder
if(T){
dirComb <- "interCon"
if(!dir.exists(paste(dirOutputs_i,"/",dirComb,"/",sep=''))){
  dir.create(paste(dirOutputs_i,"/",dirComb,"/",sep=''))}
dirIC <- paste(dirOutputs_i,"/",dirComb,"/",sep='')
}

# Calculate Interconectivity by Basin and State
if(T){

  # Interconnected sectors: water, electricity, ag, primary

  # Water IC (dficFlow_watkm3)
  #...................
  if(T){

  # State
  dficFlow_watState = data.table::fread(paste(dirOutputs_i,"/readGCAM/Tables_gcam/gcamDataTable_aggClass1.csv",sep="")) %>%
    tibble::as_tibble() %>%
    dplyr::filter(scenario %in% scenNewNames_i,
                  param %in% "watWithdrawBySec",
                  subRegion %in% metis.assumptions("US49"))%>%
    dplyr::select(scenario,subRegion,param,class,x,units,value)%>%
    dplyr::mutate(class=case_when((class=="mining")~"primary",
                                  (class=="agriculture")~"ag",
                                  (class=="municipal")~"other",
                                  (class=="livestock")~"other",
                                  (class=="industry")~"other",
                                  (class=="desalination")~"other",
                                  TRUE~class))%>%
    dplyr::group_by(scenario,subRegion,param,class,x,units)%>%
    dplyr::summarize(value=sum(value,na.rm=T))%>%
    dplyr::ungroup()%>%
    tidyr::spread(key="class",value="value")%>%
    dplyr::mutate(subRegType="State")%>%
    dplyr::filter(x %in% xRange_i)%>%
    dplyr::ungroup() %>%
     dplyr::select(subRegion, subRegType, param, x, scenario, units, other, electricity, ag, primary) %>%
     tidyr::gather(key="to",value="value",-subRegion,-subRegType,-x,-scenario,-param, -units)%>%
     dplyr::select(-param)%>%
     dplyr::mutate(from="water")%>%
    dplyr::distinct(); dficFlow_watState

  # Basin
  basin_state <- metis::mapIntersectGCAMBasinUS52@data %>%
    dplyr::select(subRegion_GCAMBasin, subRegion=subRegion_US52,ratioAreaIntersect);basin_state

  dficFlow_watBasin <- dficFlow_watState %>%
    dplyr::left_join(basin_state)%>%
    dplyr::mutate(value=value*ratioAreaIntersect,
                  subRegion=subRegion_GCAMBasin,
                  subRegType="Basin")%>%
    dplyr::select(-subRegion_GCAMBasin,-ratioAreaIntersect)%>%
    dplyr::group_by(subRegion,subRegType,x,scenario,units,to,from)%>%
    dplyr::summarize(value=sum(value,na.rm=T));dficFlow_watBasin

  # National
  dficFlow_watNat <- dficFlow_watState %>%
    dplyr::mutate(subRegion="USA",
                  subRegType="National") %>%
    dplyr::group_by(subRegion,subRegType,x,scenario,units,to,from)%>%
    dplyr::summarize(value=sum(value,na.rm=T)); dficFlow_watNat

  dficFlow_watState
  dficFlow_watBasin
  dficFlow_watNat
  # Check
  sum(dficFlow_watState$value,na.rm=T);sum(dficFlow_watBasin$value);sum(dficFlow_watNat$value);

  # Combine
  dficFlow_watkm3 <-  dficFlow_watState %>%
    dplyr::bind_rows(dficFlow_watBasin) %>%
    dplyr::bind_rows(dficFlow_watNat); dficFlow_watkm3

  }

  # Electric IC (dficFlow_elecTWh)
  #...................
  if(T){

    # Elec for Water
    if(T){

        # elecForWat coefficients
        if(T){
          # Elec for Wat Coefficients
          # From Page paper EFW_SI - Table S2 received vie slack

          efw_kwh_per_m3 <- data.frame(
            sector=c("desal","desal","irrigation","irrigation","industry","industry","industry","industry",
                     "municipal","municipal","municipal","municipal","municipal"),
            process=c("reverse_osmosis","thermal_distillation","sw","gw","sw","gw","treatment","waste_treatment",
                      "sw","gw","treatment","distribution","waste_treatment"),
            fuel=c("electricity","gas_oil","electricity","electricity","electricity","electricity","electricity",
                   "electricity","electricity","electricity","electricity","electricity","electricity"),
            low=c(1.878,25.5,0.050,0.1,0.05,0.1,0.079,0.386,0.05,0.1,0.143,0.170,0.358),
            baseline=c(2.75,58.3,0.079,0.185,0.079,0.185,0.178,0.775,0.079,0.185,0.235,0.247,0.597),
            high=c(4.278,77.8,0.1,0.3,0.1,0.3,0.319,1.144,0.1,0.3,0.4,0.339,1.008)
          ); efw_kwh_per_m3

          # Note 1 TWh/km3 = 1 KWh/m3 so no change
          efw_twh_per_km3 <- efw_kwh_per_m3 %>%
            dplyr::filter(fuel=="electricity")%>%
            dplyr::group_by(process)%>%
            dplyr::summarize_at(c("low","baseline","high"),~mean(.,na.rm=T)); efw_twh_per_km3

          elecForWatSW_TWhperkm3 <- (efw_twh_per_km3 %>% dplyr::filter(process=="gw"))$baseline; elecForWatSW_TWhperkm3
          elecForWatGW_TWhperkm3 <- (efw_twh_per_km3 %>% dplyr::filter(process=="sw"))$baseline; elecForWatGW_TWhperkm3
          elecForWatDesal_TWhperkm3 <- (efw_twh_per_km3 %>% dplyr::filter(process=="reverse_osmosis"))$baseline; elecForWatDesal_TWhperkm3
        }

        # SW/GW Ratios by State, Basin
        if(T){
        # GCAM Water by SW/GW by Basin
        swgw_Basin = data.table::fread(paste(dirOutputs_i,"/readGCAM/Tables_gcam/gcamDataTable_aggClass1.csv",sep="")) %>%
          tibble::as_tibble() %>%
          dplyr::filter(scenario %in% scenNewNames_i,
                        param %in% c("waterWithdrawROGW"),
                        class %in% c("groundwater","runoff")) %>%
          dplyr::select(scenario,subRegion,x,class, value)%>%
          tidyr::spread(key="class",value="value") %>%
          dplyr::mutate(subRegType="Basin",
                        total=groundwater+runoff,
                        ratioGW=case_when(total==0~0,TRUE~round((groundwater/total),4)),
                        ratioSW=case_when(total==0~0,TRUE~round((runoff/total),4)),
                        sumRatios=ratioGW+ratioSW,
                        gw_km3=groundwater,
                        sw_km3=runoff)%>%
          dplyr::filter(x %in% xRange_i) %>%
          dplyr::select(scenario,subRegion,x,ratioGW,ratioSW, gw_km3,sw_km3)%>%
          dplyr::mutate(subRegType="Basin")%>%
          dplyr::filter(!subRegion %in% subRegionsRemove); swgw_Basin  %>% as.data.frame()

        # SW/GW Ratios by State
        basin_state <- metis::mapIntersectGCAMBasinUS52@data %>%
          dplyr::select(subRegion=subRegion_GCAMBasin, subRegion_US52,area_sqkm);basin_state

        swgw_State <- swgw_Basin %>%
          dplyr::left_join(basin_state) %>%
          dplyr::arrange(subRegion)%>%
          dplyr::group_by(scenario,subRegion,x)%>%
          dplyr::mutate(ratioAreaIntersect=area_sqkm/sum(area_sqkm),
                        gw_km3=gw_km3*ratioAreaIntersect,
                        sw_km3=sw_km3*ratioAreaIntersect)%>%
          dplyr::ungroup()%>%
          dplyr::mutate(subRegion=subRegion_US52)%>%
          dplyr::select(-subRegion_US52)%>%
          dplyr::group_by(scenario,subRegion,x)%>%
          dplyr::summarize_at(c("gw_km3","sw_km3"),~sum(.,na.rm=T))%>%
          dplyr::mutate(ratioGW=gw_km3/(gw_km3+sw_km3),
                        ratioSW=sw_km3/(gw_km3+sw_km3),
                        subRegType="State"); swgw_State

        # National
        swgw_Nat <- swgw_Basin %>%
          dplyr::mutate(subRegion="USA",
                        subRegType="National") %>%
          dplyr::group_by(subRegion,subRegType,x,scenario)%>%
          dplyr::summarize_at(c("gw_km3","sw_km3"),~sum(.,na.rm=T))%>%
          dplyr::mutate(ratioGW=gw_km3/(gw_km3+sw_km3),
                        ratioSW=sw_km3/(gw_km3+sw_km3)); swgw_Nat

        # Combined
        swgw <- swgw_Basin %>%
          dplyr::bind_rows(swgw_State)%>%
          dplyr::bind_rows(swgw_Nat)%>%
          dplyr::filter(!subRegion %in% subRegionsRemove); swgw

        # Check
        sum(swgw_Nat$sw_km3,na.rm=T);sum(swgw_Basin$sw_km3,na.rm=T);sum(swgw_State$sw_km3,na.rm=T);

        }

        # Desal (TO DO)
        if(T){
        # # GCAM Water by desal by State
        # df_desalState = data.table::fread(paste(dirOutputs_i,"/readGCAM/Tables_gcam/gcamDataTable_aggClass1.csv",sep="")) %>%
        #   tibble::as_tibble() %>%
        #   dplyr::filter(scenario %in% scenNewNames_i,
        #                 param %in% c("watWithdrawBySec"),
        #                 class %in% c("desalination")); df_desalState  %>% as.data.frame()


       # # Check water by sec and water by ROGW
       #  dfCheck <- data.table::fread(paste(dirOutputs_i,"/readGCAM/Tables_gcam/gcamDataTable_aggClass1.csv",sep="")) %>%
       #    tibble::as_tibble() %>%
       #    dplyr::filter(scenario %in% scenNewNames_i,
       #                  param %in% c("waterWithdrawROGW","watWithdrawBySec"))%>%
       #    dplyr::select(scenario,x,param,value)%>%
       #    dplyr::group_by(scenario,x,param)%>%
       #    dplyr::summarize(value=sum(value,na.rm=T))%>%
       #    dplyr::ungroup(); dfCheck %>% filter(x==2050)
          }

        # Calculate Elec for Water
        if(T){
        elecForWater_TWh <- swgw %>%
          dplyr::mutate(elecForWatTWh_GW=(gw_km3)*elecForWatGW_TWhperkm3,
                        elecForWatTWh_SW=(sw_km3)*elecForWatSW_TWhperkm3,
                        water=elecForWatTWh_GW+elecForWatTWh_SW) %>%
          dplyr::select(-elecForWatTWh_GW, -elecForWatTWh_SW, -ratioGW, -ratioSW, -gw_km3,-sw_km3)%>%
          dplyr::filter(!subRegion %in% subRegionsRemove); elecForWater_TWh

        # Check
        elecForWater_TWh%>%dplyr::ungroup()%>%dplyr::group_by(subRegType)%>%summarize(water=sum(water,na.rm=T))

         }

    }

    # Elec for Ag
    if(T){

        # Elec for ag coefficient
        if(T){
        # https://www.ers.usda.gov/webdocs/publications/74658/60128_eib159.pdf?v=3118.7
        # Total electricity for agriculture in US in trillion BTU in 2015 = 300 trillion BTU
        # Total land area km2 for crops and livestock
        # elecForAgCoefficient in Trillion BTU /km2 - Convert to TWh/km2
        # https://www.convert-measurement-units.com/conversion-calculator.php?type=energy

        elecForAg_TWh_2015 = 300 * 0.29307107017222; elecForAg_TWh_2015

        ag_land_km2_2015 <- data.table::fread(paste(dirOutputs_i,"/readGCAM/Tables_gcam/gcamDataTable_aggClass1.csv",sep="")) %>%
            tibble::as_tibble() %>%
            dplyr::filter(scenario %in% "Ref",
                          param %in% c("landAlloc"),
                          x==2015)%>%
          dplyr::mutate(value=value*1000,
                        units="km2")%>%
          dplyr::select(scenario,region,subRegion,param,class,x,units,value) %>%
          dplyr::filter(class %in% c("crops","pasture","RootTuber"))%>%
          dplyr::group_by(scenario,region,subRegion,param,x,units)%>%
          dplyr::summarize(value=sum(value,na.rm=T)); ag_land_km2_2015

        elecforAgCoefficient_TWh_per_km2 = elecForAg_TWh_2015/ag_land_km2_2015$value;elecforAgCoefficient_TWh_per_km2
        }

        # Elec for Ag land area
        if(T){

          # Basin
          elecForAgLand_Basin <- data.table::fread(paste(dirOutputs_i,"/readGCAM/Tables_gcam/gcamDataTable_aggClass1.csv",sep="")) %>%
            tibble::as_tibble()%>%
            dplyr::filter(scenario %in% scenNewNames_i,
                          param %in% c("landAllocDetail"),
                          class %in% c("biomassGrass","biomassTree",
                                       "Corn","FiberCrop","FodderGrass","FodderHerb",
                                       "MiscCrop","OilCrop","OtherGrain","PalmFruit","Rice",
                                       "RootTuber","SugarCrop","Wheat"))%>%
            dplyr::select(scenario,region,subRegion,param,class,value,x,units)%>%
            dplyr::mutate(subRegType="Basin",
                          ag=value*1000*elecforAgCoefficient_TWh_per_km2,
                          units="TWh")%>%
            dplyr::select(-value) %>%
            dplyr::group_by(scenario,subRegion,subRegType,x)%>%
            dplyr::summarize_at(c("ag"),~sum(.,na.rm=T))%>%
            dplyr::left_join(metis.mappings("subRegionMap"))%>%
            dplyr::mutate(subRegion=subRegionMetis)%>%
            dplyr::select(-subRegionMetis)%>%
            dplyr::filter(!subRegion %in% subRegionsRemove); elecForAgLand_Basin

          # State
          basin_state <- metis::mapIntersectGCAMBasinUS52@data %>%
            dplyr::select(subRegion=subRegion_GCAMBasin, subRegion_US52,area_sqkm);basin_state

          elecForAgLand_State <- elecForAgLand_Basin %>%
            dplyr::left_join(basin_state) %>%
            dplyr::arrange(subRegion)%>%
            dplyr::group_by(scenario,subRegion,x)%>%
            dplyr::mutate(ratioAreaIntersect=area_sqkm/sum(area_sqkm),
                          ag=ag*ratioAreaIntersect)%>%
            dplyr::ungroup()%>%
            dplyr::mutate(subRegion=subRegion_US52)%>%
            dplyr::select(-subRegion_US52)%>%
            dplyr::group_by(scenario,subRegion,x)%>%
            dplyr::summarize_at(c("ag"),~sum(.,na.rm=T))%>%
            dplyr::mutate(subRegType="State"); elecForAgLand_State

          # National
          elecForAgLand_Nat <- elecForAgLand_State %>%
            dplyr::mutate(subRegion="USA",
                          subRegType="National") %>%
            dplyr::group_by(subRegion,subRegType,x,scenario)%>%
            dplyr::summarize_at(c("ag"),~sum(.,na.rm=T)); elecForAgLand_Nat

          # Combined
          elecForAgLand_TWh <-  elecForAgLand_Nat %>%
            dplyr::bind_rows( elecForAgLand_Basin) %>%
            dplyr::bind_rows( elecForAgLand_State)%>%
            dplyr::filter(!subRegion %in% subRegionsRemove); elecForAgLand_TWh

          # Check
          sum(elecForAgLand_Nat$ag,na.rm=T);sum(elecForAgLand_Basin$ag,na.rm=T);sum(elecForAgLand_State$ag,na.rm=T);


        }

    }

    # Elec for Primary
    if(T){

    # inputs by tech - elec_td_ind going to oil_refineries
    elecForPrimary_TWh <-elecForAgLand_TWh %>%
      dplyr::rename(primary=ag) %>%
      dplyr::mutate(primary=0)%>%
      dplyr::filter(!subRegion %in% subRegionsRemove); elecForPrimary_TWh
    }

    # Elec Total
    if(T){

    # State
      elecTWhTot_State = data.table::fread(paste(dirOutputs_i,"/readGCAM/Tables_gcam/gcamDataTable_aggParam.csv",sep="")) %>%
        tibble::as_tibble() %>%
        dplyr::filter(scenario %in% scenNewNames_i,
                      param %in% c("elecByTechTWh"),
                      subRegion %in% metis.assumptions("US49"))%>%
        dplyr::select(scenario, subRegion, x,value)%>%
        dplyr::group_by(scenario,subRegion, x)%>%
        dplyr::summarize(value=sum(value,na.rm=T))%>%
        dplyr::ungroup() %>%
        dplyr::mutate(subRegType="State",
                      total=value)%>%
        dplyr::select(-value); elecTWhTot_State %>% as.data.frame()

      # Basin
      basin_state <- metis::mapIntersectGCAMBasinUS52@data %>%
        dplyr::select(subRegion_GCAMBasin, subRegion=subRegion_US52,ratioAreaIntersect);basin_state

      elecTWhTot_Basin <- elecTWhTot_State %>%
        dplyr::left_join(basin_state)%>%
        dplyr::mutate(total=total*ratioAreaIntersect,
                      subRegion=subRegion_GCAMBasin,
                      subRegType="Basin")%>%
        dplyr::select(-subRegion_GCAMBasin,-ratioAreaIntersect)%>%
        dplyr::group_by(subRegion,subRegType,x,scenario)%>%
        dplyr::summarize(total=sum(total,na.rm=T))%>%
        dplyr::filter(!subRegion %in% subRegionsRemove);elecTWhTot_Basin

      #sum(elecTWhTot_State$total,na.rm=T);sum(elecTWhTot_Basin$total,na.rm=T)

      # National
      elecTWhTot_Nat <- elecTWhTot_State %>%
        dplyr::mutate(subRegion="USA",
                      subRegType="National") %>%
        dplyr::group_by(subRegion,subRegType,x,scenario)%>%
        dplyr::summarize(total=sum(total,na.rm=T));  elecTWhTot_Nat

      # Combined
      elecTWhTot <-  elecTWhTot_Nat %>%
        dplyr::bind_rows( elecTWhTot_Basin) %>%
        dplyr::bind_rows( elecTWhTot_State)%>%
        dplyr::filter(!subRegion %in% subRegionsRemove); elecTWhTot

      # Check
      sum(elecTWhTot_State$total,na.rm=T);sum( elecTWhTot_Basin$total);sum( elecTWhTot_Nat$total);

    }

    # Elec Combine
    if(T){
    dficFlow_elecTWh <- elecTWhTot %>%
      dplyr::left_join(elecForPrimary_TWh) %>%
      dplyr::left_join(elecForAgLand_TWh) %>%
      dplyr::left_join(elecForWater_TWh) %>%
      dplyr::mutate(other = total - (ag+water+primary))%>%
      dplyr::filter(x %in% xRange_i)%>%
      dplyr::ungroup() %>%
      dplyr::select(subRegion, subRegType, x, scenario, other, water, ag, primary) %>%
      tidyr::gather(key="to",value="value",-subRegion,-subRegType,-x,-scenario)%>%
      dplyr::mutate(from="electricity")%>%
      dplyr::distinct();dficFlow_elecTWh

    # Check
    (elecTWhTot%>%dplyr::filter(subRegType=="Basin"))$subRegion%>%unique()%>%sort()
    (elecForPrimary_TWh%>%dplyr::filter(subRegType=="Basin"))$subRegion%>%unique()%>%sort()
    (elecForAgLand_TWh%>%dplyr::filter(subRegType=="Basin"))$subRegion%>%unique()%>%sort()
    (elecForWater_TWh%>%dplyr::filter(subRegType=="Basin"))$subRegion%>%unique()%>%sort()
    (dficFlow_elecTWh%>%dplyr::filter(subRegType=="Basin"))$subRegion%>%unique()%>%sort()

    (elecTWhTot%>%dplyr::filter(subRegType=="State"))$subRegion%>%unique()%>%sort()
    (elecForPrimary_TWh%>%dplyr::filter(subRegType=="State"))$subRegion%>%unique()%>%sort()
    (elecForAgLand_TWh%>%dplyr::filter(subRegType=="State"))$subRegion%>%unique()%>%sort()
    (elecForWater_TWh%>%dplyr::filter(subRegType=="State"))$subRegion%>%unique()%>%sort()
    (dficFlow_elecTWh%>%dplyr::filter(subRegType=="State"))$subRegion%>%unique()%>%sort()

    elecTWhTot%>%dplyr::ungroup()%>%dplyr::group_by(subRegType)%>%summarize(total=sum(total,na.rm=T))
    elecForPrimary_TWh%>%dplyr::ungroup()%>%dplyr::group_by(subRegType)%>%summarize(primary=sum(primary,na.rm=T))
    elecForAgLand_TWh%>%dplyr::ungroup()%>%dplyr::group_by(subRegType)%>%summarize(ag=sum(ag,na.rm=T))
    elecForWater_TWh%>%dplyr::ungroup()%>%dplyr::group_by(subRegType)%>%summarize(water=sum(water,na.rm=T))
    dficFlow_elecTWh%>%dplyr::ungroup()%>%dplyr::group_by(subRegType)%>%summarize(value=sum(value,na.rm=T))

      }
  }

  # Ag IC (dficFlow_ag)
  #...................
  if(T){

    # Ratio Biomass to Elec vs Primary (biomassElecPrimRatio)
    if(T){

      biomassElecTWh_State <-data.table::fread(paste(dirOutputs_i,"/readGCAM/Tables_gcam/gcamDataTable_aggClass1.csv",sep="")) %>%
        tibble::as_tibble()%>%
        dplyr::filter(scenario %in% scenNewNames_i,
                      param %in% c("elecByTechTWh"),
                      class =="biomass")%>%
        dplyr::mutate(subRegType="State")%>%
        dplyr::select(scenario,subRegion,subRegType,value,x)%>%
        dplyr::group_by(scenario,subRegion,subRegType,x)%>%
        dplyr::summarize_at(c("value"),~sum(.,na.rm=T))%>%
        dplyr::rename(elec=value);  biomassElecTWh_State

      biomassPrimaryTWh_State <-data.table::fread(paste(dirOutputs_i,"/readGCAM/Tables_gcam/gcamDataTable.csv",sep="")) %>%
        tibble::as_tibble()%>%
        dplyr::filter(scenario %in% scenNewNames_i,
                      param %in% c("inputs"),
                      class2 %in% c("biomass liquids"),
                      class1 %in% c("regional biomass",
                                    "regional biomassOil",
                                    "regional corn for ethanol"))%>%
        dplyr::mutate(value=value*metis::metis.assumptions()$convEJ2TWh,
                      subRegType="State")%>%
        dplyr::select(scenario,subRegion,subRegType,value,x)%>%
        dplyr::group_by(scenario,subRegion,subRegType,x)%>%
        dplyr::summarize_at(c("value"),~sum(.,na.rm=T))%>%
        dplyr::rename(prim=value); biomassPrimaryTWh_State

      # Ratio State
      biomassElecPrimRatio_State <-biomassElecTWh_State %>%
        dplyr::left_join(biomassPrimaryTWh_State)%>%
        dplyr::mutate(elec=ifelse(is.na(elec),0,elec),
                      prim=ifelse(is.na(prim),0,prim),
                      ratioElec=elec/(elec+prim),
                      ratioPrim=prim/(elec+prim));biomassElecPrimRatio_State

      #biomassElecPrimRatio_State %>% dplyr::filter(subRegion=="CA",x==2015,scenario=="SSP5")

      # Basin
      basin_state <- metis::mapIntersectGCAMBasinUS52@data %>%
        dplyr::select(subRegion_GCAMBasin, subRegion=subRegion_US52,ratioAreaIntersect);basin_state

      biomassElecPrimRatio_Basin <- biomassElecPrimRatio_State %>%
        dplyr::left_join(basin_state)%>%
        dplyr::mutate(elec=elec*ratioAreaIntersect,
                      prim=prim*ratioAreaIntersect,
                      subRegion=subRegion_GCAMBasin,
                      subRegType="Basin")%>%
        dplyr::select(-subRegion_GCAMBasin,-ratioAreaIntersect,-ratioElec,-ratioPrim)%>%
        dplyr::group_by(subRegion,subRegType,x,scenario)%>%
        dplyr::summarize_at(c("elec","prim"),~(sum(.,na.rm=T)))%>%
        dplyr::mutate(elec=ifelse(is.na(elec),0,elec),
                      prim=ifelse(is.na(prim),0,prim),
                      ratioElec=elec/(elec+prim),
                      ratioPrim=prim/(elec+prim)); biomassElecPrimRatio_Basin

      # National
      biomassElecPrimRatio_National <- biomassElecPrimRatio_State %>%
        dplyr::mutate(subRegion="USA",
                      subRegType="National") %>%
        dplyr::group_by(subRegion,subRegType,x,scenario)%>%
        dplyr::summarize_at(c("elec","prim"),~(sum(.,na.rm=T)))%>%
        dplyr::mutate(elec=ifelse(is.na(elec),0,elec),
                      prim=ifelse(is.na(prim),0,prim),
                      ratioElec=elec/(elec+prim),
                      ratioPrim=prim/(elec+prim)); biomassElecPrimRatio_National

      # Check
      sum(biomassElecPrimRatio_National$elec,na.rm=T);
      sum(biomassElecPrimRatio_State$elec,na.rm=T);
      sum(biomassElecPrimRatio_Basin$elec,na.rm=T);

      # Combine
      biomassElecPrimRatio <-  biomassElecPrimRatio_State %>%
        dplyr::bind_rows(biomassElecPrimRatio_Basin) %>%
        dplyr::bind_rows(biomassElecPrimRatio_National); biomassElecPrimRatio

    }

    # Ag biomass land (agLandBiomasskm2)
    if(T){
      # Basin
      agLandBiomasskm2_Basin <- data.table::fread(paste(dirOutputs_i,"/readGCAM/Tables_gcam/gcamDataTable_aggClass1.csv",sep="")) %>%
        tibble::as_tibble()%>%
        dplyr::filter(scenario %in% scenNewNames_i,
                      param %in% c("landAllocDetail"),
                      class %in% c("biomassGrass","biomassTree"))%>%
        dplyr::select(scenario,region,subRegion,param,class,value,x,units)%>%
        dplyr::left_join(metis.mappings("subRegionMap"))%>%
        dplyr::mutate(subRegion=subRegionMetis)%>%
        dplyr::select(-subRegionMetis)%>%
        dplyr::mutate(subRegType="Basin",
                      ag=value*1000,
                      units="km2")%>%
        dplyr::select(-value) %>%
        dplyr::group_by(scenario,subRegion,subRegType,x)%>%
        dplyr::summarize_at(c("ag"),~sum(.,na.rm=T)); agLandBiomasskm2_Basin

      # State
      basin_state <- metis::mapIntersectGCAMBasinUS52@data %>%
        dplyr::select(subRegion=subRegion_GCAMBasin, subRegion_US52,area_sqkm);basin_state

      agLandBiomasskm2_State <- agLandBiomasskm2_Basin %>%
        dplyr::left_join(basin_state) %>%
        dplyr::arrange(subRegion)%>%
        dplyr::group_by(scenario,subRegion,x)%>%
        dplyr::mutate(ratioAreaIntersect=area_sqkm/sum(area_sqkm),
                      ag=ag*ratioAreaIntersect)%>%
        dplyr::ungroup()%>%
        dplyr::mutate(subRegion=subRegion_US52)%>%
        dplyr::select(-subRegion_US52)%>%
        dplyr::group_by(scenario,subRegion,x)%>%
        dplyr::summarize_at(c("ag"),~sum(.,na.rm=T))%>%
        dplyr::mutate(subRegType="State"); agLandBiomasskm2_State

      # National
      agLandBiomasskm2_National <- agLandBiomasskm2_Basin %>%
        dplyr::mutate(subRegion="USA",
                      subRegType="National") %>%
        dplyr::group_by(subRegion,subRegType,x,scenario)%>%
        dplyr::summarize_at(c("ag"),~sum(.,na.rm=T)); agLandBiomasskm2_National

      # Combine
      agLandBiomasskm2 <- agLandBiomasskm2_National %>%
        dplyr::bind_rows(agLandBiomasskm2_State) %>%
        dplyr::bind_rows(agLandBiomasskm2_Basin); agLandBiomasskm2

      # Check
      sum(agLandBiomasskm2_State$ag);sum(agLandBiomasskm2_Basin$ag);sum(agLandBiomasskm2_National$ag);

    }

    # Ag biomass land for elec (agLandEleckm2)
    if(T){
      agLandEleckm2 <- agLandBiomasskm2 %>%
        dplyr::left_join(biomassElecPrimRatio)%>%
        dplyr::mutate(electricity=ag*ratioElec)%>%
        dplyr::select(-ag,-prim,-ratioElec,-ratioPrim); agLandEleckm2
    }

    # Ag biomass land for primary (agLandPrimarykm2)
    if(T){
      agLandPrimarykm2 <- agLandBiomasskm2 %>%
        dplyr::left_join(biomassElecPrimRatio)%>%
      dplyr::mutate(primary=ag*ratioPrim)%>%
        dplyr::select(-ag,-elec,-ratioElec,-ratioPrim,-prim); agLandPrimarykm2
    }

    # Ag total crop land (agLandTotalkm2)
    if(T){
      # Basin
      agLandTotalkm2_Basin <- data.table::fread(paste(dirOutputs_i,"/readGCAM/Tables_gcam/gcamDataTable_aggClass1.csv",sep="")) %>%
        tibble::as_tibble()%>%
        dplyr::filter(scenario %in% scenNewNames_i,
                      param %in% c("landAllocDetail"),
                      class %in% c("biomassGrass","biomassTree",
                                   "Corn","FiberCrop","FodderGrass","FodderHerb",
                                   "MiscCrop","OilCrop","OtherGrain","PalmFruit","Rice",
                                   "RootTuber","SugarCrop","Wheat"))%>%
        dplyr::left_join(metis.mappings("subRegionMap"))%>%
        dplyr::mutate(subRegion=subRegionMetis)%>%
        dplyr::select(-subRegionMetis)%>%
        dplyr::select(scenario,region,subRegion,param,class,value,x,units)%>%
        dplyr::mutate(subRegType="Basin",
                      total=value*1000,
                      units="km2")%>%
        dplyr::select(-value) %>%
        dplyr::group_by(scenario,subRegion,subRegType,x)%>%
        dplyr::summarize_at(c("total"),~sum(.,na.rm=T)); agLandTotalkm2_Basin

      # State
      basin_state <- metis::mapIntersectGCAMBasinUS52@data %>%
        dplyr::select(subRegion=subRegion_GCAMBasin, subRegion_US52,area_sqkm);basin_state

      agLandTotalkm2_State <- agLandTotalkm2_Basin %>%
        dplyr::left_join(basin_state) %>%
        dplyr::arrange(subRegion)%>%
        dplyr::group_by(scenario,subRegion,x)%>%
        dplyr::mutate(ratioAreaIntersect=area_sqkm/sum(area_sqkm),
                      total=total*ratioAreaIntersect)%>%
        dplyr::ungroup()%>%
        dplyr::mutate(subRegion=subRegion_US52)%>%
        dplyr::select(-subRegion_US52)%>%
        dplyr::group_by(scenario,subRegion,x)%>%
        dplyr::summarize_at(c("total"),~sum(.,na.rm=T))%>%
        dplyr::mutate(subRegType="State"); agLandTotalkm2_State

      # National
      agLandTotalkm2_National <- agLandTotalkm2_Basin %>%
        dplyr::mutate(subRegion="USA",
                      subRegType="National") %>%
        dplyr::group_by(subRegion,subRegType,x,scenario)%>%
        dplyr::summarize_at(c("total"),~sum(.,na.rm=T)); agLandTotalkm2_National

      # Combine
      agLandTotalkm2 <- agLandTotalkm2_National %>%
        dplyr::bind_rows(agLandTotalkm2_State) %>%
        dplyr::bind_rows(agLandTotalkm2_Basin); agLandTotalkm2

      # Check
      sum(agLandTotalkm2_State$total);sum(agLandTotalkm2_Basin$total);sum(agLandTotalkm2_National$total);
    }

    # Ag Combine (dficFlow_ag)
    if(T){

      agLandTotalkm2 %>% dplyr::filter(scenario=="Carbon",x==2020,subRegion=="Pacific_and_Arctic_Coast")
      agLandEleckm2 %>% dplyr::filter(scenario=="Carbon",x==2020,subRegion=="Pacific_and_Arctic_Coast")
      agLandPrimarykm2 %>% dplyr::filter(scenario=="Carbon",x==2020,subRegion=="Pacific_and_Arctic_Coast")
      agLandBiomasskm2 %>% dplyr::filter(scenario=="Carbon",x==2020,subRegion=="Pacific_and_Arctic_Coast")

    dficFlow_ag <- agLandTotalkm2 %>%
      dplyr::left_join(agLandEleckm2) %>%
      dplyr::left_join(agLandPrimarykm2) %>%
      dplyr::mutate(water=0,
                    other = total - (electricity+primary+water))%>%
      dplyr::filter(x %in% xRange_i)%>%
      dplyr::ungroup() %>%
      dplyr::select(subRegion, subRegType, x, scenario, other, water, electricity, primary) %>%
      tidyr::gather(key="to",value="value",-subRegion,-subRegType,-x,-scenario)%>%
      dplyr::mutate(from="ag")%>%
      dplyr::distinct();dficFlow_ag
    }

    # Check
    dficFlow_ag %>% dplyr::group_by(subRegType)%>%dplyr::summarize(valueSum=sum(value,na.rm=T))
  }

  # Primary IC (dficFlow_primary)
  #...................
  if(T){

    # Primary (Coal, oil, gas) for Other EJ (primaryEJOther)
    if(T){
    # State
    primaryEJOther_State = data.table::fread(paste(dirOutputs_i,"/readGCAM/Tables_gcam/gcamDataTable.csv",sep="")) %>%
      tibble::as_tibble() %>%
      dplyr::filter(scenario %in% scenNewNames_i,
                    param %in% c("energyFinalByFuelBySectorEJ"),
                    class1 %in% c("coal","gas","liquids")) %>%
      dplyr::select(scenario, subRegion, x,value)%>%
      dplyr::group_by(scenario,subRegion, x)%>%
      dplyr::summarize(value=sum(value,na.rm=T))%>%
      dplyr::ungroup() %>%
      dplyr::mutate(subRegType="State",
                    other=value)%>%
      dplyr::select(-value); primaryEJOther_State %>% as.data.frame()

    # Basin
    basin_state <- metis::mapIntersectGCAMBasinUS52@data %>%
      dplyr::select(subRegion_GCAMBasin, subRegion=subRegion_US52,ratioAreaIntersect);basin_state

    primaryEJOther_Basin <- primaryEJOther_State %>%
      dplyr::left_join(basin_state)%>%
      dplyr::mutate(other=other*ratioAreaIntersect,
                    subRegion=subRegion_GCAMBasin,
                    subRegType="Basin")%>%
      dplyr::select(-subRegion_GCAMBasin,-ratioAreaIntersect)%>%
      dplyr::group_by(subRegion,subRegType,x,scenario)%>%
      dplyr::summarize(other=sum(other,na.rm=T)); primaryEJOther_Basin

    # National
    primaryEJOther_National <- primaryEJOther_State %>%
      dplyr::mutate(subRegion="USA",
                    subRegType="National") %>%
      dplyr::group_by(subRegion,subRegType,x,scenario)%>%
      dplyr::summarize(other=sum(other,na.rm=T)); primaryEJOther_National

    # Combine
    primaryEJOther <-  primaryEJOther_State %>%
      dplyr::bind_rows(primaryEJOther_Basin) %>%
      dplyr::bind_rows(primaryEJOther_National); primaryEJOther

    # Checks
    primaryEJOther %>% dplyr::group_by(subRegType)%>%summarize(sums=sum(other,na.rm=T))

    }

    # Primary (Coal, oil, gas) for Ag EJ (primaryEJAg)
    if(T){
    # Ag total Land from above (agLandTotalkm2)
    agLandTotalkm2

    # Primary for ag coefficient (primforAgCoefficient_EJ_per_km2)
    if(T){
      # https://www.ers.usda.gov/webdocs/publications/74658/60128_eib159.pdf?v=3118.7
      # Figure 3
      # 2015 Diesel 2015 = 410 BTU
      # Gasoline 2015 = 100 BTU
      # Natural Gas 2015 = 150 BTU
      # LP Gas 2015 = 80 BTU
      # Total = 740 BTU
      # Total land area km2 for crops and livestock
      # primForAgCoefficient in Trillion BTU /km2 - Convert to EJ/km2
      # https://www.convert-measurement-units.com/conversion-calculator.php?type=energy

      primForAg_EJ_2015 = 740 * 0.00105505585262; primForAg_EJ_2015

      ag_land_km2_2015 <- data.table::fread(paste(dirOutputs_i,"/readGCAM/Tables_gcam/gcamDataTable_aggClass1.csv",sep="")) %>%
        tibble::as_tibble() %>%
        dplyr::filter(scenario %in% "Ref",
                      param %in% c("landAlloc"),
                      x==2015)%>%
        dplyr::mutate(value=value*1000,
                      units="km2")%>%
        dplyr::select(scenario,region,subRegion,param,class,x,units,value)%>%
        dplyr::filter(class %in% c("crops","pasture","RootTuber"))%>%
        dplyr::group_by(scenario,region,subRegion,param,x,units)%>%
        dplyr::summarize(value=sum(value,na.rm=T)); ag_land_km2_2015

      primforAgCoefficient_EJ_per_km2 = primForAg_EJ_2015/ag_land_km2_2015$value; primforAgCoefficient_EJ_per_km2
    }

    # Combine
      primaryEJAg <- agLandTotalkm2 %>%
        dplyr::rename(ag=total)%>%
        dplyr::mutate(ag=ag*primforAgCoefficient_EJ_per_km2)

    # Checks
    primaryEJAg %>% dplyr::group_by(subRegType)%>%summarize(sums=sum(ag,na.rm=T))

    }

    # Primary (Coal, oil, gas) for Elec EJ (primaryEJElec)
    if(T){
      # State
      primaryEJElec_State = data.table::fread(paste(dirOutputs_i,"/readGCAM/Tables_gcam/gcamDataTable_aggClass1.csv",sep="")) %>%
        tibble::as_tibble() %>%
        dplyr::filter(scenario %in% scenNewNames_i,
                      param %in% c("elecByTechTWh"),
                      class %in% c("coal","gas","liquids")) %>%
        dplyr::select(scenario, subRegion, x,value)%>%
        dplyr::group_by(scenario,subRegion, x)%>%
        dplyr::summarize(value=sum(value,na.rm=T))%>%
        dplyr::ungroup() %>%
        dplyr::mutate(subRegType="State",
                      electricity=value/metis.assumptions("convEJ2TWh"))%>%
        dplyr::select(-value); primaryEJElec_State %>% as.data.frame()

      # Basin
      basin_state <- metis::mapIntersectGCAMBasinUS52@data %>%
        dplyr::select(subRegion_GCAMBasin, subRegion=subRegion_US52,ratioAreaIntersect);basin_state

      primaryEJElec_Basin <- primaryEJElec_State %>%
        dplyr::left_join(basin_state)%>%
        dplyr::mutate(electricity=electricity*ratioAreaIntersect,
                      subRegion=subRegion_GCAMBasin,
                      subRegType="Basin")%>%
        dplyr::select(-subRegion_GCAMBasin,-ratioAreaIntersect)%>%
        dplyr::group_by(subRegion,subRegType,x,scenario)%>%
        dplyr::summarize(electricity=sum(electricity,na.rm=T)); primaryEJElec_Basin

      # National
      primaryEJElec_National <- primaryEJElec_State %>%
        dplyr::mutate(subRegion="USA",
                      subRegType="National") %>%
        dplyr::group_by(subRegion,subRegType,x,scenario)%>%
        dplyr::summarize(electricity=sum(electricity,na.rm=T)); primaryEJElec_National

      # Combine
      primaryEJElec <-  primaryEJElec_National %>%
        dplyr::bind_rows(primaryEJElec_State) %>%
        dplyr::bind_rows(primaryEJElec_Basin); primaryEJElec

      # Checks
      primaryEJElec %>% dplyr::group_by(subRegType)%>%summarize(sums=sum(electricity,na.rm=T))

    }

    # Primary Combine
    if(T){
    dficFlow_primary <- primaryEJOther %>%
      dplyr::left_join(primaryEJAg) %>%
      dplyr::left_join(primaryEJElec) %>%
      dplyr::left_join(primaryEJOther%>%dplyr::rename(water=other)%>%dplyr::mutate(water=0))%>%
      dplyr::mutate(total = other + electricity + ag + water)%>%
      dplyr::filter(x %in% xRange_i)%>%
      dplyr::ungroup() %>%
      dplyr::select(subRegion, subRegType, x, scenario, other, water, ag, electricity) %>%
      tidyr::gather(key="to",value="value",-subRegion,-subRegType,-x,-scenario)%>%
      dplyr::mutate(from="primary")%>%
      dplyr::distinct();dficFlow_primary

    # Checks
    dficFlow_primary %>% dplyr::group_by(subRegType,to)%>%summarize(sums=sum(value,na.rm=T))%>%arrange(to)
    dficFlow_primary %>% dplyr::group_by(subRegType)%>%summarize(sums=sum(value,na.rm=T))
    }
  }

  # Combined ic indices and flows
  #...................
  if(T){
  dficFlow <- dficFlow_watkm3 %>%
    dplyr::bind_rows(dficFlow_elecTWh) %>%
    dplyr::bind_rows(dficFlow_ag) %>%
    dplyr::bind_rows(dficFlow_primary) %>%
    dplyr::group_by(subRegion,subRegType,x,units, to, from, scenario)%>%
    dplyr::summarize(value=mean(value,na.rm=T))%>%
    ungroup(); dficFlow

  # dficFlow_abs <- dficFlow_wat_abs; dficFlow_abs
  dficFlow%>%head();

  # Check
  dficFlow_watkm3%>%dplyr::ungroup()%>%dplyr::group_by(subRegType)%>%summarize(value=sum(value,na.rm=T))
  dficFlow_ag%>%dplyr::ungroup()%>%dplyr::group_by(subRegType)%>%summarize(value=sum(value,na.rm=T))
  dficFlow_elecTWh%>%dplyr::ungroup()%>%dplyr::group_by(subRegType)%>%summarize(value=sum(value,na.rm=T))
  dficFlow_primary%>%dplyr::ungroup()%>%dplyr::group_by(subRegType)%>%summarize(value=sum(value,na.rm=T))
  dficFlow%>%dplyr::ungroup()%>%dplyr::group_by(subRegType)%>%summarize(value=sum(value,na.rm=T))

  }

}

# Interconectivity Chord Diagram and Matrix
if(T){

  metis.ic(data=dficFlow %>% filter(subRegType=="National",
                                    x %in% c(min(xRange_i),max(xRange_i))),
           icSectors = c("ag","electricity","water","primary"),
           dirOutputs=dirIC,
           nameAppend="National",
           printFig=T,
           saveFile=T)

  metis.ic(data=dficFlow,
           icSectors = c("ag","electricity","water","primary"),
           dirOutputs=dirIC,
           nameAppend="All",
           printFig=F,
           saveFile=T)

  selectedRegions_i <- c("CA","KS","TX","MD","UT")
  metis.ic(data=dficFlow %>%
             dplyr::filter(subRegion %in% selectedRegions_i,
                           x %in% c(min(xRange_i),max(xRange_i))),
           icSectors = c("ag","electricity","water","primary"),
           dirOutputs=dirIC,
           nameAppend="Select",
           printFig=T,
           saveFile=T)

}

# Interconectivity Chord Diagram and Matrix Exammples
if(T){

  mat1 = tibble::tribble(
    ~"ag", ~"electricity", ~"other", ~"primary", ~"water",
    0, 0, 0,1,0,
    1, 0, 0,0,0,
    0, 1, 0,0,0,
    0, 0, 0,1,0,
  )%>%as.matrix(); mat1
  rownames(mat1) <- c("ag","electricity","primary","water"); mat1

  mat2 = tibble::tribble(
    ~"ag", ~"electricity", ~"other", ~"primary", ~"water",
    0, 0.33, 0,0.33,0.33,
    0.33, 0, 0,0.33,0.33,
    0.33, 0.33, 0,0,0.33,
    0.33, 0.33, 0,0.33,0,
  )%>%as.matrix(); mat2
  rownames(mat2) <- c("ag","electricity","primary","water"); mat2

  mat3 = tibble::tribble(
    ~"ag", ~"electricity", ~"other", ~"primary", ~"water",
    0, 0, 1,0,0,
    0, 0, 1,0,0,
    0, 0, 1,0,0,
    0, 0, 1,0,0,
  )%>%as.matrix(); mat3
  rownames(mat3) <- c("ag","electricity","primary","water"); mat3

  mat4 = tibble::tribble(
    ~"ag", ~"electricity", ~"other", ~"primary", ~"water",
    0, 0.01, 1,0.01,0.01,
    0.01, 0, 1,0.01,0.01,
    0.01, 0.01, 1,0,0.01,
    0.01, 0.01, 1,0.01,0,
  )%>%as.matrix(); mat4
  rownames(mat4) <- c("ag","electricity","primary","water"); mat4

  matList <- list(mat1,mat2,mat3,mat4)

  df <- data.frame()
  for(i in 1:length(matList)){
  mat = matList[[i]]
  df <- df %>%
    bind_rows(data.frame(from = row.names(mat),mat) %>%
                tidyr::gather(key="to",value="value",-from)%>%
                dplyr::mutate(scenario=paste("scenario",i,sep="")))
  }; df

  metis.ic(data=df,
           dirOutputs=dirIC,
           nameAppend="Examples",
           printFig=T,
           saveFile=T)

  # data=df
  # dirOutputs=dirIC
  # nameAppend="Examples"
  # printFig=T
  # saveFile=T


}

# Chart Interconectivity
if(T){

  if(!dir.exists(paste(dirIC,"/charts",sep=''))){
    dir.create(paste(dirIC,"/charts",sep=''))}

  dirIC_i <- gsub("//","/",paste(dirIC,"/charts",sep='')); dirIC_i

  dfic <- data.table::fread(paste(dirIC,"/chordMatrixAll/icAll_ic.csv",sep=""),header=T)%>%
    tibble::as_tibble(); dfic

  dficSec <- data.table::fread(paste(dirIC,"/chordMatrixAll/icAll_ic_all.csv",sep=""),header=T)%>%
    tibble::as_tibble(); dficSec

  # Combined Chart Ref
  if(T){

    # Combined Values
    dficMean <- dfic %>%
      dplyr::select(subRegion, subRegType,x,scenario, ic_sec,ic_spread)%>%
      tidyr::gather(key="icType",value="value",-subRegion,-subRegType,-scenario,-x)%>%
      dplyr::group_by(scenario,subRegType,icType,x)%>%
      dplyr::summarise(valNorm=value/max(value,na.rm=T),
                       valMean=mean(value,na.rm=T),
                       valMax=max(value,na.rm=T),
                       valMin=min(value,na.rm=T),
                       valSD=sd(value,na.rm=T))%>%
      dplyr::ungroup();dficMean

    dficLong <- dfic %>%
      dplyr::select(subRegion, subRegType,x,scenario, ic_sec,ic_spread)%>%
      tidyr::gather(key="icType",value="value",-subRegion,-subRegType,-scenario,-x);dficLong

    g <- ggplot(dficMean %>%
                  dplyr::filter(scenario=="Ref")%>%
                  dplyr::mutate(subRegType=factor(subRegType, levels =orderSubregType_i))%>%
                  dplyr::arrange(factor(subRegType, levels =orderSubregType_i)),
                aes(x=x, y=valMean, color=subRegType)) +
      theme_bw() +
      theme(strip.background = element_rect(colour="white", fill="white"))+
      #geom_line() +
      geom_point(size=5, position=position_dodge(5)) + xlab("") + ylab("") +
      geom_errorbar(aes(ymin=valMin, ymax=valMax), width=5,
                    position=position_dodge(5))+
      expand_limits(y = 0) +
      scale_color_manual(values=colorsSubregType_i) +
      theme(axis.text.x= element_text(angle=90, size=12, vjust=0.25, hjust=1)) +
      theme(axis.text.y= element_text(size=12))+
      facet_grid(.~icType); g
    ggsave(filename=paste(dirIC_i,"/indexScenSpatMeanBarREF.png",sep=''),plot=g,width=10,height=6)

    if(F){
    g <- ggplot(dficLong %>%
                  dplyr::filter(scenario=="Ref", subRegType=="State"),
                aes(x=x, y=value, color=subRegion)) +
      geom_line() +
      #geom_point(size=5, position=position_dodge(5)) + xlab("") + ylab("") +
      expand_limits(y = 0) +
      #scale_color_manual(values=colorsSubregType_i) +
      theme(axis.text.x= element_text(angle=90, size=12, vjust=0.25, hjust=1)) +
      theme(axis.text.y= element_text(size=12))+
      facet_grid(.~icType); g
    ggsave(filename=paste(dirIC_i,"/indexScenStateSpreadREF.png",sep=''),plot=g,width=15,height=6)


    # Norm Spread
    # Change Over Years
    dfixFirstYear <- dficLong %>%
      dplyr::filter(x==min(x)) %>%
      dplyr::select(subRegion, icType,scenario,value)%>%
      dplyr::rename(valueFirstYear=value)%>%
      dplyr::ungroup(); dfixFirstYear
    dfix <- dficLong  %>%
      dplyr::left_join(dfixFirstYear)%>%
      dplyr::mutate(value=value/valueFirstYear)%>%
      dplyr::ungroup(); dfix
    g <- ggplot(dfix %>%
                  dplyr::filter(subRegType=="State",
                                !subRegion %in% c("AK","HI")) ,
                aes(x=x, y=value, color=subRegion)) +
      geom_text(aes(label=subRegion),size=8) +
      #geom_point() +
      xlab("") + ylab("") + #ylim(c(0.25,1.75)) +
      scale_color_manual(values=metis.colors()$pal_16) +
      theme(axis.text.x= element_text(angle=90, size=12, vjust=0.25, hjust=1)) +
      theme(axis.text.y= element_text(size=12)) +
      theme(legend.position = "none")+
      facet_grid(scenario~icType,scales="free_y"); g
    ggsave(filename=paste(dirIC_i,"/indexScenStateSpreadREFLabels.png",sep=''),plot=g,width=15,height=15)
    }

    g <- ggplot(dficMean %>%
                  dplyr::filter(scenario=="Ref", subRegType=="National")%>%
                  dplyr::mutate(icType=factor(icType, levels =orderICType_i))%>%
                  dplyr::arrange(factor(icType, levels =orderICType_i)),
                aes(x=x, y=valMean, color=icType)) +
      theme_bw() +
      theme(strip.background = element_rect(colour="white", fill="white"))+
      geom_line() +
      geom_point(size=3) + xlab("") + ylab("") +
      expand_limits(y = 0) +
      scale_color_manual(values=colorsICType_i) +
      theme(axis.text.x= element_text(angle=90, size=12, vjust=0.25, hjust=1)) +
      theme(axis.text.y= element_text(size=12)) +
      theme(legend.position="bottom"); g
    ggsave(filename=paste(dirIC_i,"/indexREFNational.png",sep=''),plot=g,width=7,height=5)

    # Sectoral IC
    dficMeanSec <- dficSec %>%
      dplyr::select(subRegion, subRegType,x,scenario, ic_sec,ic_spread, from)%>%
      tidyr::gather(key="icType",value="value",-subRegion,-subRegType,-scenario,-x, -from)%>%
      dplyr::group_by(scenario,subRegType,icType,x, from)%>%
      dplyr::summarise(valNorm=value/max(value,na.rm=T),
                       valMean=mean(value,na.rm=T),
                       valMax=max(value,na.rm=T),
                       valMin=min(value,na.rm=T),
                       valSD=sd(value,na.rm=T))%>%
      dplyr::ungroup();dficMeanSec

    if(F){
    g <- ggplot(dficMeanSec %>%
                  dplyr::filter(scenario=="Ref")%>%
                  dplyr::mutate(subRegType=factor(subRegType, levels =orderSubregType_i))%>%
                  dplyr::arrange(factor(subRegType, levels =orderSubregType_i)),
                aes(x=x, y=valMean, color=subRegType)) +
      #geom_line() +
      geom_point(size=5, position=position_dodge(5)) + xlab("") + ylab("") +
      geom_errorbar(aes(ymin=valMin, ymax=valMax), width=5,
                    position=position_dodge(5))+
      expand_limits(y = 0) +
      scale_color_manual(values=colorsSubregType_i) +
      theme(axis.text.x= element_text(angle=90, size=12, vjust=0.25, hjust=1)) +
      theme(axis.text.y= element_text(size=12))+
      facet_grid(from~icType); g
    ggsave(filename=paste(dirIC_i,"/indexScenSpatMeanBarREF_Sec.png",sep=''),plot=g,width=10,height=10)
    }

    g <- ggplot(dficMeanSec %>%
                  dplyr::filter(scenario=="Ref", subRegType=="National")%>%
                  dplyr::mutate(icType=factor(icType, levels =orderICType_i))%>%
                  dplyr::arrange(factor(icType, levels =orderICType_i)),
                aes(x=x, y=valMean, color=icType)) +
      theme_bw() +
      theme(strip.background = element_rect(colour="white", fill="white"))+
      geom_line() +
      expand_limits(y = 0) +
      geom_point(size=3) + xlab("") + ylab("") +
      scale_color_manual(values=colorsICType_i) +
      theme(axis.text.x= element_text(angle=90, size=12, vjust=0.25, hjust=1)) +
      theme(axis.text.y= element_text(size=12))+
      facet_grid(.~from); g
    ggsave(filename=paste(dirIC_i,"/indexREFNational_Sec.png",sep=''),plot=g,width=10,height=6)

  }

  # Compare Scenarios
  # National
  if(T){
    # Combined Values
    dficMean <- dfic %>%
      dplyr::filter(subRegType=="National")%>%
      dplyr::select(subRegion, subRegType,x,scenario, ic_sec,ic_spread)%>%
      tidyr::gather(key="icType",value="value",-subRegion,-subRegType,-scenario,-x)%>%
      dplyr::group_by(scenario,subRegType,icType,x)%>%
      dplyr::summarise(valMean=mean(value,na.rm=T))%>%
      dplyr::ungroup();dficMean
    dficMean_spread <- dficMean %>%
      dplyr::select(scenario,subRegType,icType,x,valMean)%>%
      tidyr::spread(key="scenario",value="valMean");dficMean_spread

    dficMean_DiffAbs <- dficMean_spread; dficMean_DiffPrcnt <- dficMean_spread;
    for(scenario_i in unique(dficMean$scenario)[!unique(dficMean$scenario) %in% c("Ref")]){
      dficMean_DiffAbs <- dficMean_DiffAbs %>%
        dplyr::mutate(!!as.name(scenario_i) := !!as.name(scenario_i)-Ref)
      dficMean_DiffPrcnt <- dficMean_DiffPrcnt %>%
        dplyr::mutate(!!as.name(scenario_i) := (!!as.name(scenario_i)-Ref)*100/Ref)
    }
    dficMean_DiffAbs <- dficMean_DiffAbs %>%
      dplyr::select(-Ref)%>%
      tidyr::gather(key="scenario",value="valMean",-subRegType,-icType,-x);
    dficMean_DiffPrcnt <- dficMean_DiffPrcnt %>%
      dplyr::select(-Ref)%>%
      tidyr::gather(key="scenario",value="valMean",-subRegType,-icType,-x)

    dficMeanDiff <- dficMean %>% dplyr::mutate(type="Value") %>%
      dplyr::bind_rows(dficMean_DiffAbs %>% dplyr::mutate(type="Absolute Difference")) %>%
      dplyr::bind_rows(dficMean_DiffPrcnt %>% dplyr::mutate(type="% Difference"))

    g <- ggplot(dficMeanDiff %>%
                  dplyr::filter(type!="Absolute Difference")%>%
                  dplyr::mutate(scenario=factor(scenario, levels =orderScenarios_i))%>%
                  dplyr::arrange(factor(scenario, levels =orderScenarios_i))%>%
                  dplyr::mutate(type=factor(type, levels =c("Value","% Difference","Absolute Difference")))%>%
                  dplyr::arrange(factor(type, levels =c("Value","% Difference","Absolute Difference"))),
                aes(x=x, y=valMean, color=scenario)) +
      theme_bw() +
      theme(strip.background = element_rect(colour="white", fill="white"))+
      geom_line() +
      geom_point(size=3) + xlab("") + ylab("") +
      expand_limits(y = 0) +
      scale_color_manual(values=colorsScenarios_i) +
      theme(axis.text.x= element_text(angle=90, size=12, vjust=0.25, hjust=1)) +
      theme(axis.text.y= element_text(size=12)) +
      theme(legend.position="bottom")+
      facet_grid(type~icType,scales= "free_y"); g
    ggsave(filename=paste(dirIC_i,"/indexNationalDiffPrcnt.png",sep=''),plot=g,width=6,height=6)
  }

  # By State
  if(T){
    # Combined Values
    states <- c("TX","MT","MD","CA","ME")

    for(state_i in states){
      dficMean <- dfic %>%
        dplyr::filter(subRegType=="State")%>%
        dplyr::filter(subRegion==state_i)%>%
        dplyr::select(subRegion, subRegType,x,scenario, ic,ic_sec,ic_spread)%>%
        tidyr::gather(key="icType",value="value",-subRegion,-subRegType,-scenario,-x)%>%
        dplyr::group_by(scenario,subRegType,icType,x)%>%
        dplyr::summarise(valMean=mean(value,na.rm=T))%>%
        dplyr::ungroup();dficMean
      dficMean_spread <- dficMean %>%
        dplyr::select(scenario,subRegType,icType,x,valMean)%>%
        tidyr::spread(key="scenario",value="valMean");dficMean_spread

      dficMean_DiffAbs <- dficMean_spread; dficMean_DiffPrcnt <- dficMean_spread;
      for(scenario_i in unique(dficMean$scenario)[!unique(dficMean$scenario) %in% c("Ref")]){
        dficMean_DiffAbs <- dficMean_DiffAbs %>%
          dplyr::mutate(!!as.name(scenario_i) := !!as.name(scenario_i)-Ref)
        dficMean_DiffPrcnt <- dficMean_DiffPrcnt %>%
          dplyr::mutate(!!as.name(scenario_i) := (!!as.name(scenario_i)-Ref)*100/Ref)
      }
      dficMean_DiffAbs <- dficMean_DiffAbs %>%
        dplyr::select(-Ref)%>%
        tidyr::gather(key="scenario",value="valMean",-subRegType,-icType,-x);
      dficMean_DiffPrcnt <- dficMean_DiffPrcnt %>%
        dplyr::select(-Ref)%>%
        tidyr::gather(key="scenario",value="valMean",-subRegType,-icType,-x)

      dficMeanDiff <- dficMean %>% dplyr::mutate(type="Value") %>%
        dplyr::bind_rows(dficMean_DiffAbs %>% dplyr::mutate(type="Absolute Difference")) %>%
        dplyr::bind_rows(dficMean_DiffPrcnt %>% dplyr::mutate(type="% Difference"))

      g <- ggplot(dficMeanDiff %>%
                    dplyr::filter(type!="Absolute Difference")%>%
                    dplyr::mutate(scenario=factor(scenario, levels =orderScenarios_i))%>%
                    dplyr::arrange(factor(scenario, levels =orderScenarios_i))%>%
                    dplyr::mutate(type=factor(type, levels =c("Value","% Difference","Absolute Difference")))%>%
                    dplyr::arrange(factor(type, levels =c("Value","% Difference","Absolute Difference"))),
                  aes(x=x, y=valMean, color=scenario)) +
        geom_line() +
        geom_point(size=3) + xlab("") + ylab("") +
        expand_limits(y = 0) +
        scale_color_manual(values=colorsScenarios_i) +
        theme(axis.text.x= element_text(angle=90, size=12, vjust=0.25, hjust=1)) +
        theme(axis.text.y= element_text(size=12)) +
        theme(legend.position="bottom")+
        facet_grid(type~icType,scales= "free_y"); g
      ggsave(filename=paste(dirIC_i,"/indexNationalDiffPrcntState_",state_i,".png",sep=''),plot=g,width=6,height=6)
    }
  }
}

# Map Interconnectiivty
if(T){

  dfic <- data.table::fread(paste(dirIC,"/chordMatrixAll/icAll_ic.csv",sep=""),header=T)%>%
    tibble::as_tibble(); dfic

  # Plot ic index maps
   dfic_map <- dfic %>%
      dplyr::filter(subRegType=="State",
                    subRegion %in% c(metis::metis.assumptions("US49")),
                    x %in% c(min(xRange_i),max(xRange_i))) %>%
      dplyr::select(subRegion,subRegType,x,scenario,ic,ic_spread,ic_sec)%>%
      dplyr::group_by(subRegion,subRegType,x,scenario)%>%
      tidyr::gather(key="param",value="value",-subRegion,-subRegType,-x,-scenario);dfic_map

   dfic_map$x%>%unique()

    metis.mapsProcess(polygonTable=dfic_map %>% filter(scenario=="Ref",param %in% c("ic_sec","ic_spread")),
                      dirOutputs=dirIC,
                      scaleRange = c(0,0.4),
                      #scaleRangeDiffPrcnt = c(-100,100),
                      #scaleRangeDiffxPrcnt = c(-100,100),
                      #scenRef = "Ref",
                      #xRef = 2015,
                      animateOn = F,
                      legendFixedBreaks=7)

    # Top ic_sec and ic locations

    # Plot ic index maps
    dfic %>%
      dplyr::group_by(subRegType,scenario,x)%>%
      dplyr::filter(subRegType=="State",
                    subRegion %in% c(metis::metis.assumptions("US49")),
                    x %in% c(min(xRange_i),max(xRange_i)))%>%
      dplyr::summarize(mean_ic_sec=mean(ic_sec),mean_ic_spread=mean(ic_spread))

    dfic_map <- dfic %>%
      dplyr::group_by(subRegType,scenario,x)%>%
      dplyr::filter(subRegType=="State",
                    subRegion %in% c(metis::metis.assumptions("US49")),
                    x %in% c(min(xRange_i),max(xRange_i)),
                    ic_sec>0.25, ic_spread>0.25) %>%
                    #ic_sec>mean(ic_sec), ic_spread>mean(ic_spread)) %>%
      dplyr::ungroup()%>%
      dplyr::select(subRegion,subRegType,x,scenario,ic_spread,ic_sec)%>%
      dplyr::group_by(subRegion,subRegType,x,scenario)%>%
      tidyr::gather(key="class",value="value",-subRegion,-subRegType,-x,-scenario)%>%
      dplyr::mutate(param="ic");dfic_map


    metis.mapsProcess(polygonTable=dfic_map,
                      dirOutputs=dirIC,
                      scaleRange = c(0,0.5),
                      #scaleRangeDiffPrcnt = c(-100,100),
                      #scaleRangeDiffxPrcnt = c(-100,100),
                      #scenRef = "Ref",
                      #xRef = 2015,
                      animateOn = F,
                      legendFixedBreaks=7,
                      nameAppend = "MAXCombSecSpread")

}

# Map Interconnectivity flow params
if(T){

  # Plot flows absolute
  dfic_map <- dficFlow %>%
    dplyr::filter(subRegType=="State",
                  subRegion %in% c(metis::metis.assumptions("US49")),
                  x %in% c(min(xRange_i),max(xRange_i))) %>%
    dplyr::rename(param=from, class=to)%>%
    dplyr::mutate(classPalette=case_when(
      param=="water"~"pal_wet",
      param=="electricity"~"pal_hot",
      param=="primary"~"Purples",
      param=="ag"~"pal_green",
      TRUE~"pal_spectral"
    ));dfic_map

  dfic_map$x%>%unique()

  metis.mapsProcess(polygonTable=dfic_map,
                    dirOutputs=dirIC,
                    #scaleRange = c(0,0.5),
                    #scaleRangeDiffPrcnt = c(-100,100),
                    #scaleRangeDiffxPrcnt = c(-100,100),
                    scenRef = "Ref",
                    xRef = 2015,
                    animateOn = F,
                    legendFixedBreaks=9,
                    nameAppend = "icFlowsAbs")

  # Flow ratios
  dfic_ratios <- dfic_map %>%
    dplyr::group_by(subRegion,subRegType,x,units,param,scenario)%>%
    dplyr::mutate(value=value/sum(value,na.rm=T))%>%
    ungroup(); dfic_ratios

   metis.mapsProcess(polygonTable=dfic_ratios,
                    dirOutputs=dirIC,
                    #scaleRange = c(0,0.5),
                    #scaleRangeDiffPrcnt = c(-100,100),
                    #scaleRangeDiffxPrcnt = c(-100,100),
                    scenRef = "Ref",
                    xRef = 2015,
                    animateOn = F,
                    legendFixedBreaks=5,
                    nameAppend = "icFlowsRatio")
}


#..............................
# Compare Scenarios and Correlations
#...............................

# Compare ic_sec vs ic_pread
if(T){

    # Create Folder
    if(!dir.exists(paste(dirOutputs_i,"/",dirComb,"/",sep=''))){
      dir.create(paste(dirOutputs_i,"/",dirComb,"/",sep=''))}
    if(!dir.exists(paste(dirOutputs_i,"/",dirComb,"/CorrelationsIC",sep=''))){
      dir.create(paste(dirOutputs_i,"/",dirComb,"/CorrelationsIC",sep=''))}
    dirIC_i <- paste(dirOutputs_i,"/",dirComb,"/CorrelationsIC",sep='')


    dfic <- data.table::fread(paste(dirIC,"/chordMatrixAll/icAll_ic.csv",sep=""),header=T)%>%
      tibble::as_tibble(); dfic

    dficcomp <- dfic %>% dplyr::select(subRegion,subRegType,x,scenario,region,ic_sec,ic_spread)

    for(x_i in c(2015,2100)){
      for(subRegType_i in unique(dficcomp$subRegType)){

        g <- ggplot(dficcomp %>%
                      dplyr::filter(x==x_i,
                                    subRegType==subRegType_i,
                                    !subRegion %in% subRegionsRemove),
                    aes(y=ic_sec, x=ic_spread, color=scenario)) +
          theme_bw() +
          ggtitle(x_i)+
          theme(strip.background = element_rect(colour="white", fill="white"))+
          geom_point(size=0)+ guides(colour = guide_legend(override.aes = list(size=1)))+
          geom_text(aes(label=subRegion),size=3,show.legend = FALSE) +
          scale_color_manual(values=colorsScenarios_i[unique(dficcomp$scenario)%>%as.character()]) +
          theme(axis.text.x= element_text(angle=90, size=12, vjust=0.25, hjust=1)) +
          theme(axis.text.y= element_text(size=12)) +
          theme(legend.position="bottom")+
          theme(plot.title = element_text(hjust = 0.5),
                axis.title.y = element_text(vjust = 2)); g
        ggsave(filename=paste(dirIC_i,"/ic_",x_i,"_",subRegType_i,"_icsecVSicspread.png",sep=''),plot=g,width=6,height=6)
        print(paste("File saved as: ",dirIC_i,"/ic_",x_i,"_",subRegType_i,"_icsecVSicspread.png",sep=''))
      }
    }
  }


# Compare Ratios
if(T){

  # Create Folder
  if(!dir.exists(paste(dirOutputs_i,"/",dirComb,"/",sep=''))){
    dir.create(paste(dirOutputs_i,"/",dirComb,"/",sep=''))}
  if(!dir.exists(paste(dirOutputs_i,"/",dirComb,"/CorrelationsRatios",sep=''))){
    dir.create(paste(dirOutputs_i,"/",dirComb,"/CorrelationsRatios",sep=''))}
  dirIC_i <- paste(dirOutputs_i,"/",dirComb,"/CorrelationsRatios",sep='')

  dfic <- data.table::fread(paste(dirIC,"/chordMatrixAll/icAll_ic.csv",sep=""),header=T)%>%
    tibble::as_tibble(); dfic

  dficSec <- data.table::fread(paste(dirIC,"/chordMatrixAll/icAll_ic_all.csv",sep=""),header=T)%>%
    tibble::as_tibble(); dficSec

  # Prep Data
  if(T){
    ic <- dfic %>%
      dplyr::select(-rankic,-rankic_sec,-rankic_spread,-region)%>%
      tidyr::gather(key="type",value="value",-subRegion,-subRegType,-x,-scenario); ic

    dfic_ratios <- dficFlow %>%
      dplyr::filter(x %in% c(min(xRange_i),max(xRange_i))) %>%
      dplyr::rename(param=from, class=to)%>%
      dplyr::group_by(subRegion,subRegType,x,units,param,scenario)%>%
      dplyr::mutate(value=value/sum(value,na.rm=T))%>%
      ungroup(); dfic_ratios


  compareICParam <- ic %>%
    dplyr::bind_rows(dfic_ratios%>%
                       dplyr::mutate(type=paste(param,"_",class,sep=""))%>%
                       dplyr::select(-units,-class,-param));compareICParam

  compareICParam_spread <- compareICParam %>%
    tidyr::spread(key="scenario",value="value"); compareICParam_spread

  compareICParam_DiffAbs <- compareICParam_spread; compareICParam_DiffPrcnt <- compareICParam_spread;
  for(scenario_i in unique(compareICParam$scenario)[!unique(compareICParam$scenario) %in% c("Ref")]){
    compareICParam_DiffAbs <- compareICParam_DiffAbs %>%
      dplyr::mutate(!!as.name(scenario_i) := !!as.name(scenario_i)-Ref)
    compareICParam_DiffPrcnt <- compareICParam_DiffPrcnt %>%
      dplyr::mutate(!!as.name(scenario_i) := (!!as.name(scenario_i)-Ref)*100/Ref)
  }
  compareICParam_DiffAbs <- compareICParam_DiffAbs %>%
    dplyr::select(-Ref)%>%
    tidyr::gather(key="scenario",value="value",-subRegType,-subRegion,-x,-type)%>%
    dplyr::mutate(type=paste("diffAbs_",type,sep=""));
  compareICParam_DiffPrcnt <- compareICParam_DiffPrcnt %>%
    dplyr::select(-Ref)%>%
    tidyr::gather(key="scenario",value="value",-subRegType,-subRegion,-x,-type)%>%
    dplyr::mutate(type=paste("diffPrcnt_",type,sep=""));

  compareICParamDiff <- compareICParam %>%
    dplyr::bind_rows(compareICParam_DiffAbs) %>%
    dplyr::bind_rows(compareICParam_DiffPrcnt)

  compareICParamDiff_spread <- compareICParamDiff %>%
    tidyr::spread(key="type",value="value"); compareICParamDiff_spread
  }

  # Plot Comparison Figures
  if(T){

    names(compareICParamDiff_spread)
    namesIC <- names(compareICParamDiff_spread)[grepl("_ic|ic_",names(compareICParamDiff_spread))]; namesIC
    namesICnot <-  names(compareICParamDiff_spread)[!grepl("_ic|ic_|^ic$",names(compareICParamDiff_spread))]; namesICnot
    namesParam <- names(compareICParamDiff_spread)[!grepl("_ic|ic_|^ic$|subRegion|subRegType|x|scenario",names(compareICParamDiff_spread))]; namesParam

    xCompare = c(2100)

    compareICParamDiff_long <- compareICParamDiff_spread %>%
      tidyr::gather(key="icType",value="icValue",-namesICnot)%>%
      tidyr::gather(key="paramType",value="paramValue",-subRegion,-subRegType,-x,-scenario,-icType,-icValue)%>%
      dplyr::filter(!is.na(icValue),!is.na(paramValue)); compareICParamDiff_long

    # Values
    dfx <-compareICParamDiff_long %>%
      dplyr::select(subRegion,subRegType,x,scenario,icType,paramType,icValue,paramValue)%>%
      dplyr::mutate(scenario=factor(scenario, levels =orderScenarios_i))%>%
      dplyr::arrange(factor(scenario, levels =orderScenarios_i))%>%
      droplevels(); dfx$scenario%>%unique();
    dfx$icType%>%unique(); dfx$paramType%>%unique();dfx$subRegion%>%unique();dfx$subRegType%>%unique()

    # Paper figure
    if(T){
           g <- ggplot(dfx %>%
                          dplyr::filter(x==2100,
                                        grepl("^ic_sec$|^ic_spread$",icType),
                                        grepl("^water_ag",paramType),
                                        subRegType=="State",
                                        !subRegion %in% subRegionsRemove),
                        aes(y=icValue, x=paramValue, color=scenario)) +
              theme_bw() +
              ggtitle("2100")+
              theme(strip.background = element_rect(colour="white", fill="white"))+
              geom_point(size=0)+ guides(colour = guide_legend(override.aes = list(size=1)))+
              geom_text(aes(label=subRegion),size=3,show.legend = FALSE) +
              scale_color_manual(values=colorsScenarios_i[unique(dfx$scenario)%>%as.character()]) +
              theme(axis.text.x= element_text(angle=90, size=12, vjust=0.25, hjust=1)) +
              theme(axis.text.y= element_text(size=12)) +
              theme(legend.position="bottom")+
              theme(plot.title = element_text(hjust = 0.5),
                    axis.title.y = element_text(vjust = 2))+
              facet_grid(paramType~icType,scales= "free_y"); g
            ggsave(filename=paste(dirIC_i,"/ic_water_ag_2100_State_valPaper.png",sep=''),plot=g,width=8,height=6)
            print(paste("File saved as: ",dirIC_i,"/ic_water_ag_2100_State_valPaper.png",sep=''))
    }


    # ic vs. param
    if(T){
    for(x_i in xCompare){
      for(subRegType_i in unique(compareICParamDiff_long$subRegType)){
        for(paramType_i in c("water","ag","electricity","primary")){

        g <- ggplot(dfx %>%
                      dplyr::filter(x==x_i,
                                    grepl("^ic_sec$|^ic_spread$",icType),
                                    grepl(paste("^",paramType_i,"_*",sep=""),paramType),
                                    subRegType==subRegType_i,
                                    !subRegion %in% subRegionsRemove),
                    aes(y=icValue, x=paramValue, color=scenario)) +
          theme_bw() +
          ggtitle(x_i)+
          theme(strip.background = element_rect(colour="white", fill="white"))+
          geom_point(size=0)+ guides(colour = guide_legend(override.aes = list(size=1)))+
          geom_text(aes(label=subRegion),size=3,show.legend = FALSE) +
          scale_color_manual(values=colorsScenarios_i[unique(dfx$scenario)%>%as.character()]) +
          theme(axis.text.x= element_text(angle=90, size=12, vjust=0.25, hjust=1)) +
          theme(axis.text.y= element_text(size=12)) +
          theme(legend.position="bottom")+
          theme(plot.title = element_text(hjust = 0.5),
                axis.title.y = element_text(vjust = 2))+
        facet_grid(paramType~icType,scales= "free_y"); g
        ggsave(filename=paste(dirIC_i,"/ic_",paramType_i,"_",x_i,"_",subRegType_i,"_val.png",sep=''),plot=g,width=8,height=8)
        print(paste("File saved as: ",dirIC_i,"/ic_",paramType_i,"_",x_i,"_",subRegType_i,"_val.png",sep=''))
      }
      }
    }
    }

    # Prcnt Diff ic vs. Prcnt diff param
    if(F){
    for(x_i in xCompare){
      for(subRegType_i in unique(compareICParamDiff_long$subRegType)){
        for(paramType_i in c("water","ag","electricity","primary")){

          g <- ggplot(dfx %>%
                        dplyr::filter(x==x_i,
                                      grepl("diffPrcnt_ic_sec$|^diffPrcnt_ic_spread$",icType),
                                      grepl(paste("^",paramType_i,"_*",sep=""),paramType),
                                      subRegType==subRegType_i,
                                      !subRegion %in% subRegionsRemove),
                      aes(y=icValue, x=paramValue, color=scenario)) +
            theme_bw() +
            ggtitle(x_i)+
            theme(strip.background = element_rect(colour="white", fill="white"))+
            geom_point(size=0)+ guides(colour = guide_legend(override.aes = list(size=1)))+
            geom_text(aes(label=subRegion),size=3,show.legend = FALSE) +
            scale_color_manual(values=colorsScenarios_i[unique(dfx$scenario)%>%as.character()]) +
            theme(axis.text.x= element_text(angle=90, size=12, vjust=0.25, hjust=1)) +
            theme(axis.text.y= element_text(size=12)) +
            theme(legend.position="bottom")+
            theme(plot.title = element_text(hjust = 0.5),
                  axis.title.y = element_text(vjust = 2))+
            facet_grid(paramType~icType,scales= "free_y"); g
          ggsave(filename=paste(dirIC_i,"/ic_",paramType_i,"_",x_i,"_",subRegType_i,"_diffPrcntICvsVal.png",sep=''),plot=g,width=8,height=8)
          print(paste("File saved as: ",dirIC_i,"/ic_",paramType_i,"_",x_i,"_",subRegType_i,"_diffPrcntICvsVal.png",sep=''))
        }
      }
    }
    }

    # Prcnt Diff ic vs. Prcnt diff param
    if(F){
    for(x_i in xCompare){
      for(subRegType_i in unique(compareICParamDiff_long$subRegType)){
        for(paramType_i in c("water","ag","electricity","primary")){

          g <- ggplot(dfx %>%
                        dplyr::filter(x==x_i,
                                      grepl("diffPrcnt_ic_sec$|^diffPrcnt_ic_spread$",icType),
                                      grepl(paste("^diffPrcnt_",paramType_i,"_*",sep=""),paramType),
                                      subRegType==subRegType_i,
                                      !subRegion %in% subRegionsRemove),
                      aes(y=icValue, x=paramValue, color=scenario)) +
            theme_bw() +
            ggtitle(x_i)+
            theme(strip.background = element_rect(colour="white", fill="white"))+
            geom_point(size=0)+ guides(colour = guide_legend(override.aes = list(size=1)))+
            geom_text(aes(label=subRegion),size=3,show.legend = FALSE) +
            scale_color_manual(values=colorsScenarios_i[unique(dfx$scenario)%>%as.character()]) +
            theme(axis.text.x= element_text(angle=90, size=12, vjust=0.25, hjust=1)) +
            theme(axis.text.y= element_text(size=12)) +
            theme(legend.position="bottom")+
            theme(plot.title = element_text(hjust = 0.5),
                  axis.title.y = element_text(vjust = 2))+
            facet_grid(paramType~icType,scales= "free_y"); g
          ggsave(filename=paste(dirIC_i,"/ic_",paramType_i,"_",x_i,"_",subRegType_i,"_diffPrcnt.png",sep=''),plot=g,width=8,height=8)
          print(paste("File saved as: ",dirIC_i,"/ic_",paramType_i,"_",x_i,"_",subRegType_i,"_diffPrcnt.png",sep=''))
        }
      }
    }
    }


  }

}

