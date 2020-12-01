# ---- INFORMATION ----
# R script to process the results of the TIMER Building Stocks & Renovation 
# Author: Vassilis Daioglou
# Reference: Daioglou  et al. (in preparation)
# 
# This script processes and presents the results of a number of scenarios 
# aimed at showing projections of energy use and CO2 emissions of the residential sector
# and the effect of:
# (i) Building insulation and renovation
# (ii) Changing heating technology selection
# (iii) Impact of climate policy (carbon price) on the aboce
# 
# TIMER Scenario runs:
# 1. Baseline (SSP2)
# 2. Baseline, InsulNew (No renovations - but improvements of marginal stock allowed)
# 3. Mitigation (SSP2 - RCP 2.6)
# 4. Mitigation, InsulNew (No renovations - but improvements of marginal stock allowed - Energy Carrier market shares same as baseline)
# 5. Mitigation, InsulAll (All insulation (new + renovation) allowed - Energy Carrier market shares same as baseline)
#
# ---- START ----
# clear memory
rm(list=ls()) 

# Load Libraries
library(reshape2);
library(ggplot2);
library(plyr);
library(dplyr)
library(data.table);
library(tidyr)
library(stringr)
library(xlsx)
library(openxlsx)
library(ggpubr)
library(gridExtra)

# ---- INPUTS: Constants ----
ppi <- 300
FSizeStrip = 9
FSizeAxis = 9
FSizeLeg = 9

Regions = c("BRA","CAN","CEU","CHN","EAF","INDIA","INDO","JAP","KOR",'ME',"MEX","NAF",
            "OCE","RCAM","RSAF","RSAM","RSAS","RUS","SAF","SEAS","STAN","TUR","UKR","USA","WAF","WEU",
            "OECD90","REF","ASIA","MAF","LAM",
            "World")

ActiveYears = c("2010","2020","2030","2040","2050","2060","2070","2080","2090","2100")
ActiveYears2 = c("2030","2050","2075","2100")

ActiveRegion <- "World"
ActiveRegions =c("World","BRA","CHN","USA","WEU")
RCPRegions =c("OECD90","REF","ASIA","MAF","LAM","World")

ActiveDemog =c("U1","U2","U3","U4","U5","R1","R2","R3","R4","R5")
ActiveDemog1 =c("Total","Urban","Rural")

Scenarios = c("SSP2_Baseline","SSP2_InsulNew",
              "SSP2_SPA0_26I_D_Baseline","SSP2_SPA0_26I_D_Full","SSP2_SPA0_26I_D_InsulAll","SSP2_SPA0_26I_D_InsulNew")
ScenBase  =c("SSP2_Baseline","SSP2_SPA0_26I_D_Baseline")
ScenBase2  =c("SSP2_InsulNew","SSP2_Baseline","SSP2_SPA0_26I_D_Baseline")
ScenStand = c("SSP2_Baseline","SSP2_InsulNew","SSP2_SPA0_26I_D_Baseline","SSP2_SPA0_26I_D_InsulAll","SSP2_SPA0_26I_D_InsulNew")
ScenInsul = c("SSP2_Baseline","SSP2_InsulNew","SSP2_SPA0_26I_D_InsulNew","SSP2_SPA0_26I_D_InsulAll","SSP2_SPA0_26I_D_Baseline")

EnergyCarriers = c("Coal","Oil","Gas",
                   "Hydrogen","ModBio","TradBio","SecHeat",
                   "ElecResistance","ElecHeatpump","Elec","ElecPV",
                   "Total")

HeatingCarriers = c("Coal","Oil","Gas",
                   "Hydrogen","ModBio","TradBio","SecHeat",
                   "ElecResistance","ElecHeatpump","ElecPV",
                   "Total")

ECColour=c("black","navyblue","purple",
           "skyblue","forestgreen","brown","bisque",
           "gray","orange","gray","coral1",
           "black")

HeatECColour=c("black","navyblue","purple",
           "skyblue","forestgreen","brown","bisque",
           "gray","goldenrod4","lightsalmon",
           "black")

  # Baseline
data_SSP1 <- "data/BuildStocks/BuildingStocks/SSP1_Baseline.xlsx"
data_SSP2 <- "data/BuildStocks/BuildingStocks/SSP2_Baseline.xlsx"
data_SSP3 <- "data/BuildStocks/BuildingStocks/SSP3_Baseline.xlsx"
data_SSP4 <- "data/BuildStocks/BuildingStocks/SSP4_Baseline.xlsx"
data_SSP5 <- "data/BuildStocks/BuildingStocks/SSP5_Baseline.xlsx"

data_SSP2_norenov <- "data/BuildStocks/BuildingStocks/SSP2_InsulNew.xlsx"

  # Mitigation Scenarios
data_SSP2_mitig <- "data/BuildStocks/BuildingStocks/SSP2_SPA0_26I_D_Baseline.xlsx"

  # Sensitivity Scenarios
data_mitig_InsulAll <- "data/BuildStocks/BuildingStocks/SSP2_SPA0_26I_D_InsulAll.xlsx"
data_mitig_InsulNew <- "data/BuildStocks/BuildingStocks/SSP2_SPA0_26I_D_InsulNew.xlsx"

# set higher RAM capacity for java (used in clsx package)
options(java.parameters = "-Xmx8000m")

# ---- INPUTS: Data ----
  # set directory path 
setwd("C:/Users/Asus/Documents/Github/IMAGE/")
  # Read Data Files for Baseline Scenario
Baseline = rbind(read.xlsx(data_SSP1, sheet = "data"),
                 read.xlsx(data_SSP2, sheet = "data"),
                 read.xlsx(data_SSP2_norenov, sheet = "data"),
                 read.xlsx(data_SSP3, sheet = "data"),
                 read.xlsx(data_SSP4, sheet = "data"),
                 read.xlsx(data_SSP5, sheet = "data"))

Mitigation = read.xlsx(data_SSP2_mitig, sheet = "data")

InsulAll_450 = read.xlsx(data_mitig_InsulAll, sheet = "data")
InsulNew_450 = read.xlsx(data_mitig_InsulNew, sheet = "data")

rm(data_SSP1,data_SSP2,data_SSP3,data_SSP4,data_SSP5,
   data_SSP2_norenov,
   data_SSP2_mitig,
   data_mitig_InsulAll,data_mitig_InsulNew)
#
# ---- MUNGING ----
# Create Single Dataset
Baseline = melt(Baseline, id.vars=c("Model","Scenario","Region","Variable","Unit"))
Mitigation = melt(Mitigation, id.vars=c("Model","Scenario","Region","Variable","Unit"))
InsulAll_450 = melt(InsulAll_450, id.vars=c("Model","Scenario","Region","Variable","Unit"))
InsulNew_450 = melt(InsulNew_450, id.vars=c("Model","Scenario","Region","Variable","Unit"))

DATA = rbind(Baseline,
             Mitigation,
             InsulAll_450,InsulNew_450)

DATA$Model <- NULL
colnames(DATA)[5] <- "Year"

rm(Baseline,Mitigation,
   InsulAll_450,InsulNew_450)
# Rename Variables
DATA$Variable <- gsub("[[:punct:]]","",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Residential","Res",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Final Energy","FE",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Useful Energy","UE",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Carbon Content","CC",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Electricity","Elec",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Heating","Heat",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Cooling","Cool",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Secondary Heat","SecHeat",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Modern Biomass","ModBio",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Traditional Biomass","TradBio",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Insulation","Insul",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Investments","Inv",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Intensity","Int",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Emissions","Emis",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Renovation","Renov",DATA$Variable,fixed=F)
DATA$Variable <- gsub("and","",DATA$Variable,fixed=F)
DATA$Variable <- gsub("per capita","pc",DATA$Variable,fixed=F)
DATA$Variable <- gsub("per floorspace","pfs",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Population","Pop",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Independence","Indep",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Age Cohort","AgeCoh",DATA$Variable,fixed=F)
DATA$Variable <- gsub("10 years or less","<10",DATA$Variable,fixed=F)
DATA$Variable <- gsub("51 years or more",">51",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Cost Component","CostFrac",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Capital","Cap",DATA$Variable,fixed=F)
DATA$Variable <- gsub("ElecRes","",DATA$Variable,fixed=F)
DATA$Variable <- gsub("ElecSolarRes","",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Fixed","",DATA$Variable,fixed=F)
DATA$Variable <- gsub("[[:space:]]","",DATA$Variable,fixed=F)

DATA$Year = as.numeric(substr(DATA$Year, start=1, stop=4))
DATA$value = as.numeric(as.character(DATA$value))

DATA$ScenOrder = factor(DATA$Scenario, levels =c("SSP1_Baseline","SSP2_Baseline","SSP3_Baseline","SSP4_Baseline","SSP5_Baseline",
                                                 "SSP2_InsulNew",
                                                 "SSP2_SPA0_26I_D_InsulNew","SSP2_SPA0_26I_D_InsulAll","SSP2_SPA0_26I_D_Baseline"))
#
# ---- RCP REGION DEFINITON ----
get.RCP5 <- function(dataframe,spread_var){
  dataframe = dataframe %>% mutate(OECD90=CAN+JAP+OCE+TUR+USA+WEU)
  dataframe = dataframe %>% mutate(REF=CEU+RUS+STAN+UKR)
  dataframe = dataframe %>% mutate(ASIA=CHN+INDIA+INDO+KOR+RSAS+SEAS)
  dataframe = dataframe %>% mutate(MAF=EAF+ME+NAF+RSAF+SAF+WAF)
  dataframe = dataframe %>% mutate(LAM=BRA+MEX+RCAM+RSAM)
  dataframe = melt(dataframe,id.vars=c("Scenario","Variable","Unit","Year","ScenOrder"), na.rm=FALSE )
  colnames(dataframe)[colnames(dataframe)=="variable"]<-"Region"
  dataframe
}
  # First determing wieghting factors population & floorspace
Weights = subset(DATA, Variable=="Pop"|Variable=="FloorspaceTotal")
Weights = spread(Weights,Region,value)
Weights = get.RCP5(Weights)
Weights$Unit <- NULL
Weights = spread(Weights,Variable,value)
# colnames(Weights)[colnames(Weights)=="variable"]<-"Region"
Weights$ID = paste(Weights$Scenario,Weights$Region,Weights$Year)
  
  # Identify variables whose regions can be summed (i.e. absolute measures)
DATA.R1 = subset(DATA, Unit=="kgCO2/yr"|Unit=="GJ"|Unit=="m2"|Unit=="Bn$")
DATA.R1 = spread(DATA.R1,Region,value)
DATA.R1 = get.RCP5(DATA.R1)

  # Identify Variables whose RCP regional data will be POPULATION weighted 
DATA.R2 = subset(DATA, Variable=="CCElec"|Variable=="CCHeat"|Variable=="EmisCO2HeatCoolpc"|Variable=="UEHeatCoolpc")
DATA.R2$ID = paste(DATA.R2$Scenario,DATA.R2$Region,DATA.R2$Year)
DATA.R2$Pop <- Weights[match(DATA.R2$ID, Weights$ID),"Pop"] 
DATA.R2 = DATA.R2 %>% mutate(PopWeight = value * Pop)
DATA.R2 = subset(DATA.R2, select=-c(value,Pop,ID))
DATA.R2 = spread(DATA.R2,Region,PopWeight)
DATA.R2 = get.RCP5(DATA.R2)
DATA.R2$ID = paste(DATA.R2$Scenario,DATA.R2$Region,DATA.R2$Year)
DATA.R2$Pop <- Weights[match(DATA.R2$ID, Weights$ID),"Pop"] 
DATA.R2 = DATA.R2 %>% mutate(valueCor = value / Pop)
DATA.R2 = subset(DATA.R2, select = -c(value,ID,Pop))
colnames(DATA.R2)[colnames(DATA.R2)=="valueCor"] <- "value"
# 
  # Identify Variables whose RCP regional data will be FLOORSPACE weighted 
DATA.R3 = subset(DATA, (Unit=="$/m2"|Unit=="W/m^2/K"|Unit=="US$2010/kW/yr"|Unit=="US$2010/kW"|Unit=="US$/kWhe") | 
                   (Variable=="InsulAverageRenovRate"|Variable=="UEHeatCoolpfs"|Variable=="UEIntHeat"|
                   Variable=="FloorspaceAgeCohFrac<10"|Variable=="FloorspaceAgeCohFrac11to20years"|Variable=="FloorspaceAgeCohFrac21to30years"|
                   Variable=="FloorspaceAgeCohFrac31to40years"|Variable=="FloorspaceAgeCohFrac41to50years"|Variable=="FloorspaceAgeCohFrac>51"|
                   Variable=="FEResIndepTotal"|Variable=="FEResIndepUrban"|Variable=="FEResIndepRural"|
                   Variable=="FEResIndepU1"|Variable=="FEResIndepU2"|Variable=="FEResIndepU3"|Variable=="FEResIndepU4"|Variable=="FEResIndepU5"|
                   Variable=="FEResIndepR1"|Variable=="FEResIndepR2"|Variable=="FEResIndepR3"|Variable=="FEResIndepR4"|Variable=="FEResIndepR5"))
DATA.R3$ID = paste(DATA.R3$Scenario,DATA.R3$Region,DATA.R3$Year)
DATA.R3$FS <- Weights[match(DATA.R3$ID, Weights$ID),"FloorspaceTotal"] 
DATA.R3 = DATA.R3 %>% mutate(FSWeight = value * FS)
DATA.R3 = subset(DATA.R3, select=-c(value,FS,ID))
DATA.R3 = spread(DATA.R3,Region,FSWeight)
DATA.R3 = get.RCP5(DATA.R3)
DATA.R3$ID = paste(DATA.R3$Scenario,DATA.R3$Region,DATA.R3$Year)
DATA.R3$FS <- Weights[match(DATA.R3$ID, Weights$ID),"FloorspaceTotal"] 
DATA.R3 = DATA.R3 %>% mutate(valueCor = value / FS)
DATA.R3 = subset(DATA.R3, select = -c(value,ID,FS))
colnames(DATA.R3)[colnames(DATA.R3)=="valueCor"] <- "value"

#
  # Final Data Set
DATA1 = rbind(DATA.R1,DATA.R2,DATA.R3)
rm(DATA, DATA.R1, DATA.R2, DATA.R3, Weights)
# SEPARATE DATASETS
# ---- ***Final Energy (FE)*** ----
DATA.FE <- subset(DATA1, Variable=="FECoolElec"|Variable=="FEHeatCoal"|Variable=="FEHeatElecResistance"|Variable=="FEHeatElecHeatpump"|Variable=="FEHeatGas"|Variable=="FEHeatHydrogen"
                 |Variable=="FEHeatModBio"|Variable=="FEHeatOil"|Variable=="FEHeatSecHeat"|Variable=="FEHeatTradBio")
DATA.FE$Prim <- DATA.FE$Variable
DATA.FE$Prim <- gsub("FECool","",DATA.FE$Prim,fixed=F)
DATA.FE$Prim <- gsub("FEHeat","",DATA.FE$Prim,fixed=F)
DATA.FE$Variable = substr(DATA.FE$Variable, start=1, stop=6)
DATA.FE <- DATA.FE[c("Scenario","Region","Year","Variable","Prim","Unit","value","ScenOrder")]

DATA.FE <- spread(DATA.FE,Prim,value)
DATA.FE[is.na(DATA.FE)]<- 0.0
DATA.FE = DATA.FE %>% mutate(Total = Coal + Elec + ElecResistance + ElecHeatpump + Gas + Hydrogen + ModBio + Oil + SecHeat + TradBio)
DATA.FE = melt(DATA.FE, id.vars=c("Scenario","Region","Year","Variable","Unit","ScenOrder"))
colnames(DATA.FE)[7] <- "Prim"
DATA.FE = subset(DATA.FE, !(Variable=="FECool"&(Prim=="Coal"|Prim=="ElecHeatpump"|Prim=="ElecResistance"|Prim=="Gas"|Prim=="Hydrogen"
                                                |Prim=="ModBio"|Prim=="Oil"|Prim=="SecHeat"|Prim=="TradBio"
                                                  |Prim=="Total")))
temp = subset(DATA.FE, Variable=="FECool"|(Variable=="FEHeat"&Prim=="Total"))
temp$Prim <- "Total"
temp = spread(temp,Variable,value)
temp = temp %>% mutate(FECoolHeat = FECool + FEHeat)
temp = melt(temp, id.vars=c("Scenario","Region","Year","Unit","Prim","ScenOrder"))
colnames(temp)[7] <- "Variable"
  
DATA.FE = rbind(DATA.FE,temp)
rm(temp)

DATA.FE$PrimOrder  =factor(DATA.FE$Prim, levels = EnergyCarriers)

# ---- *** Rooftop PV (PV)*** ----
DATA.PV <- subset(DATA1, Variable=="FEResExportElec"|Variable=="FEResGenerationElec"|Variable=="FEResNetElec"|Variable=="FEResElecPVHeatCool")
DATA.PV$Prim <- "ElecPV"
DATA.PV$PrimOrder  =factor(DATA.PV$Prim, levels = EnergyCarriers)
# ---- *** Rooftop PV Cost (PVC)*** ----
DATA.PVC <- subset(DATA1, Variable=="CapCostPV"|Variable=="OMCostPV")
DATA.PVC$Unit <- "$2010/kWe"
DATA.PVC = spread(DATA.PVC,Variable,value)
DATA.PVC = DATA.PVC %>% mutate(TotalInvPV = CapCostPV + OMCostPV)
DATA.PVC = melt(DATA.PVC, id.vars=c("Scenario","Unit","Year","ScenOrder","Region"))
colnames(DATA.PVC)[6] <- "Variable"

DATA.PVC = rbind(DATA.PVC,subset(DATA1, Unit=="US$/kWhe"))


DATA.PVC$Demographic <- substr(DATA.PVC$Variable, start=15, stop=20)
DATA.PVC$Demographic[DATA.PVC$Demographic == ""] <- "Total"
DATA.PVC$DemogOrder = factor(DATA.PVC$Demographic, levels =c("Total","Urban","Rural",
                                                           "U1","U2","U3","U4","U5",
                                                           "R1","R2","R3","R4","R5"))
DATA.PVC$DemogQ = substr(DATA.PVC$DemogOrder, start=2, stop=2)
DATA.PVC$DemogQ = as.numeric(DATA.PVC$DemogQ)
DATA.PVC$DemogTUR = substr(DATA.PVC$DemogOrder, start=1, stop=1)

# ---- ***Carbon Contents (CC)*** ----
DATA.CC <- subset(DATA1, Variable=="CCElec"|Variable=="CCHeat")

# ---- ***Floorspace (FS)*** ----
DATA.FS <- subset(DATA1, Variable=="InsulFloorspaceLevel1"|Variable=="InsulFloorspaceLevel2"|Variable=="InsulFloorspaceLevel3"|
                    Variable=="InsulFloorspaceLevel4"|Variable=="InsulFloorspaceLevel5"|Variable=="InsulFloorspaceLevel6")
DATA.FS$InsulLevel <- DATA.FS$Variable
DATA.FS$InsulLevel <- gsub("InsulFloorspaceLevel","",DATA.FS$InsulLevel,fixed=F)
DATA.FS$Variable = substr(DATA.FS$Variable, start=6, stop=15)
DATA.FS <- DATA.FS[c("Scenario","Region","Year","Variable","InsulLevel","Unit","value","ScenOrder")]

# ---- ***Age Cohorts (AGE)*** ----
DATA.AGE <- subset(DATA1, Variable=="FloorspaceAgeCohFrac<10"|Variable=="FloorspaceAgeCohFrac11to20years"|Variable=="FloorspaceAgeCohFrac21to30years"|
                     Variable=="FloorspaceAgeCohFrac31to40years"|Variable=="FloorspaceAgeCohFrac41to50years"|Variable=="FloorspaceAgeCohFrac>51"|
                     Variable=="FloorspaceAgeCoh<10"|Variable=="FloorspaceAgeCoh11to20years"|Variable=="FloorspaceAgeCoh21to30years"|
                     Variable=="FloorspaceAgeCoh31to40years"|Variable=="FloorspaceAgeCoh41to50years"|Variable=="FloorspaceAgeCoh>51")
DATA.AGE$AgeCohort <- DATA.AGE$Variable
DATA.AGE$AgeCohort <- gsub("FloorspaceAgeCoh","",DATA.AGE$AgeCohort,fixed=F)
DATA.AGE$AgeCohort <- gsub("Frac","",DATA.AGE$AgeCohort,fixed=F)

DATA.AGE1 = subset(DATA.AGE, Variable=="FloorspaceAgeCohFrac<10"|Variable=="FloorspaceAgeCohFrac11to20years"|Variable=="FloorspaceAgeCohFrac21to30years"|
                     Variable=="FloorspaceAgeCohFrac31to40years"|Variable=="FloorspaceAgeCohFrac41to50years"|Variable=="FloorspaceAgeCohFrac>51")
DATA.AGE1$Variable <- "FloorspaceFrac"
DATA.AGE2 = subset(DATA.AGE, Variable=="FloorspaceAgeCoh<10"|Variable=="FloorspaceAgeCoh11to20years"|Variable=="FloorspaceAgeCoh21to30years"|
                     Variable=="FloorspaceAgeCoh31to40years"|Variable=="FloorspaceAgeCoh41to50years"|Variable=="FloorspaceAgeCoh>51")
DATA.AGE2$Variable <- "Floorspace"

DATA.AGE = rbind(DATA.AGE1,DATA.AGE2)

DATA.AGE <- DATA.AGE[c("Scenario","Region","Year","Variable","AgeCohort","Unit","value","ScenOrder")]
DATA.AGE$AgeCohortOrder = factor(DATA.AGE$AgeCohort, levels=c("<10","11to20years","21to30years","31to40years","41to50years",">51"))
rm(DATA.AGE1,DATA.AGE2)
# ---- ***Cost Components (COST) ***
DATA.COST <- subset(DATA1, Unit == "$/m2")
DATA.COST$InsulLevel <- DATA.COST$Variable
DATA.COST$InsulLevel <- gsub("InsulCostFracLevel","",DATA.COST$InsulLevel,fixed=F)
DATA.COST$InsulLevel <- gsub("Cap","",DATA.COST$InsulLevel,fixed=F)
DATA.COST$InsulLevel <- gsub("Heat","",DATA.COST$InsulLevel,fixed=F)
DATA.COST$InsulLevel <- gsub("Cool","",DATA.COST$InsulLevel,fixed=F)
DATA.COST$InsulLevel <- gsub("Total","",DATA.COST$InsulLevel,fixed=F)
DATA.COST$Variable = substr(DATA.COST$Variable, start=20, stop=25)
DATA.COST <- DATA.COST[c("Scenario","Region","Year","Variable","InsulLevel","Unit","value","ScenOrder")]

# ---- ***Investments (INV)*** ----
DATA.INV <- subset(DATA1, Variable=="InvInsulRenov"|Variable=="InvInsulTotal")

# ---- ***Useful Energy (UE)*** ----
DATA.UE <- subset(DATA1, Variable=="UEHeatCoolpc"|Variable=="UEHeatCoolpfs"|Variable=="UEHeatCool"|Variable=="UEIntHeat")
  # Normalise to 2020 value
DATA.UE$ID = paste(DATA.UE$Scenario,DATA.UE$Region,DATA.UE$Variable)
DATA.UE2020 = subset(DATA.UE, Year==2020)
DATA.UE$val_2020 <- DATA.UE2020[match(DATA.UE$ID, DATA.UE2020$ID),"value"]
rm(DATA.UE2020)
DATA.UE = DATA.UE %>% mutate(Normalised_2020 = value/val_2020)
DATA.UE$ID <- NULL
DATA.UE$val_2020 <- NULL

#
# ---- ***UValue Levels (UVL) *** ----
DATA.UVL <- subset(DATA1, Variable=="UvalueLevel1"|Variable=="UvalueLevel2"|Variable=="UvalueLevel3"|
                    Variable=="UvalueLevel4"|Variable=="UvalueLevel5"|Variable=="UvalueLevel6")
DATA.UVL$InsulLevel <- DATA.UVL$Variable
DATA.UVL$InsulLevel <- gsub("UvalueLevel","",DATA.UVL$InsulLevel,fixed=F)
DATA.UVL$Variable = substr(DATA.UVL$Variable, start=1, stop=6)
DATA.UVL <- DATA.UVL[c("Scenario","Region","Year","Variable","InsulLevel","Unit","value","ScenOrder")]

#
# ---- ***UValues (UV)*** ----
DATA.UV <- subset(DATA1, Variable=="UEUvalue")

# ---- ***Emissions (EM)*** ----
DATA.EM <- subset(DATA1, Variable=="EmisCO2DirectHeatCool"|Variable=="EmisCO2HeatCool"|Variable=="EmisCO2HeatCoolpc")

# ---- ***Renovation Rate (RR)*** ----
DATA.RR <- subset(DATA1, Variable=="InsulAverageRenovRate")

# ---- ***Energy Independence (ID)*** ----
DATA.ID <- subset(DATA1,      Variable=="FEResIndepTotal"|Variable=="FEResIndepUrban"|Variable=="FEResIndepRural"|
                              Variable=="FEResIndepU1"|Variable=="FEResIndepU2"|Variable=="FEResIndepU3"|Variable=="FEResIndepU4"|Variable=="FEResIndepU5"|
                              Variable=="FEResIndepR1"|Variable=="FEResIndepR2"|Variable=="FEResIndepR3"|Variable=="FEResIndepR4"|Variable=="FEResIndepR5")

DATA.ID$Demographic <- substr(DATA.ID$Variable, start=11, stop=20)

DATA.ID$DemogOrder = factor(DATA.ID$Demographic, levels =c("Total","Urban","Rural",
                                                 "U1","U2","U3","U4","U5",
                                                 "R1","R2","R3","R4","R5"))
DATA.ID$DemogQ = substr(DATA.ID$DemogOrder, start=2, stop=2)
DATA.ID$DemogQ = as.numeric(DATA.ID$DemogQ)
DATA.ID$DemogTUR = substr(DATA.ID$DemogOrder, start=1, stop=1)

#
# ---- DATASET FOR TABLEv1 ----
DATA.T1 = rbind(
  subset(DATA.INV, Scenario %in% ScenBase & Region %in% RCPRegions & Year %in% ActiveYears2),
  subset(DATA.RR, Scenario %in% ScenBase & Region %in% RCPRegions & Year %in% ActiveYears2))
DATA.T1a = subset(DATA.T1, Variable=="InvInsulRenov")
DATA.T1a = spread(DATA.T1a, Year, value)

DATA.T1b = subset(DATA.T1, Variable=="InvInsulTotal")
DATA.T1b = spread(DATA.T1b, Year, value)

DATA.T1c = subset(DATA.T1, Variable=="InsulAverageRenovRate")
DATA.T1c = spread(DATA.T1c, Year, value)

DATA.T1 = spread(DATA.T1, Year, value)

# 
# ---- DATASETS FOR FIGURES ----
  # Figure 1
DATA.FIG1 = subset(DATA.FS, Scenario %in% ScenBase & Region %in% RCPRegions & Year %in% ActiveYears)

DATA.FIG1a = rbind(subset(DATA1, Variable == "FloorspaceTotal" & Scenario == "SSP2_Baseline" & Region %in% RCPRegions & Year %in% ActiveYears),
                   subset(DATA.UV, Scenario %in% ScenBase & Region %in% RCPRegions & Year %in% ActiveYears))

  # Figure 2
DATA.FIG2 = rbind(DATA.FE,DATA.PV)
DATA.FIG2 = subset(DATA.FIG2, Scenario %in% ScenBase & Year %in% ActiveYears & Region %in% RCPRegions)
DATA.FIG2 = subset(DATA.FIG2, Variable=="FEHeat"|Variable=="FEResElecPVHeatCool"|Variable=="FECool")
DATA.FIG2$value[DATA.FIG2$Variable=="FEResElecPVHeatCool"] <- -1 * DATA.FIG2$value[DATA.FIG2$Variable=="FEResElecPVHeatCool"]
#  Remove duplicates
DATA.FIG2<-DATA.FIG2[!duplicated(DATA.FIG2),]
  #  Figure 3
temp = subset(DATA.FE, Scenario %in% ScenInsul & Variable=="FECoolHeat" & Year %in% ActiveYears & Region %in% RCPRegions & Prim=="Total")
temp = subset(temp, select = -c(Scenario,Unit,Prim,PrimOrder))

temp1 = subset(DATA.EM, Scenario %in% ScenInsul & Variable=="EmisCO2DirectHeatCool" & Year %in% ActiveYears & Region %in% RCPRegions)
temp1 = subset(temp1, select = -c(Scenario,Unit))

temp = rbind(temp,temp1)
temp$ID = paste(temp$Region, temp$Variable, temp$Year)

# Separate datasets for results of each decomposition component
temp.BaseNoRenov = subset(temp, ScenOrder == "SSP2_InsulNew")
temp.Base = subset(temp, ScenOrder == "SSP2_Baseline")
temp.EffNew = subset(temp, ScenOrder == "SSP2_SPA0_26I_D_InsulNew")
temp.EffNewRen = subset(temp, ScenOrder == "SSP2_SPA0_26I_D_InsulAll")
temp.Tot = subset(temp, ScenOrder == "SSP2_SPA0_26I_D_Baseline")

# Complete dataset
decompose <- function(dataframe1,dataframe2){
  colnames(dataframe1)[colnames(dataframe1)=="value"] <- "max"
  dataframe1$min <- dataframe2[match(dataframe1$ID,dataframe2$ID),"value"]
  dataframe1
}

Area.Base = decompose(temp.BaseNoRenov,temp.Base)
Area.EffNew = decompose(temp.Base, temp.EffNew)
Area.EffNewRen = decompose(temp.EffNew, temp.EffNewRen)
Area.EffNewRenFuel = decompose(temp.EffNewRen,temp.Tot)
                          
DATA.FIG3 = rbind(Area.Base,Area.EffNew,Area.EffNewRen,Area.EffNewRenFuel)

#  Add total emissions/energy for each scenario
temp$ID = paste(temp$Region, temp$ScenOrder, temp$Variable, temp$Year)
DATA.FIG3$ID = paste(DATA.FIG3$Region, DATA.FIG3$ScenOrder, DATA.FIG3$Variable, DATA.FIG3$Year)

DATA.FIG3$value <- temp[match(DATA.FIG3$ID,temp$ID),"value"]

temp$min <- NA
temp$max <- NA

#  Add emission and energy projection for the mitigation scenarios
DATA.FIG3 = rbind(DATA.FIG3,subset(temp, ScenOrder == "SSP2_SPA0_26I_D_Baseline"))

DATA.FIG3$ID <- NULL
DATA.FIG3$Variable = factor(DATA.FIG3$Variable, levels=c("FECoolHeat","EmisCO2DirectHeatCool"))
rm(temp,temp1,temp.BaseNoRenov,temp.Base,temp.EffNew,temp.EffNewRen,temp.Tot,
   Area.EffNew,Area.EffNewRen,Area.EffNewRenFuel)

# 
# ---- BASLINE AND MITIGATION COMPARISON ----
Compare = subset(DATA.FE, Scenario %in% ScenBase & Region %in% RCPRegions & Year %in% ActiveYears)
Compare = subset(Compare, select=-c(ScenOrder,PrimOrder))
Compare =Compare[!duplicated(Compare),]

  # Comparison between Baseline and Mitigation
Compare.Scen = spread(Compare, Scenario, value)
Compare.Scen = Compare.Scen %>% mutate(MitigChange_Perc = (SSP2_SPA0_26I_D_Baseline - SSP2_Baseline)/SSP2_Baseline * 100)
Compare.Scen = subset(Compare.Scen, Region %in% RCPRegions & Year %in% ActiveYears2 & Prim == "Total" & Variable != "FECoolHeat")

  # Comparison between 2020 and Mitigation
Compare.Time = subset(Compare, (Year=="2020"|Year=="2050"|Year=="2100") & Scenario == "SSP2_SPA0_26I_D_Baseline")
Compare.Time = spread(Compare.Time, Year, value)
colnames(Compare.Time)[6:8] <- c("x2020","x2050","x2100")
Compare.Time = Compare.Time %>% mutate(PercChang_20_50 = (x2050 - x2020)/x2020 * 100)
Compare.Time = Compare.Time %>% mutate(PercChang_20_100 = (x2100 - x2020)/x2020 * 100)
Compare.Time = subset(Compare.Time, Region %in% RCPRegions & Prim == "Total" & Variable != "FECoolHeat")

# ---- DATASET FOR SUPPLEMENTARY DATA ----
ScenarioDF <- data.frame(Scenario = c("SSP1_Baseline",
                                         "SSP2_Baseline",
                                         "SSP2_InsulNew",
                                         "SSP3_Baseline",
                                         "SSP4_Baseline",
                                         "SSP5_Baseline",
                                         "SSP2_SPA0_26I_D_Baseline",
                                         "SSP2_SPA0_26I_D_InsulAll",
                                         "SSP2_SPA0_26I_D_InsulNew"),
                           ScenLabel = c("SSP1",
                                          "SSP2",
                                          "SSP2 NewIsul",
                                          "SSP3",
                                          "SSP4",
                                          "SSP5",
                                          "SSP2 - 2°C",
                                          "SSP2 - 2°C - AllInsul",
                                          "SSP2 - 2°C - NewInsul"))

SupData.FE = rbind(DATA.FE, DATA.PV)
SupData.FE = subset(SupData.FE, Variable=="FEHeat"|Variable=="FECool"|Variable=="FEResGenerationElec")
SupData.FE = subset(SupData.FE, select = -c(Scenario,Prim))
SupData.FE = subset(SupData.FE, !(Variable=="FEHeat" & PrimOrder=="Elec"))
SupData.FE$ScenName = ScenarioDF[match(SupData.FE$ScenOrder,ScenarioDF$Scenario),"ScenLabel"]
SupData.FE$ScenOrder <- NULL
SupData.FE = SupData.FE[,c(7,1,2,3,6,4,5)]
colnames(SupData.FE)[] <- c("Scenario","Region","Year","Variable","Energy Carrier","Unit","Value")
SupData.FE$Variable = gsub("FEHeat","Heating",SupData.FE$Variable)
SupData.FE$Variable = gsub("FECool","Cooling",SupData.FE$Variable)
SupData.FE$Variable = gsub("FEResGenerationElec","Rooftop PV Generation",SupData.FE$Variable)
SupData.FE =SupData.FE[!duplicated(SupData.FE),]

SupData.FS = DATA.FS
SupData.FS = subset(SupData.FS, select = -c(Scenario))
SupData.FS$ScenName = ScenarioDF[match(SupData.FS$ScenOrder,ScenarioDF$Scenario),"ScenLabel"]
SupData.FS$ScenOrder <- NULL
SupData.FS = SupData.FS[,c(7,1,2,3,4,5,6)]
colnames(SupData.FS)[] <- c("Scenario","Region","Year","Variable","Insulation Level","Unit","Value")
SupData.FS =SupData.FS[!duplicated(SupData.FS),]

SupData.UE = subset(DATA.UE, Variable == "UEHeatCool")
SupData.UE = subset(SupData.UE, select = -c(Scenario, Normalised_2020))
SupData.UE = rbind(SupData.UE, subset(DATA.UV, select = -c(Scenario)))
SupData.UE$ScenName = ScenarioDF[match(SupData.UE$ScenOrder,ScenarioDF$Scenario),"ScenLabel"]
SupData.UE$ScenOrder <- NULL
SupData.UE = SupData.UE[,c(6,4,3,1,2,5)]
colnames(SupData.UE)[] <- c("Scenario","Region","Year","Variable","Unit","Value")
SupData.UE$Variable = gsub("UEHeatCool","Heating & Cooling Useful Energy Demand",SupData.UE$Variable)
SupData.UE$Variable = gsub("UEUvalue","Aggregate Residential U-Value",SupData.UE$Variable)
SupData.UE =SupData.UE[!duplicated(SupData.UE),]

SupData.EM = subset(DATA.EM, Variable == "EmisCO2DirectHeatCool"|Variable=="EmisCO2HeatCool")
SupData.EM = subset(SupData.EM, select = -c(Scenario))
SupData.EM$ScenName = ScenarioDF[match(SupData.EM$ScenOrder,ScenarioDF$Scenario),"ScenLabel"]
SupData.EM$ScenOrder <- NULL
SupData.EM = SupData.EM[,c(6,4,3,1,2,5)]
colnames(SupData.EM)[] <- c("Scenario","Region","Year","Variable","Unit","Value")
SupData.EM$Variable = gsub("EmisCO2DirectHeatCool","Heating & Cooling Direct Emissions",SupData.EM$Variable)
SupData.EM$Variable = gsub("EmisCO2HeatCool","Heating & Cooling Direct & Indirect Emissions",SupData.EM$Variable)
SupData.EM =SupData.EM[!duplicated(SupData.EM),]

SupData.RR = subset(DATA.RR, Year %in% ActiveYears2)
SupData.RR = subset(SupData.RR, select = -c(Scenario))
SupData.RR$ScenName = ScenarioDF[match(SupData.RR$ScenOrder,ScenarioDF$Scenario),"ScenLabel"]
SupData.RR$ScenOrder <- NULL
SupData.RR = SupData.RR[,c(6,4,3,1,2,5)]
colnames(SupData.RR)[] <- c("Scenario","Region","Year","Variable","Unit","Value")
SupData.RR$Variable = gsub("InsulAverageRenovRate","Average Renovation rate since 2020",SupData.RR$Variable)
SupData.RR =SupData.RR[!duplicated(SupData.RR),]

#
# ---- LABELS ----
scen_labels <-c("SSP2_Baseline"="Baseline",
                "SSP2_InsulNew"="Baseline - No Renovations",
                "SSP2_SPA0_26I_D_Baseline"="2°C",
                "SSP2_SPA0_26I_D_Full"="Full \n2°C",
                "SSP2_SPA0_26I_D_Demand"="Demand \n2°C",
                "SSP2_SPA0_26I_D_Floorspace"="Floorspace \n2°C",
                "SSP2_SPA0_26I_D_InsulAll"="New and Renovations \n2°C",
                "SSP2_SPA0_26I_D_InsulNew"="New Building \nInsulation - 2°C")

scen_labels2 <-c("SSP2_Baseline"="Baseline",
                 "SSP2_InsulNew"="Baseline - No Renovations",
                 "SSP2_SPA0_26I_D_Baseline"="2°C",
                "SSP2_SPA0_26I_D_InsulAll"="New and Renovations \n2°C",
                "SSP2_SPA0_26I_D_InsulNew"="New Building \nInsulation - 2°C")

scen_labels3 <-c("SSP2_Baseline"="Effect of Insulation \nin New Buildings",
                 "SSP2_InsulNew"="Baseline - No Renovations",
                 "SSP2_SPA0_26I_D_InsulNew"="Effect of Retrooofits",
                 "SSP2_SPA0_26I_D_InsulAll"="Effect of heating & /n cooling technology choices",
                 "SSP2_SPA0_26I_D_Baseline"="Total")


reg_labels <-c("BRA"="Brazil","CAN"="Canada","CEU"="Central Europe","CHN"="China+","EAF"="Eastern Africa",
               "INDIA"="India","INDO"="Indonesia","JAP"="Japan","KOR"="Korean Penunsila","ME"="Middle East",
               "MEX"="Mexico","NAF"="Northern Africa","OCE"="Oceania","RCAM"="Rest of Central America","RSAF"="Rest of Southern Africa",
               "RSAM"="Rest of Southern America","RSAS"="Rest of Southern Asia","RUS"="Russia","SAF"="South Africa","SEAS"="Southeast Asia",
               "STAN"="Kazakhstan +","TUR"="Turkey","UKR"="Ukraine","USA"="USA","WAF"="Western Africa","WEU"="Western Europe",
               "OECD90"="OECD","REF"="Reforming \nEconomies","ASIA"="Asia","MAF"="Middle East \n& Africa","LAM"="Latin \nAmerica",
               "World"="Global")

var_labels <-c("RenovationRate"="Renovation \nRate \n(%)",
               "UeIntHeat"="Heating & Cooling \nIntensity (desired) \n(2010=1)",
               "HeatCoolDemand_pc"="Heating & Cooling \nDemand \n(2010=1)",
               "ResiCO2EmisHeatCool"="Heating & Cooling \nEmissions \n(2010=1)",
               "FECoolHeat"="Secondary Energy \nHeating & Cooling",
               "EmisCO2HeatCool"="Emissions\nHeating and Cooling",
               "EmisCO2DirectHeatCool"="Emissions\nHeating and Cooling")

turq_labels <-c("1"="Total","2"="Urban","3"="Rural")

cc_labels <-c("CCElec"="Electricity","CCHeat"="Heating Fuels")

prim_labels <-c("Coal"="Coal",
                "Oil"="Oil",
                "Gas"="Nat. Gas",
                "Hydrogen"="Hydrogen",
                "ModBio"="Modern Biomass",
                "TradBio"="Traditionl Biomass",
                "SecHeat"="District Heat",
                "ElecResistance"="Resistance Heater",
                "ElecHeatpump"="Heat Pump",
                "Elec"="Cooling Electricity",
                "ElecPV"="Rooftop Photovoltaic \n(generation)",
                "Total"="Total")

primheat_labels <- prim_labels[-10] 

# ---- FIGURES ----
# ----- Figure 1: Stocks ----
left_axis1 = expression(paste("Floorspace [bill m"^"2","]"))
right_axis1 = expression(paste("Aggregate U-Value of building envelope[W/m"^"2","/K]"))
axis_scale1 = 50

StckUV.BMR <- ggplot() + 
  geom_line(data=subset(DATA.FIG1a, Variable=="FloorspaceTotal"),
            aes(x=Year,y = value/1e9, color=Variable), size=1, alpha=1) +
  geom_point(data=subset(DATA.FIG1a, Variable=="UEUvalue"),
            aes(x=Year,y = value * axis_scale1, shape=ScenOrder),size=1.0, alpha=0.66) +
  scale_y_continuous(name = left_axis1, 
                     sec.axis = sec_axis(~. * 1/axis_scale1, name = right_axis1))+
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_line(colour="gray80", size = 0.3)) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.box="vertical", legend.direction = "horizontal", legend.spacing.y=unit(0.01,"cm")) +
  scale_colour_manual(values="black",name="Floorspace (Left axis)",breaks="FloorspaceTotal",labels="", guide="legend") +
  scale_shape_manual(values=c(1,8),
                    name="U-Value (Right axis)",
                    breaks=c("SSP2_Baseline","SSP2_SPA0_26I_D_Baseline"),
                    labels=c("Baseline","2°C")) +
  facet_wrap(Region~., nrow=2, scales="free_y", labeller=labeller(Region=reg_labels, ScenOrder=scen_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
StckUV.BMR
#
# ---- Figure 2: Fuels & Emissions ----
axis_scale2 = 8
left_axis2 = "Final Energy [EJ/yr]"
right_axis2 = expression(paste("Heating & Cooling Emissions", "[Gt",CO[2],"/yr]",""))

FuelsEmis.BMR <- ggplot() + 
  geom_bar(data=subset(DATA.FIG2, !(Prim=="Elec"|Prim=="Total") & Region %in% RCPRegions), 
           aes(x=Year,y = value/1e9, fill=PrimOrder),alpha=0.7, stat="identity") +
  geom_line(data=subset(DATA.FIG2, Variable=="FECool" & Region %in% RCPRegions),
            aes(x=Year,y = value/1e9, color="CoolingElec"),size=1, alpha=1, linetype="dashed") +
  geom_point(data=subset(DATA.EM, Scenario %in% ScenBase& Year %in% ActiveYears & Region %in% RCPRegions & Variable=="EmisCO2DirectHeatCool")
             , aes(x=Year,y = (value/1e12) * axis_scale2, colour="Emission"),size=2, alpha=1, shape=10, stroke=1.1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  scale_y_continuous(name = left_axis2, 
                     sec.axis = sec_axis(~. * 1/axis_scale2, name = right_axis2))+
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_fill_manual(values=HeatECColour,
                    name="Heating Fuels \n(left axis)",
                    breaks=HeatingCarriers,
                    labels=primheat_labels) +
  scale_color_manual(values=c(CoolingElec="gray21",Emission="firebrick"),
                     name="",
                     labels=c("Electricity for Cooling \n(left axis)","Heating & Cooling Emissions \n(right axis)")) +
  facet_grid(Region~ScenOrder, scales="free_y", labeller=labeller(Region=reg_labels, ScenOrder=scen_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
FuelsEmis.BMR

axis_scale2 = 17
FuelsEmis.Aggr <- ggplot() + 
  geom_bar(data=subset(DATA.FIG2, ((Variable=="FEHeat"&Prim=="Total")|(Variable=="FECool"&Prim=="Total")|(Prim=="ElecPV")) & Region %in% RCPRegions), 
           aes(x=Year,y = value/1e9, fill=Variable),alpha=0.7, stat="identity") +
  geom_point(data=subset(DATA.EM, Scenario %in% ScenBase& Year %in% ActiveYears & Region %in% RCPRegions & Variable=="EmisCO2DirectHeatCool")
             , aes(x=Year,y = value/1e12 * axis_scale2, colour=Variable),size=2, alpha=0.8, shape=10, stroke=1.1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  scale_y_continuous(name = left_axis2, 
                     sec.axis = sec_axis(~. * 1/axis_scale2, name = right_axis2))+
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_fill_manual(values=c("firebrick","dodgerblue","gold2"),
                    name="Final Energy \n(left axis)",
                    breaks=c("FEHeat","FECool","FEResElecPVHeatCool"),
                    labels=c("Heating ","Cooling","Rooftop Photovoltaic \n(Generation)")) +
  scale_color_manual(values="black",
                     name="",
                     labels="Direct Heating & \nCooling emissions \n(right axis)") +
  facet_grid(Region~ScenOrder, scales="free_y", labeller=labeller(Region=reg_labels, ScenOrder=scen_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
FuelsEmis.Aggr

FuelsEmis.AggrGlob <- ggplot() + 
  geom_bar(data=subset(DATA.FIG2, ((Variable=="FEHeat"&Prim=="Total")|(Variable=="FECool"&Prim=="Total")) & Region == "World"), 
           aes(x=Year,y = value/1e9, fill=Variable),alpha=0.7, stat="identity") +
  geom_point(data=subset(DATA.EM, Scenario %in% ScenBase& Year %in% ActiveYears & Region == "World" & Variable=="EmisCO2DirectHeatCool")
             , aes(x=Year,y = value/1e12 * axis_scale2, colour=Variable),size=2, alpha=1, shape=10, stroke=1.1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  scale_y_continuous(name = left_axis2, 
                     sec.axis = sec_axis(~. * 1/axis_scale2, name = right_axis2))+
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_fill_manual(values=c("firebrick1","deepskyblue"),
                    name="Final Energy",
                    breaks=c("FEHeat","FECool"),
                    labels=c("Heating ","Cooling")) +
  scale_color_manual(values="black",
                     name="",
                     labels="Direct + Indirect \nHeating & Cooling emissions \n(right axis)") +
  facet_grid(Region~ScenOrder, scales="free_y", labeller=labeller(Region=reg_labels, ScenOrder=scen_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
FuelsEmis.AggrGlob

# 
# ---- Figure 3: Decomposition ----
axis_scale3 = 35
left_axis3 = "Secondary Energy [EJ/yr]"
right_axis3 = expression(paste("Heating & Cooling Emissions", "[Gt",CO[2],"/yr]",""))
                         
Areas <- ggplot(data=DATA.FIG3) + 
  geom_ribbon(data=subset(DATA.FIG3, Variable=="FECoolHeat" & !ScenOrder=="SSP2_SPA0_26I_D_Baseline"), 
              aes(x=Year, ymin=min/1e9,ymax=max/1e9, fill=ScenOrder), alpha="0.5", colour="gray30", size=0.1) +
  geom_ribbon(data=subset(DATA.FIG3, Variable=="EmisCO2DirectHeatCool" & !ScenOrder=="SSP2_SPA0_26I_D_Baseline"), 
              aes(x=Year, ymin=min/1e12 * axis_scale3,ymax=max/1e12 * axis_scale3, fill=ScenOrder), alpha="0.5", colour="gray30", size=0.1) +
  geom_line(data=subset(DATA.FIG3, Variable=="FECoolHeat" & ScenOrder %in% ScenBase2), 
            aes(x=Year,y = value/1e9, color=ScenOrder),size=0.75, alpha=1) +
  geom_line(data=subset(DATA.FIG3, Variable=="EmisCO2DirectHeatCool" & ScenOrder %in% ScenBase2), 
            aes(x=Year,y = value/1e12 * axis_scale3, color=ScenOrder),size=0.75, alpha=1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  scale_y_continuous(name = left_axis3, 
                     sec.axis = sec_axis(~. * 1/axis_scale3, name = right_axis3))+
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_line(colour="gray80", size = 0.3)) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  xlab("") + ylab("EJ/yr") +
  scale_colour_manual(values=c("gray50","firebrick","forestgreen","forestgreen","forestgreen"),
                    name="",
                    breaks=c("SSP2_InsulNew","SSP2_Baseline","SSP2_SPA0_26I_D_InsulNew","SSP2_SPA0_26I_D_InsulAll","SSP2_SPA0_26I_D_Baseline"),
                    labels=c("Baseline (No renovations)","Baseline","x","y","2°C")) +
  scale_fill_manual(values=c("gray75","green3","darkorchid4","skyblue"),
                    name="",
                    breaks=c("SSP2_InsulNew","SSP2_Baseline","SSP2_SPA0_26I_D_InsulNew","SSP2_SPA0_26I_D_InsulAll"),
                    labels=c("Effect of Renovation \nin Baseline",
                             "Effect of Insulation \nin New Buildings",
                             "Effect of Renovations \n(mitigation)",
                             "Effect of heating & \ncooling technology choices")) +
  facet_grid(Region~Variable, scales="free_y",labeller=labeller(Region=reg_labels, Variable=var_labels)) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
Areas

Emis.decomp <- ggplot(data=DATA.FIG3) + 
  geom_ribbon(data=subset(DATA.FIG3, Variable=="EmisCO2DirectHeatCool" & !ScenOrder=="SSP2_SPA0_26I_D_Baseline"), 
              aes(x=Year, ymin=min/1e12, ymax=max/1e12, fill=ScenOrder), alpha="0.5", colour="gray30", size=0.1) +
  geom_line(data=subset(DATA.FIG3, Variable=="EmisCO2DirectHeatCool" & ScenOrder %in% ScenBase), 
            aes(x=Year,y = value/1e12, color=ScenOrder),size=0.75, alpha=1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_line(colour="gray80", size = 0.3)) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.box = "vertical") +
  xlab("") + ylab(right_axis3) +
  scale_colour_manual(values=c("gray","firebrick","forestgreen","forestgreen","forestgreen"),
                      name="",
                      breaks=c("SSP2_InsulNew","SSP2_Baseline","SSP2_SPA0_26I_D_InsulNew","SSP2_SPA0_26I_D_InsulAll","SSP2_SPA0_26I_D_Baseline"),
                      labels=c("NoRenov","Baseline","x","y","2°C")) +
  scale_fill_manual(values=c("gray75","green3","darkorchid4","skyblue"),
                    name="",
                    breaks=c("SSP2_InsulNew","SSP2_Baseline","SSP2_SPA0_26I_D_InsulNew","SSP2_SPA0_26I_D_InsulAll"),
                    labels=c("Effect of Renovation \nin Baseline",
                             "Effect of Insulation \nin New Buildings",
                             "Effect of Renovations \n(mitigation)",
                             "Effect of heating & \ncooling technology choices")) +
  facet_wrap(.~Region, nrow = 2, scales="free_y",labeller=labeller(Region=reg_labels, Variable=var_labels)) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
Emis.decomp

#
# ---- Figure S1: Age Cohorts ----
Floorspace.BAR <- ggplot(data=subset(DATA.AGE, Scenario=="SSP2_Baseline" & Variable=="Floorspace" & Region %in% RCPRegions & Year %in% ActiveYears)
                         ,aes(x=Year,y = value/1e9, fill=AgeCohortOrder)) + 
  geom_bar(stat="identity") +
  ylab(left_axis1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_fill_manual(values=c("gray90","gray70","gray50","gray30","gray20","black"),
                    name="Age Cohort \n(years since construction)",
                    breaks=c("<10","11to20years","21to30years","31to40years","41to50years",">51"),
                    labels=c("< 10 years","11 to 20","21 to 30","31 to 40","41 to 50","> 51")) +
  facet_wrap(Region~., nrow=2, scales="free_y", labeller=labeller(Region=reg_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
Floorspace.BAR

FloorspaceFrac.BAR <- ggplot(data=subset(DATA.AGE, Scenario=="SSP2_Baseline" & Variable=="FloorspaceFrac" & Region %in% RCPRegions & Year %in% ActiveYears)
                             ,aes(x=Year,y = value*100, fill=AgeCohortOrder)) + 
  geom_bar(stat="identity") +
  ylab("% of floorspace") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_fill_manual(values=c("gray90","gray70","gray50","gray30","gray20","black"),
                    name="Age Cohort \n(years since construction)",
                    breaks=c("<10","11to20years","21to30years","31to40years","41to50years",">51"),
                    labels=c("< 10 years","11 to 20","21 to 30","31 to 40","41 to 50","> 51")) +
  facet_wrap(Region~., nrow=2, scales="free_y", labeller=labeller(Region=reg_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
FloorspaceFrac.BAR
#
# ---- Figure S2: Insulation level Stocks ----
Stck.BMR <- ggplot(data=DATA.FIG1,aes(x=Year,y = value/1e9, fill=InsulLevel)) + 
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ylab(left_axis1) +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_fill_manual(values=c("firebrick","chocolate1","yellow","cornflowerblue","chartreuse","forestgreen"),
                    name="Insulation level",
                    breaks=c("1","2","3","4","5","6"),
                    labels=c("1","2","3","4","5","6")) +
  facet_grid(Region~ScenOrder, scales="free_y", labeller=labeller(Region=reg_labels, ScenOrder=scen_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
Stck.BMR

StckUV.MR <- ggplot() + 
  geom_bar(data=subset(DATA.FIG1, Scenario == "SSP2_SPA0_26I_D_Baseline"),aes(x=Year,y = value/1e9, fill=InsulLevel), stat="identity") +
  geom_point(data=subset(DATA.FIG1a, Variable=="UEUvalue" & Scenario == "SSP2_SPA0_26I_D_Baseline"),
             aes(x=Year,y = value * axis_scale1),size=1.0, alpha=0.66) +
  scale_y_continuous(name = left_axis1, 
                     sec.axis = sec_axis(~. * 1/axis_scale1, name = right_axis1))+
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  scale_fill_manual(values=c("firebrick","chocolate1","yellow","cornflowerblue","chartreuse","forestgreen"),
                    name="Insulation level",
                    breaks=c("1","2","3","4","5","6"),
                    labels=c("1","2","3","4","5","6"),
                    guide = guide_legend(direction = "horizontal", title.position =  "top", nrow = 1)) +
  facet_wrap(Region~., nrow=2, scales="free_y", labeller=labeller(Region=reg_labels, ScenOrder=scen_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
StckUV.MR

#
# ---- Figure S3: PV Costs ----
PVInv.BR <- ggplot(data=subset(DATA.PVC, Scenario == "SSP2_SPA0_26I_D_Baseline" & Variable=="TotalInvPV" & Year %in% ActiveYears & Region %in% RCPRegions & !Region=="World")
                    , aes(x=Year,y = value, colour=Region)) + 
  geom_line(size=1, alpha=1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  ggtitle("A. Residential PV Investment Costs") +
  xlim(2020,2100) +
  ylim(0,2000) +
  xlab("") + ylab(expression(paste("Capital + O&M Cost [$"[2010],"/kW"[e],"]"))) +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_line(colour="gray80", size = 0.3)) + 
  theme(text= element_text(size=FSizeLeg, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_colour_manual(values=c("dodgerblue","firebrick","darkgoldenrod1","dimgray","forestgreen"),
                      name="",
                      breaks=c("OECD90","REF","ASIA","MAF","LAM"),
                      labels=c("OECD","Reforming \nEconomies","Asia","Middle East \n& Africa","Latin \nAmerica")) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
PVInv.BR

PVLCOE.MRQ <- ggplot() + 
  geom_line(data=subset(DATA.PVC, Scenario == "SSP2_SPA0_26I_D_Baseline" & Unit == "US$/kWhe" & 
                          Year %in% ActiveYears & Region %in% RCPRegions & Demographic %in% ActiveDemog & !Region=="World")
            , aes(x=Year,y = value, colour=DemogOrder, alpha=DemogQ/5), size=1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  ggtitle("B. Residential PV Levelized Costs of Electricity") +
  xlim(2020,2100) + ylim(0,1) +
  xlab("") + ylab(expression(paste("$"[2010],"/kWh"[e]))) +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_line(colour="gray80", size = 0.3)) + 
  theme(text= element_text(size=FSizeLeg, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  scale_colour_manual(values=c("black","black","black","black","black",
                               "deepskyblue","deepskyblue","deepskyblue","deepskyblue","deepskyblue"),
                      name="Demographic:",
                      breaks=c( "U1","","","","",
                                "R1","","","",""),
                      labels=c( "Urban","","","","",
                                "Rural","","","","")) +
  scale_alpha(range=c(0.2,1),
              name="Quintile \n(Shade)",
              labels=c("1","2","3","4","5")) +
  # scale_alpha(guide=FALSE) +
  facet_wrap(Region~., nrow = 2, labeller=labeller(Region=reg_labels, ScenOrder=scen_labels)) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
PVLCOE.MRQ

lay<-rbind(1,1,1,1,
           2,2,2,2,2,2,2,2) 
ResPV <- grid.arrange(PVInv.BR,PVLCOE.MRQ, layout_matrix=lay)

# 
#
# ---- Figure S4: Intensity ----
UEInt.BMR <- ggplot(data=subset(DATA.UE, Scenario %in% ScenBase & Variable=="UEHeatCoolpfs" & Year %in% ActiveYears & Region %in% RCPRegions)
                    , aes(x=Year,y = Normalised_2020, colour=ScenOrder)) + 
  geom_line(size=1, alpha=1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  xlim(2020,2100) +
  xlab("") + ylab(expression(paste("Useful Energy Intensity for Heating and Cooling [kJ/m"^"2",", 2020 = 1]"))) +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_line(colour="gray80", size = 0.3)) + 
  theme(text= element_text(size=FSizeLeg, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_colour_manual(values=c("black","green3"),
                      name="",
                      breaks=c("SSP2_Baseline","SSP2_SPA0_26I_D_Baseline"),
                      labels=c("Baseline","2°C")) +
  facet_wrap(Region~., nrow=3, labeller=labeller(Region=reg_labels)) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
UEInt.BMR
#

# ---- Figure S5: Energy Independence ----
EnIndep.BMRQ <- ggplot() + 
  geom_jitter(data=subset(DATA.ID, Scenario %in% ScenBase & Year %in% ActiveYears & Region %in% RCPRegions & Demographic %in% ActiveDemog1)
            , aes(x=Year,y = value, shape=DemogOrder, colour=DemogOrder), size=2, width=1, alpha=1, stroke=1) +
  geom_line(data=subset(DATA.ID, Scenario %in% ScenBase & Year %in% ActiveYears & Region %in% RCPRegions & Demographic %in% ActiveDemog)
            , aes(x=Year,y = value, colour=DemogOrder, alpha=DemogQ/5), size=0.5) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  xlim(2020,2100) +
  xlab("") + ylab("Fraction of Final Energy Independence [-]") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_line(colour="gray80", size = 0.3)) + 
  theme(text= element_text(size=FSizeLeg, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_shape_manual(values=c(1,4,8),
                      name="Demographic",
                      breaks=c( "Total","Urban","Rural"),
                      labels=c( "Total","Urban","Rural")) +
  scale_colour_manual(values=c("deepskyblue","deepskyblue","deepskyblue","deepskyblue","deepskyblue",
                               "deepskyblue","red",
                               "black","black","black","black","black",
                               "black"),
                      name="Demographic:",
                      breaks=c( "R1","R2","R3","R4","R5",
                                "Rural","Total",
                                "U1","U2","U3","U4","U5",
                                "Urban"),
                      labels=c( "Rural","","","","",
                                "Rural","Total",
                                "Urban","","","","",
                                "Urban"),
                      guide=FALSE) +
  scale_alpha(range=c(0.2,1),
              name="Quintile \n(Shade)",
              labels=c("1","2","3","4","5")) +
  facet_grid(Region~ScenOrder, labeller=labeller(Region=reg_labels, ScenOrder=scen_labels)) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
EnIndep.BMRQ

EnIndep.MRQ <- ggplot() + 
  geom_line(data=subset(DATA.ID, Scenario == "SSP2_SPA0_26I_D_Baseline" & Year %in% ActiveYears & Region %in% RCPRegions & Demographic %in% ActiveDemog)
            , aes(x=Year,y = value, colour=DemogOrder, alpha=DemogQ/5), size=1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  xlim(2020,2100) +
  xlab("") + ylab("Fraction of Final Energy Independence [-]") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_line(colour="gray80", size = 0.3)) + 
  theme(text= element_text(size=FSizeLeg, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  scale_colour_manual(values=c("black","black","black","black","black",
                               "deepskyblue","deepskyblue","deepskyblue","deepskyblue","deepskyblue"),
                      name="Demographic:",
                      breaks=c( "U1","","","","",
                                "R1","","","",""),
                      labels=c( "Urban","","","","",
                                "Rural","","","","")) +
  scale_alpha(range=c(0.2,1),
              name="Quintile \n(Shade)",
              labels=c("1","2","3","4","5")) +
  # scale_alpha(guide=FALSE) +
  facet_wrap(Region~., nrow = 2, labeller=labeller(Region=reg_labels, ScenOrder=scen_labels)) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
EnIndep.MRQ

#
# # ---- OUTPUTS FOR MANUSCRIPT ----
# write.xlsx(DATA.T1, file="output/BuildStocks/Table1.xlsx", sheetName="Table 1", append=FALSE, row.names=FALSE, showNA = TRUE)
# 
# png(file = "output/BuildStocks/Fig1.png", width = 7*ppi, height = 5.5*ppi, units = "px", res = ppi)
# plot(StckUV.BMR)
# dev.off()
# 
# png(file = "output/BuildStocks/Fig2.png", width = 7*ppi, height = 8*ppi, units = "px", res = ppi)
# plot(FuelsEmis.Aggr)
# dev.off()
# 
# png(file = "output/BuildStocks/Fig3.png", width = 7*ppi, height = 8*ppi, units = "px", res = ppi)
# plot(Areas)
# dev.off()
# 
# png(file = "output/BuildStocks/FigS1.png", width = 8*ppi, height = 5*ppi, units = "px", res = ppi)
# plot(Floorspace.BAR)
# dev.off()
# 
# png(file = "output/BuildStocks/FigS2.png", width = 6*ppi, height = 8*ppi, units = "px", res = ppi)
# plot(Stck.BMR)
# dev.off()
# 
# png(file = "output/BuildStocks/FigS3.png", width = 5*ppi, height = 7*ppi, units = "px", res = ppi)
# plot(ResPV)
# dev.off()
# 
# png(file = "output/BuildStocks/FigS4.png", width = 7*ppi, height = 8*ppi, units = "px", res = ppi)
# plot(UEInt.BMR)
# dev.off()
# 
# png(file = "output/BuildStocks/FigS5.png", width = 6*ppi, height = 8*ppi, units = "px", res = ppi)
# plot(EnIndep.BMRQ)
# dev.off()
# #
# # ---- OTHER OUTPUTS ----
# png(file = "output/BuildStocks/Other/Fig2_Fuels.png", width = 7*ppi, height = 8*ppi, units = "px", res = ppi)
# plot(FuelsEmis.BMR)
# dev.off()
# 
# png(file = "output/BuildStocks/Other/Fig2_AggregateGlobal.png", width = 7*ppi, height = 3*ppi, units = "px", res = ppi)
# plot(FuelsEmis.AggrGlob)
# dev.off()
# 
# png(file = "output/BuildStocks/Other/Fig3_Emis.png", width = 7*ppi, height = 5*ppi, units = "px", res = ppi)
# plot(Emis.decomp)
# dev.off()
# 
# png(file = "output/BuildStocks/Other/FigS3_Mitig.png", width = 7*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(EnIndep.MRQ)
# dev.off()
# 
# png(file = "output/BuildStocks/Other/Fig1_InsulLev.png", width = 7*ppi, height = 5*ppi, units = "px", res = ppi)
# plot(StckUV.MR)
# dev.off()
# 
# png(file = "output/BuildStocks/Other/AgeCohortFrac.png", width = 8*ppi, height = 5*ppi, units = "px", res = ppi)
# plot(FloorspaceFrac.BAR)
# dev.off()
# 
# # ---- SUPPLEMENTARY DATA OUTPUT ----
# wb <- createWorkbook()
# 
# addWorksheet(wb, "Final Energy")
# writeDataTable(wb, sheet = "Final Energy", x = SupData.FE)
# 
# addWorksheet(wb, "Useful Energy")
# writeDataTable(wb, sheet = "Useful Energy", x = SupData.UE)
# 
# addWorksheet(wb, "Emissions")
# writeDataTable(wb, sheet = "Emissions", x = SupData.EM)
# 
# addWorksheet(wb, "Floor Space")
# writeDataTable(wb, sheet = "Floor Space", x = SupData.FS)
# 
# addWorksheet(wb, "Renovation Rate")
# writeDataTable(wb, sheet = "Renovation Rate", x = SupData.RR)
# 
# saveWorkbook(wb, "output/BuildStocks/IMAGE_Residential_Supplementary_Data.xlsx", overwrite = TRUE)
