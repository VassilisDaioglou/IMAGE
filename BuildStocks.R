# ---- INFORMATION ----
# R script to process the results of the TIMER Building Stocks & Renovation 
# Vassilis Daioglou, November 2019
# 
# Make appropriate to read IAMC template outputs
# Vassilis Daioglou, April 2020
#
# Reformulate for Paper on energy efficiency and technology choice
# Vassilis Daioglou, May 2020
# 
# This script processes and presents the results of a number of scenarios 
# Aimed at showing the impacts on energy use and CO2 emissions of:
# (i) Building insulation and renovation
# (ii) How these contrast behavioural (demand) changes
# 
# For this a number of scenarios are run with TIMER:
# 1. Baseline (SSP2)
# 2. InsulNew (No retrofits - but improvements of marginal stock allowed - Energy Carrier market shares same as baseline)
# 3. InsulAll (All insulation (new + retrofit) allowed - Energy Carrier market shares same as baseline)
# 4. Demand (Reduced demand of energy services in residential sector)	
# 5. Floorspace (Reduced residential floorspace)
# 6. Full (Demand + Floorspace from above)	
#
# Baseline repeated for a mitigation scenario (450)
# All other only for mitigation in order to decompose how they contribute to mitigation
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

Scenarios = c("SSP2_Baseline",
              "SSP2_450_Baseline","SSP2_450_Full","SSP2_450_Demand","SSP2_450_Floorspace","SSP2_450_InsulAll","SSP2_450_InsulNew")
ScenBase  =c("SSP2_Baseline","SSP2_450_Baseline")
ScenStand = c("SSP2_Baseline","SSP2_450_Baseline","SSP2_450_Full","SSP2_450_Demand","SSP2_450_Floorspace","SSP2_450_InsulAll","SSP2_450_InsulNew")
ScenInsul = c("SSP2_Baseline","SSP2_450_InsulNew","SSP2_450_InsulAll","SSP2_450_Baseline")
ScenBehav = c("SSP2_Baseline","SSP2_450_Baseline","SSP2_450_Demand","SSP2_450_Floorspace","SSP2_450_Full")

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

  # Mitigation Scenarios
data_SSP1_mitig <- "data/BuildStocks/BuildingStocks/SSP1_450_Baseline.xlsx"
data_SSP2_mitig <- "data/BuildStocks/BuildingStocks/SSP2_450_Baseline.xlsx"
data_SSP3_mitig <- "data/BuildStocks/BuildingStocks/SSP3_450_Baseline.xlsx"
data_SSP4_mitig <- "data/BuildStocks/BuildingStocks/SSP4_450_Baseline.xlsx"
data_SSP5_mitig <- "data/BuildStocks/BuildingStocks/SSP5_450_Baseline.xlsx"

  # Sensitivity Scenarios
# data_mitig_Full <- "data/BuildStocks/BuildingStocks/SSP2_450_Full.xlsx"
# data_mitig_Demand <- "data/BuildStocks/BuildingStocks/SSP2_450_Demand.xlsx"
# data_mitig_Floorspace <- "data/BuildStocks/BuildingStocks/SSP2_450_Floorspace.xlsx"
data_mitig_InsulAll <- "data/BuildStocks/BuildingStocks/SSP2_450_InsulAll.xlsx"
data_mitig_InsulNew <- "data/BuildStocks/BuildingStocks/SSP2_450_InsulNew.xlsx"

# set higher RAM capacity for java (used in clsx package)
options(java.parameters = "-Xmx8000m")

# ---- INPUTS: Data ----
  # set directory path 
setwd("C:/Users/Asus/Documents/Github/IMAGE/")
  # Read Data Files for Baseline Scenario
Baseline = rbind(read.xlsx(data_SSP1, sheet = "data"),
                 read.xlsx(data_SSP2, sheet = "data"),
                 read.xlsx(data_SSP3, sheet = "data"),
                 read.xlsx(data_SSP4, sheet = "data"),
                 read.xlsx(data_SSP5, sheet = "data"))

# Mitigation = rbind(read.xlsx(data_SSP1_mitig, sheet = "data"),
#                    read.xlsx(data_SSP2_mitig, sheet = "data"),
#                    read.xlsx(data_SSP3_mitig, sheet = "data"),
#                    read.xlsx(data_SSP4_mitig, sheet = "data"),
#                    read.xlsx(data_SSP5_mitig, sheet = "data"))
Mitigation = read.xlsx(data_SSP2_mitig, sheet = "data")

# Demand_450 = read.xlsx(data_mitig_Demand, sheet = "data")
# Floorspace_450 = read.xlsx(data_mitig_Floorspace, sheet = "data")
# Full_450 = read.xlsx(data_mitig_Full, sheet = "data")
InsulAll_450 = read.xlsx(data_mitig_InsulAll, sheet = "data")
InsulNew_450 = read.xlsx(data_mitig_InsulNew, sheet = "data")

rm(data_SSP1,data_SSP2,data_SSP3,data_SSP4,data_SSP5,
   data_SSP1_mitig,data_SSP2_mitig,data_SSP3_mitig,data_SSP4_mitig,data_SSP5_mitig,
   # data_mitig_Full,data_mitig_Demand,data_mitig_Floorspace,
   data_mitig_InsulAll,data_mitig_InsulNew)
#
# ---- MUNGING ----
# Create Single Dataset
Baseline = melt(Baseline, id.vars=c("Model","Scenario","Region","Variable","Unit"))

Mitigation = melt(Mitigation, id.vars=c("Model","Scenario","Region","Variable","Unit"))
# Full_450 = melt(Full_450, id.vars=c("Model","Scenario","Region","Variable","Unit"))
# Demand_450 = melt(Demand_450, id.vars=c("Model","Scenario","Region","Variable","Unit"))
# Floorspace_450 = melt(Floorspace_450, id.vars=c("Model","Scenario","Region","Variable","Unit"))
InsulAll_450 = melt(InsulAll_450, id.vars=c("Model","Scenario","Region","Variable","Unit"))
InsulNew_450 = melt(InsulNew_450, id.vars=c("Model","Scenario","Region","Variable","Unit"))

DATA = rbind(Baseline,
             Mitigation,
             # Full_450,Demand_450,Floorspace_450,
             InsulAll_450,InsulNew_450)
rm(Baseline,Mitigation,
   # Full_450,Demand_450,Floorspace_450,
   InsulAll_450,InsulNew_450)
DATA$Model <- NULL
colnames(DATA)[5] <- "Year"

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
DATA$Variable <- gsub("[[:space:]]","",DATA$Variable,fixed=F)

DATA$Year = as.numeric(substr(DATA$Year, start=1, stop=4))

DATA$ScenOrder = factor(DATA$Scenario, levels =c("SSP1_Baseline","SSP2_Baseline","SSP3_Baseline","SSP4_Baseline","SSP5_Baseline",
                                                 "SSP2_450_Demand","SSP2_450_Floorspace","SSP2_450_Full","SSP2_450_InsulNew","SSP2_450_InsulAll","SSP2_450_Baseline"))
# ---- RCP REGION DEFINITON ----
  # First determing wieghting factors population & floorspace
Weights = subset(DATA, Variable=="Pop"|Variable=="FloorspaceTotal")
Weights = spread(Weights,Region,value)
Weights = Weights %>% mutate(OECD90=CAN+JAP+OCE+TUR+USA+WEU)
Weights = Weights %>% mutate(REF=CEU+RUS+STAN+UKR)
Weights = Weights %>% mutate(ASIA=CHN+INDIA+INDO+KOR+RSAS+SEAS)
Weights = Weights %>% mutate(MAF=EAF+ME+NAF+RSAF+SAF+WAF)
Weights = Weights %>% mutate(LAM=BRA+MEX+RCAM+RSAM)
Weights = melt(Weights,id.vars=c("Scenario","Variable","Unit","Year","ScenOrder"), na.rm=FALSE )
Weights$Unit <- NULL
Weights = spread(Weights,Variable,value)
colnames(Weights)[colnames(Weights)=="variable"]<-"Region"
Weights$ID = paste(Weights$Scenario,Weights$Region,Weights$Year)
  # Identify variables whose regions can be summed (i.e. absolute measures)
DATA.R1 = subset(DATA, Unit=="kgCO2/yr"|Unit=="GJ"|Unit=="m2"|Unit=="Bn$")
DATA.R1 = spread(DATA.R1,Region,value)
DATA.R1 = DATA.R1 %>% mutate(OECD90=CAN+JAP+OCE+TUR+USA+WEU)
DATA.R1 = DATA.R1 %>% mutate(REF=CEU+RUS+STAN+UKR)
DATA.R1 = DATA.R1 %>% mutate(ASIA=CHN+INDIA+INDO+KOR+RSAS+SEAS)
DATA.R1 = DATA.R1 %>% mutate(MAF=EAF+ME+NAF+RSAF+SAF+WAF)
DATA.R1 = DATA.R1 %>% mutate(LAM=BRA+MEX+RCAM+RSAM)
DATA.R1 = melt(DATA.R1,id.vars=c("Scenario","Variable","Unit","Year","ScenOrder"), na.rm=FALSE )
colnames(DATA.R1)[colnames(DATA.R1)=="variable"]<-"Region"
  # Identify Variables whose RCP regional data will be POPULATION weighted 
DATA.R2 = subset(DATA, Variable=="CCElec"|Variable=="CCHeat"|Variable=="EmisCO2HeatCoolpc"|Variable=="UEHeatCoolpc")
DATA.R2$ID = paste(DATA.R2$Scenario,DATA.R2$Region,DATA.R2$Year)
DATA.R2$Pop <- Weights[match(DATA.R2$ID, Weights$ID),"Pop"] 
DATA.R2 = DATA.R2 %>% mutate(PopWeight = value * Pop)
DATA.R2 = subset(DATA.R2, select=-c(value,Pop,ID))
DATA.R2 = spread(DATA.R2,Region,PopWeight)
DATA.R2 = DATA.R2 %>% mutate(OECD90=CAN+JAP+OCE+TUR+USA+WEU)
DATA.R2 = DATA.R2 %>% mutate(REF=CEU+RUS+STAN+UKR)
DATA.R2 = DATA.R2 %>% mutate(ASIA=CHN+INDIA+INDO+KOR+RSAS+SEAS)
DATA.R2 = DATA.R2 %>% mutate(MAF=EAF+ME+NAF+RSAF+SAF+WAF)
DATA.R2 = DATA.R2 %>% mutate(LAM=BRA+MEX+RCAM+RSAM)
DATA.R2 = melt(DATA.R2,id.vars=c("Scenario","Variable","Unit","Year","ScenOrder"), na.rm=FALSE )
colnames(DATA.R2)[colnames(DATA.R2)=="variable"]<-"Region"

DATA.R2$ID = paste(DATA.R2$Scenario,DATA.R2$Region,DATA.R2$Year)
DATA.R2$Pop <- Weights[match(DATA.R2$ID, Weights$ID),"Pop"] 
DATA.R2 = DATA.R2 %>% mutate(valueCor = value / Pop)
DATA.R2 = subset(DATA.R2, select = -c(value,ID,Pop))
colnames(DATA.R2)[colnames(DATA.R2)=="valueCor"] <- "value"
# 
  # Identify Variables whose RCP regional data will be FLOORSPACE weighted 
DATA.R3 = subset(DATA, Variable=="InsulAverageRenovRate"|Variable=="UEHeatCoolpfs"|Variable=="UEIntHeat"|Variable=="UEUvalue"|
                   Variable=="AgeCohFrac"|
                   Variable=="FEResIndepTotal"|Variable=="FEResIndepUrban"|Variable=="FEResIndepRural"|
                   Variable=="FEResIndepU1"|Variable=="FEResIndepU2"|Variable=="FEResIndepU3"|Variable=="FEResIndepU4"|Variable=="FEResIndepU5"|
                   Variable=="FEResIndepR1"|Variable=="FEResIndepR2"|Variable=="FEResIndepR3"|Variable=="FEResIndepR4"|Variable=="FEResIndepR5")
DATA.R3$ID = paste(DATA.R3$Scenario,DATA.R3$Region,DATA.R3$Year)
DATA.R3$FS <- Weights[match(DATA.R3$ID, Weights$ID),"FloorspaceTotal"] 
DATA.R3 = DATA.R3 %>% mutate(FSWeight = value * FS)
DATA.R3 = subset(DATA.R3, select=-c(value,FS,ID))
DATA.R3 = spread(DATA.R3,Region,FSWeight)
DATA.R3 = DATA.R3 %>% mutate(OECD90=CAN+JAP+OCE+TUR+USA+WEU)
DATA.R3 = DATA.R3 %>% mutate(REF=CEU+RUS+STAN+UKR)
DATA.R3 = DATA.R3 %>% mutate(ASIA=CHN+INDIA+INDO+KOR+RSAS+SEAS)
DATA.R3 = DATA.R3 %>% mutate(MAF=EAF+ME+NAF+RSAF+SAF+WAF)
DATA.R3 = DATA.R3 %>% mutate(LAM=BRA+MEX+RCAM+RSAM)
DATA.R3 = melt(DATA.R3,id.vars=c("Scenario","Variable","Unit","Year","ScenOrder"), na.rm=FALSE )
colnames(DATA.R3)[colnames(DATA.R3)=="variable"]<-"Region"

DATA.R3$ID = paste(DATA.R3$Scenario,DATA.R3$Region,DATA.R3$Year)
DATA.R3$FS <- Weights[match(DATA.R3$ID, Weights$ID),"FloorspaceTotal"] 
DATA.R3 = DATA.R3 %>% mutate(valueCor = value / FS)
DATA.R3 = subset(DATA.R3, select = -c(value,ID,FS))
colnames(DATA.R3)[colnames(DATA.R3)=="valueCor"] <- "value"

#
  # Final Data Set
DATA1 = rbind(DATA.R1,DATA.R2,DATA.R3)
rm(DATA, DATA.R1, DATA.R2, DATA.R3)
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
# ---- ***Carbon Contents (CC)*** ----
DATA.CC <- subset(DATA1, Variable=="CCElec"|Variable=="CCHeat")

# ---- ***Floorspace (FS)*** ----
DATA.FS <- subset(DATA1, Variable=="InsulFloorspaceLevel1"|Variable=="InsulFloorspaceLevel2"|Variable=="InsulFloorspaceLevel3"|
                    Variable=="InsulFloorspaceLevel4"|Variable=="InsulFloorspaceLevel5"|Variable=="InsulFloorspaceLevel6")
DATA.FS$InsulLevel <- DATA.FS$Variable
DATA.FS$InsulLevel <- gsub("InsulFloorspaceLevel","",DATA.FS$InsulLevel,fixed=F)
DATA.FS$Variable = substr(DATA.FS$Variable, start=6, stop=15)
DATA.FS <- DATA.FS[c("Scenario","Region","Year","Variable","InsulLevel","Unit","value","ScenOrder")]

# --- ***Age Cohorts (AGE)*** ----
DATA.AGE <- subset(DATA1, Variable=="AgeCoh"|Variable=="AgeCohFrac")

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
temp.Base = subset(temp, ScenOrder == "SSP2_Baseline")
temp.EffNew = subset(temp, ScenOrder == "SSP2_450_InsulNew")
temp.EffNewRen = subset(temp, ScenOrder == "SSP2_450_InsulAll")
temp.Tot = subset(temp, ScenOrder == "SSP2_450_Baseline")

# Complete dataset
Area.EffNew = temp.Base
colnames(Area.EffNew)[colnames(Area.EffNew)=="value"] <- "max"
Area.EffNew$min <- temp.EffNew[match(Area.EffNew$ID,temp.EffNew$ID),"value"]

Area.EffNewRen = temp.EffNew
colnames(Area.EffNewRen)[colnames(Area.EffNewRen)=="value"] <- "max"
Area.EffNewRen$min <- temp.EffNewRen[match(Area.EffNewRen$ID,temp.EffNewRen$ID),"value"]

Area.EffNewRenFuel = temp.EffNewRen
colnames(Area.EffNewRenFuel)[colnames(Area.EffNewRenFuel)=="value"] <- "max"
Area.EffNewRenFuel$min <- temp.Tot[match(Area.EffNewRenFuel$ID,temp.Tot$ID),"value"]

DATA.FIG3 = rbind(Area.EffNew,Area.EffNewRen,Area.EffNewRenFuel)

#  Add total emissions/energy for each scenario
temp$ID = paste(temp$Region, temp$ScenOrder, temp$Variable, temp$Year)
DATA.FIG3$ID = paste(DATA.FIG3$Region, DATA.FIG3$ScenOrder, DATA.FIG3$Variable, DATA.FIG3$Year)

DATA.FIG3$value <- temp[match(DATA.FIG3$ID,temp$ID),"value"]

temp$min <- NA
temp$max <- NA

#  Add emission and energy projection for the mitigation scenarios
DATA.FIG3 = rbind(DATA.FIG3,subset(temp, ScenOrder == "SSP2_450_Baseline"))

DATA.FIG3$ID <- NULL
DATA.FIG3$Variable = factor(DATA.FIG3$Variable, levels=c("FECoolHeat","EmisCO2DirectHeatCool"))
rm(temp,temp1,temp.Base,temp.EffNew,temp.EffNewRen,temp.Tot,Area.EffNew,Area.EffNewRen,Area.EffNewRenFuel)

# 
# ---- LABELS ----
scen_labels <-c("SSP2_Baseline"="Baseline",
                "SSP2_450_Baseline"="2°C",
                "SSP2_450_Full"="Full \n2°C",
                "SSP2_450_Demand"="Demand \n2°C",
                "SSP2_450_Floorspace"="Floorspace \n2°C",
                "SSP2_450_InsulAll"="New and Retrofit \n2°C",
                "SSP2_450_InsulNew"="New Building \nInsulation - 2°C")

scen_labels2 <-c("SSP2_Baseline"="Baseline",
                "SSP2_450_Baseline"="2°C",
                "SSP2_450_InsulAll"="New and Retrofit \n2°C",
                "SSP2_450_InsulNew"="New Building \nInsulation - 2°C")

scen_labels3 <-c("SSP2_Baseline"="Effect of Insulation \nin New Buildings",
                 "SSP2_450_InsulNew"="Effect of Retrooofits",
                 "SSP2_450_InsulAll"="Effect of heating & /n cooling technology choices",
                 "SSP2_450_Baseline"="Total")


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
left_axis1 = "Floorspace [bill m^2]"
right_axis1 = "Aggregate U-Value of building envelope[W/m^2/K]"
axis_scale1 = 50

StckUV.BMR <- ggplot() + 
  geom_line(data=subset(DATA.FIG1a, Variable=="FloorspaceTotal"),
            aes(x=Year,y = value/1e9), color="black",size=1, alpha=1) +
  geom_point(data=subset(DATA.FIG1a, Variable=="UEUvalue"),
            aes(x=Year,y = value * axis_scale1, shape=ScenOrder),size=1.0, alpha=0.66) +
  scale_y_continuous(name = left_axis1, 
                     sec.axis = sec_axis(~. * 1/axis_scale1, name = right_axis1))+
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_line(colour="gray80", size = 0.3)) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  scale_shape_manual(values=c(1,8),
                    name="U-Value projections \n(Right axis)",
                    breaks=c("SSP2_Baseline","SSP2_450_Baseline"),
                    labels=c("Baseline","2C")) +
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
  scale_fill_manual(values=c("dodgerblue","firebrick","lightsalmon"),
                    name="Final Energy \n(left axis)",
                    breaks=c("FEHeat","FECool","FEResElecPVHeatCool"),
                    labels=c("Heating ","Cooling","Rooftop Photovoltaic \n(Generation)")) +
  scale_color_manual(values="black",
                     name="",
                     labels="Direct + Indirect \nHeating & Cooling emissions \n(right axis)") +
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
  scale_fill_manual(values=c("deepskyblue","firebrick1"),
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
  geom_ribbon(data=subset(DATA.FIG3, Variable=="FECoolHeat" & !ScenOrder=="SSP2_450_Baseline"), aes(x=Year, ymin=min/1e9,ymax=max/1e9, fill=ScenOrder), alpha="0.5", colour="gray30", size=0.1) +
  geom_ribbon(data=subset(DATA.FIG3, Variable=="EmisCO2DirectHeatCool" & !ScenOrder=="SSP2_450_Baseline"), aes(x=Year, ymin=min/1e12 * axis_scale3,ymax=max/1e12 * axis_scale3, fill=ScenOrder), alpha="0.5", colour="gray30", size=0.1) +
  geom_line(data=subset(DATA.FIG3, Variable=="FECoolHeat" & ScenOrder %in% ScenBase), aes(x=Year,y = value/1e9, color=ScenOrder),size=0.75, alpha=1) +
  geom_line(data=subset(DATA.FIG3, Variable=="EmisCO2DirectHeatCool" & ScenOrder %in% ScenBase), aes(x=Year,y = value/1e12 * axis_scale3, color=ScenOrder),size=0.75, alpha=1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  scale_y_continuous(name = left_axis3, 
                     sec.axis = sec_axis(~. * 1/axis_scale3, name = right_axis3))+
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_line(colour="gray80", size = 0.3)) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  xlab("") + ylab("EJ/yr") +
  scale_colour_manual(values=c("firebrick","forestgreen","forestgreen","forestgreen"),
                    name="",
                    breaks=c("SSP2_Baseline","SSP2_450_InsulNew","SSP2_450_InsulAll","SSP2_450_Baseline"),
                    labels=c("Baseline","x","y","2C")) +
  scale_fill_manual(values=c("green3","darkorchid4","skyblue"),
                    name="",
                    breaks=c("SSP2_Baseline","SSP2_450_InsulNew","SSP2_450_InsulAll"),
                    labels=c("Effect of Insulation \nin New Buildings",
                             "Effect of Retrofits",
                             "Effect of heating & \ncooling technology choices")) +
  facet_grid(Region~Variable, scales="free_y",labeller=labeller(Region=reg_labels, Variable=var_labels)) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
Areas

Emis.decomp <- ggplot(data=DATA.FIG3) + 
  geom_ribbon(data=subset(DATA.FIG3, Variable=="EmisCO2DirectHeatCool" & !ScenOrder=="SSP2_450_Baseline"), aes(x=Year, ymin=min/1e12, ymax=max/1e12, fill=ScenOrder), alpha="0.5", colour="gray30", size=0.1) +
  geom_line(data=subset(DATA.FIG3, Variable=="EmisCO2DirectHeatCool" & ScenOrder %in% ScenBase), aes(x=Year,y = value/1e12, color=ScenOrder),size=0.75, alpha=1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_line(colour="gray80", size = 0.3)) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  xlab("") + ylab(right_axis3) +
  scale_colour_manual(values=c("firebrick","forestgreen","forestgreen","forestgreen"),
                      name="",
                      breaks=c("SSP2_Baseline","SSP2_450_InsulNew","SSP2_450_InsulAll","SSP2_450_Baseline"),
                      labels=c("Baseline","x","y","2C")) +
  scale_fill_manual(values=c("green3","darkorchid4","skyblue"),
                    name="",
                    breaks=c("SSP2_Baseline","SSP2_450_InsulNew","SSP2_450_InsulAll"),
                    labels=c("Effect of Insulation \nin New Buildings",
                             "Effect of Retrofits",
                             "Effect of heating & \ncooling technology choices")) +
  facet_wrap(.~Region, nrow = 2, scales="free_y",labeller=labeller(Region=reg_labels, Variable=var_labels)) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
Emis.decomp

#
# ---- Figure S1: Insulation level Stocks ----
Stck.BMR <- ggplot(data=DATA.FIG1,aes(x=Year,y = value/1e9, fill=InsulLevel)) + 
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
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
  geom_bar(data=subset(DATA.FIG1, Scenario == "SSP2_450_Baseline"),aes(x=Year,y = value/1e9, fill=InsulLevel), stat="identity") +
  geom_point(data=subset(DATA.FIG1a, Variable=="UEUvalue" & Scenario == "SSP2_450_Baseline"),
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
                    labels=c("1","2","3","4","5","6")) +
  facet_wrap(Region~., nrow=2, scales="free_y", labeller=labeller(Region=reg_labels, ScenOrder=scen_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
StckUV.MR

# ---- Figure S2: Intensity ----
UEInt.BMR <- ggplot(data=subset(DATA.UE, Scenario %in% ScenBase & Variable=="UEHeatCoolpfs" & Year %in% ActiveYears & Region %in% RCPRegions)
                    , aes(x=Year,y = Normalised_2020, colour=ScenOrder)) + 
  geom_line(size=1, alpha=1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  xlim(2020,2100) +
  # ylim(0,1.2) +
  xlab("") + ylab("Useful Energy Intensity for Heating and Cooling [kJ/m^2, 2020 = 1]") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_line(colour="gray80", size = 0.3)) + 
  theme(text= element_text(size=FSizeLeg, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_colour_manual(values=c("black","green3"),
                      name="",
                      breaks=c("SSP2_Baseline","SSP2_450_Baseline"),
                      labels=c("Baseline","2°C")) +
  facet_wrap(Region~., nrow=3, labeller=labeller(Region=reg_labels)) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
UEInt.BMR
#

# ---- Figure S3: Energy Independence ----
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
  geom_line(data=subset(DATA.ID, Scenario == "SSP2_450_Baseline" & Year %in% ActiveYears & Region %in% RCPRegions & Demographic %in% ActiveDemog)
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
# # ---- OUTPUTS ----
# write.xlsx(DATA.T1, file="output/BuildStocks/Table1.xlsx", sheetName="Table 1", append=FALSE, row.names=FALSE, showNA = TRUE)
# 
# png(file = "output/BuildStocks/Fig1.png", width = 7*ppi, height = 5*ppi, units = "px", res = ppi)
# plot(StckUV.BMR)
# dev.off()
# 
# png(file = "output/BuildStocks/Fig2.png", width = 7*ppi, height = 8*ppi, units = "px", res = ppi)
# plot(FuelsEmis.BMR)
# dev.off()
# 
# png(file = "output/BuildStocks/Fig3.png", width = 7*ppi, height = 8*ppi, units = "px", res = ppi)
# plot(Areas)
# dev.off()
# 
# png(file = "output/BuildStocks/FigS1.png", width = 6*ppi, height = 8*ppi, units = "px", res = ppi)
# plot(Stck.BMR)
# dev.off()
# 
# png(file = "output/BuildStocks/FigS2.png", width = 7*ppi, height = 8*ppi, units = "px", res = ppi)
# plot(UEInt.BMR)
# dev.off()
# 
# png(file = "output/BuildStocks/FigS3.png", width = 6*ppi, height = 8*ppi, units = "px", res = ppi)
# plot(EnIndep.BMRQ)
# dev.off()
# #
# #
# png(file = "output/BuildStocks/Fig2_Aggregate.png", width = 7*ppi, height = 8*ppi, units = "px", res = ppi)
# plot(FuelsEmis.Aggr)
# dev.off()
# 
# png(file = "output/BuildStocks/Fig2_AggregateGlobal.png", width = 7*ppi, height = 3*ppi, units = "px", res = ppi)
# plot(FuelsEmis.AggrGlob)
# dev.off()
# 
# png(file = "output/BuildStocks/Fig3_Emis.png", width = 7*ppi, height = 5*ppi, units = "px", res = ppi)
# plot(Emis.decomp)
# dev.off()
# 
# png(file = "output/BuildStocks/FigS3_Mitig.png", width = 7*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(EnIndep.MRQ)
# dev.off()
# 
# png(file = "output/BuildStocks/Fig1_InsulLev.png", width = 7*ppi, height = 5*ppi, units = "px", res = ppi)
# plot(StckUV.MR)
# dev.off()
#

# ---- FIG: UE Total ----
UECoolHeat.SV <- ggplot(data=subset(DATA.UE, Scenario %in% ScenInsul & (!Variable=="UEIntHeat") & Year %in% ActiveYears & Region==ActiveRegion)
                        , aes(x=Year,y = Normalised_2020, colour=ScenOrder)) + 
  geom_line(size=1, alpha=1) +
  xlim(2020,2100) +
  xlab("") + ylab("kJ/m^2/HDD, normalised to 2020") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  scale_colour_manual(values=c("black","green3","firebrick", "skyblue"),
                      name="",
                      breaks=c("SSP2_Baseline","SSP2_450_Baseline","SSP2_450_InsulNew","SSP2_450_InsulAll"),
                      labels=c("Baseline","2°C","No Retrofits - 2°C","No Improv. Insul. - 2°C")) +
  facet_grid(Variable~., scales="free_y", labeller=labeller(Region=reg_labels, ScenOrder=scen_labels)) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
UECoolHeat.SV

UECoolHeat.SRV <- ggplot(data=subset(DATA.UE, Scenario %in% ScenInsul & (!Variable=="UEIntHeat") & Year %in% ActiveYears & Region %in% ActiveRegions)
                         , aes(x=Year,y = Normalised_2020, colour=ScenOrder)) + 
  geom_line(size=1, alpha=1) +
  # xlim(2010,2100) +
  xlab("") + ylab("kJ/m^2/HDD, normalised to 2020") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  scale_colour_manual(values=c("black","green3","firebrick", "skyblue"),
                      name="",
                      breaks=c("SSP2_Baseline","SSP2_450_Baseline","SSP2_450_InsulNew","SSP2_450_InsulAll"),
                      labels=c("Baseline","2°C","No Retrofits - 2°C","No Improv. Insul. - 2°C")) +
  facet_grid(Variable~Region, scales="free_y", labeller=labeller(Region=reg_labels, ScenOrder=scen_labels)) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
UECoolHeat.SRV

# 
# ---- FIG: FE Heat ----
FEHeat.S <- ggplot(data=subset(DATA.FE, Scenario %in% ScenStand & Variable=="FEHeat" & Year %in% ActiveYears & Region==ActiveRegion & !(Prim=="Total"))
                   , aes(x=Year,y = value, fill=Prim)) + 
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  xlim(2020,2100) +
  # ylim(0,1.2) +
  xlab("") + ylab("GJ/yr") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeLeg, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_colour_manual(values=c("black","gray","orange","skyblue", "navyblue","forestgreen","purple","bisque","brown"),
                      name="",
                      breaks=c("Coal","ElecResistance","ElecHeatpump","Gas","Hydrogen","ModBio","Oil","SecHeat","TradBio"),
                      labels=c("Coal","Elec-Resistance","Elec-Heatpump","Gas","Hydrogen","ModBio","Oil","SecHeat","TradBio")) +
  facet_grid(.~ScenOrder, scales="free_y", labeller=labeller(Region=reg_labels, ScenOrder=scen_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
FEHeat.S

FEHeat.SR <- ggplot(data=subset(DATA.FE, Scenario %in% ScenStand & Variable=="FEHeat" & Year %in% ActiveYears & Region %in% ActiveRegions & !(Prim=="Total"))
                    , aes(x=Year,y = value/1e9, fill=Prim)) + 
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  xlim(2020,2100) +
  # ylim(0,1.2) +
  xlab("") + ylab("EJ/yr") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeLeg, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_fill_manual(values=c("black","gray","maroon","orange","skyblue", "navyblue","forestgreen","purple","bisque","brown"),
                    name="",
                    breaks=c("Coal","Elec","ElecResistance","ElecHeatpump","Gas","Hydrogen","ModBio","Oil","SecHeat","TradBio"),
                    labels=c("Coal","Elec","Elec-Resistance","Elec-Heatpump","Gas","Hydrogen","ModBio","Oil","SecHeat","TradBio")) +
  facet_grid(Region~ScenOrder, labeller=labeller(Region=reg_labels, ScenOrder=scen_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
FEHeat.SR

#
# ---- FIG: FE Cool ----
FECool.SR <- ggplot(data=subset(DATA.FE, Scenario %in% ScenInsul & Variable=="FECool" & Year %in% ActiveYears & Region %in% ActiveRegions & Prim=="Elec")
                    , aes(x=Year,y = value/1e9, colour=ScenOrder)) + 
  geom_line(size=1, alpha=1) +
  # xlim(2010,2100) +
  xlab("") + ylab("EJ/yr") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  scale_colour_manual(values=c("black","green3","firebrick", "skyblue"),
                      name="",
                      breaks=c("SSP2_Baseline","SSP2_450_Baseline","SSP2_450_InsulNew","SSP2_450_InsulAll"),
                      labels=c("Baseline","2°C","No Retrofits - 2°C","No Improv. Insul. - 2°C")) +
  facet_grid(.~Region, scales="free_y", labeller=labeller(Region=reg_labels, ScenOrder=scen_labels)) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
FECool.SR

# 
# ---- FIG: FE Total ----
FECoolHeat.S <- ggplot(data=subset(DATA.FE, Scenario %in% ScenInsul & Variable=="FECoolHeat" & Year %in% ActiveYears & Region==ActiveRegion & Prim=="Total")
                       , aes(x=Year,y = value/1e9, colour=Scenario)) + 
  geom_line(size=1, alpha=1) +
  # xlim(2010,2100) +
  xlab("") + ylab("EJ/yr") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  scale_colour_manual(values=c("black","green3","firebrick", "skyblue"),
                      name="",
                      breaks=c("SSP2_Baseline","SSP2_450_Baseline","SSP2_450_InsulNew","SSP2_450_InsulAll"),
                      labels=c("Baseline","2°C","No Retrofits - 2°C","No Improv. Insul. - 2°C")) +
  # facet_grid(.~ScenOrder, scales="free_y", labeller=labeller(Region=reg_labels, ScenOrder=scen_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
FECoolHeat.S

FECoolHeat.SR <- ggplot(data=subset(DATA.FE, Scenario %in% ScenInsul & Variable=="FECoolHeat" & Year %in% ActiveYears & Region %in% ActiveRegions & Prim=="Total")
                        , aes(x=Year,y = value/1e9, colour=ScenOrder)) + 
  geom_line(size=1, alpha=1) +
  # xlim(2010,2100) +
  xlab("") + ylab("EJ/yr") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  scale_colour_manual(values=c("black","green3","firebrick", "skyblue"),
                      name="",
                      breaks=c("SSP2_Baseline","SSP2_450_Baseline","SSP2_450_InsulNew","SSP2_450_InsulAll"),
                      labels=c("Baseline","2°C","No Retrofits - 2°C","No Improv. Insul. - 2°C")) +
  facet_grid(.~Region, scales="free_y", labeller=labeller(Region=reg_labels, ScenOrder=scen_labels)) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
FECoolHeat.SR

FECoolHeat.SRC <- ggplot(data=subset(DATA.FE, Scenario %in% ScenStand & Variable=="FECoolHeat" & Year %in% ActiveYears 
                                     & Region %in% ActiveRegions)
                         , aes(x=Year,y = value/1e9, fill=Prim)) + 
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  xlim(2020,2100) +
  # ylim(0,1.2) +
  xlab("") + ylab("EJ/yr") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeLeg, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_fill_manual(values=c("black","gray","maroon","orange","skyblue", "navyblue","forestgreen","purple","bisque","brown"),
                    name="",
                    breaks=c("Coal","Elec","ElecResistance","ElecHeatpump","Gas","Hydrogen","ModBio","Oil","SecHeat","TradBio"),
                    labels=c("Coal","Elec","Elec-Resistance","Elec-Heatpump","Gas","Hydrogen","ModBio","Oil","SecHeat","TradBio")) +
  facet_grid(Region~ScenOrder, labeller=labeller(Region=reg_labels, ScenOrder=scen_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
FECoolHeat.SRC

# 
# ---- FIG: Carbon Contents ----
CC.R <- ggplot(data=subset(DATA.CC, Region %in% ActiveRegions & (Scenario=="SSP2_Baseline"|Scenario=="SSP2_450_Baseline"))
               , aes(x=Year,y = value, colour=ScenOrder)) + 
  geom_line(size=1,alpha=0.8) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  xlim(2020,2100) +
  xlab("") + ylab("kgC/GJ") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_colour_manual(values=c("black","green3"),
                      name="",
                      breaks=c("SSP2_Baseline","SSP2_450_Baseline"),
                      labels=c("Baseline","2°C")) +
  facet_grid(Variable~Region, scales="free_y", labeller=labeller(Region=reg_labels, ScenOrder=scen_labels, Variable=cc_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
CC.R


CC.SRV <- ggplot(data=subset(DATA.CC, Region %in% ActiveRegions & Scenario %in% ScenInsul)
                 , aes(x=Year,y = value, colour=ScenOrder)) + 
  geom_line(size=1,alpha=0.8) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  xlim(2020,2100) +
  xlab("") + ylab("kgC/GJ") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_colour_manual(values=c("black","green3","firebrick", "skyblue"),
                      name="",
                      breaks=c("SSP2_Baseline","SSP2_450_Baseline","SSP2_450_InsulNew","SSP2_450_InsulAll"),
                      labels=c("Baseline","2°C","No Retrofits - 2°C","No Improv. Insul. - 2°C")) +
  facet_grid(Variable~Region, scales="free_y", labeller=labeller(Region=reg_labels, ScenOrder=scen_labels, Variable=cc_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
CC.SRV

#
# ---- FIG: Cost Component ----
# plot_list = list()
# for(i in c(2020,2050,2100)){
#   Costs.Abs <- ggplot(data=subset(CostComponent, (TURQ==1) & 
#                                     Region %in% Regions & 
#                                     (Year==i) &
#                                     (Scen=="SSP2"|Scen=="SSP2_450") &
#                                     (variable=="CapitalNorm"|variable=="HeatFNorm"|variable=="CoolFNorm"))
#                       , aes(x=EffLevel,y = value, fill=variable)) + 
#     geom_bar(stat="identity") +
#     xlab("") + ylab("") +
#     theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
#     theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
#     theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#     theme(legend.position="right") +
#     scale_fill_manual(values=c("gray","firebrick","cornflowerblue"),
#                       name="",
#                       breaks=c("CapitalNorm","HeatFNorm","CoolFNorm"),
#                       labels=c("Capital Costs","Heating Fuel","Cooling Fuel")) +
#     
#     facet_grid(Region~Scen, scales="free_y", labeller=labeller(Region=reg_labels, Scen=scen_labels)) + 
#     theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
#   Costs.Abs
#   plot_list[[i]] = Costs.Abs
# }
# plot_list[[2020]]
# plot_list[[2100]]
# 
# Costs.WEU <- ggplot(data=subset(CostComponent, (TURQ==1) & 
#                                   Region == 11  & 
#                                   (Year==2020|Year==2050|Year==2100) &
#                                   (Scen=="SSP2"|Scen=="SSP2_450") &
#                                   (variable=="CapitalNorm"|variable=="HeatFNorm"|variable=="CoolFNorm"))
#                     , aes(x=EffLevel,y = value, fill=variable)) + 
#   geom_bar(stat="identity") +
#   xlab("Efficiency Level") + ylab("Cost relative to 2020 - Efficiency Level = 1") +
#   theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
#   theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   theme(legend.position="right") +
#   scale_x_continuous(breaks=c(1,2,3,4,5,6)) + 
#   scale_fill_manual(values=c("gray","firebrick","cornflowerblue"),
#                     name="",
#                     breaks=c("CapitalNorm","HeatFNorm","CoolFNorm"),
#                     labels=c("Capital Costs","Heating Fuel","Cooling Fuel")) +
#   facet_grid(Year~Scen, scales="free_y", labeller=labeller(Region=reg_labels, Scen=scen_labels)) + 
#   theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
# Costs.WEU
# 
# #

