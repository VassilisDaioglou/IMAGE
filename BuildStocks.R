# ---- INFORMATION ----
# R script to process the results of the TIMER Building Stocks & Renovation 
# Vassilis Daioglou, November 2019
# 
#  Make appropriate to read IAMC template outputs
#  Vassilis Daioglou, April 2020
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
ActiveYears2 = c("2020","2040","2060","2080","2100")

ActiveRegion <- "World"
ActiveRegions =c("World","BRA","CHN","USA","WEU")
RCPRegions =c("OECD90","REF","ASIA","MAF","LAM","World")

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
           "gray","orange","coral1",
           "black")

data_Baseline <- "data/BuildStocks/BuildingStocks/SSP2_Baseline.xlsx"

data_mitig_Baseline <- "data/BuildStocks/BuildingStocks/SSP2_450_Baseline.xlsx"
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
Baseline = read.xlsx(data_Baseline, sheet = "data")

Baseline_450 = read.xlsx(data_mitig_Baseline, sheet = "data")
# Demand_450 = read.xlsx(data_mitig_Demand, sheet = "data")
# Floorspace_450 = read.xlsx(data_mitig_Floorspace, sheet = "data")
# Full_450 = read.xlsx(data_mitig_Full, sheet = "data")
InsulAll_450 = read.xlsx(data_mitig_InsulAll, sheet = "data")
InsulNew_450 = read.xlsx(data_mitig_InsulNew, sheet = "data")

rm(data_Baseline,
   data_mitig_Baseline,
   # data_mitig_Full,data_mitig_Demand,data_mitig_Floorspace,
   data_mitig_InsulAll,data_mitig_InsulNew)
#
# ---- MUNGING ----
# Create Single Dataset
Baseline = melt(Baseline, id.vars=c("Model","Scenario","Region","Variable","Unit"))

Baseline_450 = melt(Baseline_450, id.vars=c("Model","Scenario","Region","Variable","Unit"))
# Full_450 = melt(Full_450, id.vars=c("Model","Scenario","Region","Variable","Unit"))
# Demand_450 = melt(Demand_450, id.vars=c("Model","Scenario","Region","Variable","Unit"))
# Floorspace_450 = melt(Floorspace_450, id.vars=c("Model","Scenario","Region","Variable","Unit"))
InsulAll_450 = melt(InsulAll_450, id.vars=c("Model","Scenario","Region","Variable","Unit"))
InsulNew_450 = melt(InsulNew_450, id.vars=c("Model","Scenario","Region","Variable","Unit"))

DATA = rbind(Baseline,
             Baseline_450,
             # Full_450,Demand_450,Floorspace_450,
             InsulAll_450,InsulNew_450)
rm(Baseline,Baseline_450,
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
DATA$Variable <- gsub("[[:space:]]","",DATA$Variable,fixed=F)

DATA$Year = as.numeric(substr(DATA$Year, start=1, stop=4))

DATA$ScenOrder = factor(DATA$Scenario, levels =c("SSP2_Baseline",
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

# 
# Identify Variables whose RCP regional data will be FLOORSPACE weighted 
DATA.R3 = subset(DATA, Variable=="InsulAverageRenovRate"|Variable=="UEHeatCoolpfs"|Variable=="UEIntHeat"|Variable=="UEUvalue")


#
# SEPARATE DATASETS
# ---- ***Final Energy*** ----
DATA.FE <- subset(DATA.R1, Variable=="FECoolElec"|Variable=="FEHeatCoal"|Variable=="FEHeatElecResistance"|Variable=="FEHeatElecHeatpump"|Variable=="FEHeatGas"|Variable=="FEHeatHydrogen"
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

# ---- *** Rooftop PV *** ----
DATA.PV <- subset(DATA, Variable=="FEResExportElec"|Variable=="FEResGenerationElec"|Variable=="FEResNetElec"|Variable=="FEResElecPVHeatCool")
DATA.PV$Prim <- "ElecPV"
DATA.PV$PrimOrder  =factor(DATA.PV$Prim, levels = EnergyCarriers)
# ---- ***Carbon Contents*** ----
DATA.CC <- subset(DATA, Variable=="CCElec"|Variable=="CCHeat")

# ---- ***Floorspace*** ----
DATA.FS <- subset(DATA, Variable=="InsulFloorspaceLevel1"|Variable=="InsulFloorspaceLevel2"|Variable=="InsulFloorspaceLevel3"|
                    Variable=="InsulFloorspaceLevel4"|Variable=="InsulFloorspaceLevel5"|Variable=="InsulFloorspaceLevel6")
DATA.FS$InsulLevel <- DATA.FS$Variable
DATA.FS$InsulLevel <- gsub("InsulFloorspaceLevel","",DATA.FS$InsulLevel,fixed=F)
DATA.FS$Variable = substr(DATA.FS$Variable, start=6, stop=15)
DATA.FS <- DATA.FS[c("Scenario","Region","Year","Variable","InsulLevel","Unit","value","ScenOrder")]

# ---- ***Investments*** ----
DATA.INV <- subset(DATA, Variable=="InvInsulRenov"|Variable=="InvInsulTotal")

# ---- ***Useful Energy*** ----
DATA.UE <- subset(DATA, Variable=="UEHeatCoolpc"|Variable=="UEHeatCoolpfs"|Variable=="UEHeatCool"|Variable=="UEIntHeat")
  # Normalise to 2020 value
DATA.UE$ID = paste(DATA.UE$Scenario,DATA.UE$Region,DATA.UE$Variable)
DATA.UE2020 = subset(DATA.UE, Year==2020)
DATA.UE$val_2020 <- DATA.UE2020[match(DATA.UE$ID, DATA.UE2020$ID),"value"]
rm(DATA.UE2020)
DATA.UE = DATA.UE %>% mutate(Normalised_2020 = value/val_2020)
DATA.UE$ID <- NULL
DATA.UE$val_2020 <- NULL

#
# ---- ***UValues*** ----
DATA.UV <- subset(DATA, Variable=="UEUValue")

# ---- ***Emissions*** ----
DATA.EM <- subset(DATA.R1, Variable=="EmisCO2HeatCool"|Variable=="EmisCO2HeatCoolpc")

# ---- MITIGATION EFFECT ON DEMAND ----
# EneEffect = spread(UEHeatCool_pc,Scen,value)
# EneEffect = EneEffect %>% mutate(Tot_Mitig = SSP2 - SSP2_450)
# EneEffect = EneEffect %>% mutate(Eff_Mitig = SSP2_450_NIR - SSP2_450)
# EneEffect = EneEffect %>% mutate(Eff_Frac = Eff_Mitig / Tot_Mitig)
# 
# test = subset(EneEffect, Year==2100 & Region %in% Regions & (variable=="Total"|variable=="Rural"|variable=="Urban"))
# #
# # MinMax dataframe for effect of insulation
# InsulMinMax = subset(DATA.TRQS, (Scen=="SSP2_450"|Scen=="SSP2_450_NIR")&(Variable=="HeatCoolDemand_pc"|Variable=="ResiCO2EmisHeatCool"))
# InsulMinMax = spread(InsulMinMax,Scen,value)
# InsulMinMax$ID = paste(InsulMinMax$Year,InsulMinMax$Region,InsulMinMax$TURQ,InsulMinMax$Variable)
# 
# DATA.TRQS$ID = paste(DATA.TRQS$Year,DATA.TRQS$Region,DATA.TRQS$TURQ,DATA.TRQS$Variable)
# 
# DATA.TRQS$Min <- InsulMinMax[match(DATA.TRQS$ID,InsulMinMax$ID),7]
# DATA.TRQS$Max <- InsulMinMax[match(DATA.TRQS$ID,InsulMinMax$ID),8]

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
               "FECoolHeat"="Final Energy [EJ/yr] \nHeating & Cooling",
               "EmisCO2HeatCool"="Emissions [kgCO2/cap/yr] \nHeating and Cooling")

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
# ---- Figure 1: Fuels & Emissions ----
DATA.FIG1 = rbind(DATA.FE,DATA.PV)
DATA.FIG1 = subset(DATA.FIG1, Scenario %in% ScenBase & Year %in% ActiveYears & Region %in% RCPRegions & !(Prim=="Total"))
DATA.FIG1 = subset(DATA.FIG1, Variable=="FEHeat"|Variable=="FEResElecPVHeatCool"|Variable=="FECool")
DATA.FIG1$value[DATA.FIG1$Variable=="FEResElecPVHeatCool"] <- -1 * DATA.FIG1$value[DATA.FIG1$Variable=="FEResElecPVHeatCool"]

axis_scale = 1/8
left_axis = "Secondary Energy [EJ/yr]"
right_axis = "Heating & Cooling Emissions [MtCO2/yr]"

FuelsEmis.BM <- ggplot() + 
  geom_bar(data=subset(DATA.FIG1, !Prim=="Elec" & (Region==ActiveRegion)), 
           aes(x=Year,y = value/1e9, fill=PrimOrder),alpha=0.66, stat="identity") +
  geom_line(data=subset(DATA.FIG1, Variable=="FECool" & Region==ActiveRegion)
            , aes(x=Year,y = value/1e9, color="CoolingElec"),size=1, alpha=1, linetype="dashed") +
  geom_point(data=subset(DATA.EM, Scenario %in% ScenBase& Year %in% ActiveYears & Region==ActiveRegion & Variable=="EmisCO2HeatCool")
            , aes(x=Year,y = value/10e9 * axis_scale, colour="Emission"),size=3, alpha=1, shape=10, stroke=1.1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  scale_y_continuous(name = left_axis, 
                     sec.axis = sec_axis(~. * 1/axis_scale, name = right_axis))+
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_fill_manual(values=HeatECColour,
                    name="Heating Fuels",
                    breaks=HeatingCarriers,
                    labels=primheat_labels) +
  scale_color_manual(values=c(CoolingElec="gray21",Emission="firebrick"),
                    name="",
                    labels=c("Electricity for Cooling","Heating & Cooling Emissions")) +
  facet_grid(.~ScenOrder, scales="free_y", labeller=labeller(Region=reg_labels, ScenOrder=scen_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
FuelsEmis.BM

FuelsEmis.BMR <- ggplot() + 
  geom_bar(data=subset(DATA.FIG1, !(Prim=="Elec") & Region %in% RCPRegions), 
           aes(x=Year,y = value/1e9, fill=PrimOrder),alpha=0.66, stat="identity") +
  geom_line(data=subset(DATA.FIG1, Variable=="FECool" & Region %in% RCPRegions),
            aes(x=Year,y = value/1e9, color="CoolingElec"),size=1, alpha=1, linetype="dashed") +
  geom_point(data=subset(DATA.EM, Scenario %in% ScenBase& Year %in% ActiveYears & Region %in% RCPRegions & Variable=="EmisCO2HeatCool")
             , aes(x=Year,y = value/10e9 * axis_scale, colour="Emission"),size=3, alpha=1, shape=10, stroke=1.1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  scale_y_continuous(name = left_axis, 
                     sec.axis = sec_axis(~. * 1/axis_scale, name = right_axis))+
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_fill_manual(values=HeatECColour,
                    name="Heating Fuels",
                    breaks=HeatingCarriers,
                    labels=primheat_labels) +
  scale_color_manual(values=c(CoolingElec="gray21",Emission="firebrick"),
                     name="",
                     labels=c("Electricity for Cooling","Heating & Cooling Emissions")) +
  facet_grid(Region~ScenOrder, scales="free_y", labeller=labeller(Region=reg_labels, ScenOrder=scen_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
FuelsEmis.BMR

# 
# ---- Figure 2: Decomposition ----
FEDecomp.BM <- ggplot() + 
  geom_bar(data=subset(DATA.FE, Scenario %in% ScenInsul & Variable=="FECoolHeat" & Year %in% ActiveYears2 & Region==ActiveRegion & Prim=="Total")
           , aes(x=as.character(Year), y = value/1e9, fill=ScenOrder),position="dodge", stat="identity") +
  geom_bar(data=subset(DATA.EM, Scenario %in% ScenInsul & Year %in% ActiveYears2 & Region==ActiveRegion)
           , aes(x=as.character(Year), y = value, fill=ScenOrder),position="dodge", stat="identity") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  scale_fill_manual(values=c("gray","firebrick", "skyblue","green3"),
                      name="",
                      breaks=ScenInsul,
                      labels=scen_labels2) +
  facet_wrap(.~Variable, scales="free_y", labeller=labeller(Variable=var_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
FEDecomp.BM

#
# ---- FIG: Stocks ----
Stck.S <- ggplot(data=subset(DATA.FS, Scenario %in% ScenStand & Region %in% ActiveRegion & Year %in% ActiveYears), 
                 aes(x=Year,y = value/1e9, fill=InsulLevel)) + 
  geom_bar(stat="identity") +
  # xlim(2010,2100) +
  xlab("") + ylab("bill. m^2") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  scale_fill_manual(values=c("firebrick","chocolate1","yellow","cornflowerblue","chartreuse","forestgreen"),
                      name="",
                      breaks=c("1","2","3","4","5","6"),
                      labels=c("1","2","3","4","5","6")) +
  facet_grid(.~ScenOrder, scales="free_y", labeller=labeller(Region=reg_labels, ScenOrder=scen_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
Stck.S

Stck.R <- ggplot(data=subset(DATA.FS, Scenario=="SSP2_Baseline" & Year %in% ActiveYears), aes(x=Year,y = value/1e9, fill=InsulLevel)) + 
  geom_bar(stat="identity") +
  # xlim(2010,2100) +
  xlab("") + ylab("bill. m^2") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  scale_fill_manual(values=c("firebrick","chocolate1","yellow","cornflowerblue","chartreuse","forestgreen"),
                    name="",
                    breaks=c("1","2","3","4","5","6"),
                    labels=c("1","2","3","4","5","6")) +
  facet_wrap(Region~., nrow=6, scales="free_y", labeller=labeller(Region=reg_labels, Scenario=scen_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
Stck.R

#
# ---- FIG: UE Intensity ----
UEInt.S <- ggplot(data=subset(DATA.UE, Scenario %in% ScenInsul & Variable=="UEIntHeat" & Year %in% ActiveYears & Region==ActiveRegion)
                , aes(x=Year,y = Normalised_2020, colour=ScenOrder)) + 
  geom_line(size=1, alpha=1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  xlim(2020,2100) +
  # ylim(0,1.2) +
  xlab("") + ylab("kJ/m^2/HDD, normalised to 2020") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeLeg, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_colour_manual(values=c("black","green3","firebrick", "skyblue"),
                      name="",
                      breaks=c("SSP2_Baseline","SSP2_450_Baseline","SSP2_450_InsulNew","SSP2_450_InsulAll"),
                      labels=c("Baseline","2°C","No Retrofits - 2°C","No Improv. Insul. - 2°C")) +
  # facet_wrap(Region~., nrow=3) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
UEInt.S
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
FECool.S <- ggplot(data=subset(DATA.FE, Scenario %in% ScenInsul & Variable=="FECool" & Year %in% ActiveYears & Region==ActiveRegion & Prim=="Elec")
                   , aes(x=Year,y = value/1e9, colour=Prim)) + 
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
FECool.S

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
# ---- FIG: Emissions ----
Emis.S <- ggplot(data=subset(DATA.EM, Scenario %in% ScenInsul & Year %in% ActiveYears & Region==ActiveRegion)
                  , aes(x=Year,y = value, colour=ScenOrder)) + 
  geom_line(size=1, alpha=1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  xlim(2020,2100) +
  # ylim(0,1.2) +
  xlab("") + ylab("kcCO2/cap") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeLeg, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_colour_manual(values=c("black","green3","firebrick", "skyblue"),
                      name="",
                      breaks=c("SSP2_Baseline","SSP2_450_Baseline","SSP2_450_InsulNew","SSP2_450_InsulAll"),
                      labels=c("Baseline","2°C","No Retrofits - 2°C","No Improv. Insul. - 2°C")) +
  # facet_wrap(Region~., nrow=3) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
Emis.S

Emis.SR <- ggplot(data=subset(DATA.EM, Scenario %in% ScenInsul & Year %in% ActiveYears & Region %in% ActiveRegions)
                 , aes(x=Year,y = value, colour=ScenOrder)) + 
  geom_line(size=1, alpha=1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  xlim(2020,2100) +
  # ylim(0,1.2) +
  xlab("") + ylab("kcCO2/cap") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeLeg, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_colour_manual(values=c("black","green3","firebrick", "skyblue"),
                      name="",
                      breaks=c("SSP2_Baseline","SSP2_450_Baseline","SSP2_450_InsulNew","SSP2_450_InsulAll"),
                      labels=c("Baseline","2°C","No Retrofits - 2°C","No Improv. Insul. - 2°C")) +
  facet_wrap(Region~., nrow=1) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
Emis.SR

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
# ---- FIG: Combined Results ----
BaseEffect <- ggplot(data=subset(DATA.TRQS, TURQ=="Total" &
                                  !(Variable=="RenovationRate") &
                                  (Scen=="SSP2"))
                    , aes(x=Year,y = value, colour=Scen)) + 
  geom_line(size=1,alpha=1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  xlim(2010,2100) +
  xlab("") + ylab("") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeStrip, hjust=1), axis.text.y = element_text(size=FSizeStrip)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  scale_colour_manual(values=c("navy","forestgreen","gray"),
                      name="",
                      breaks=c("SSP2","SSP2_450","SSP2_450_NIR"),
                      labels=c("Baseline","Mitig.","Mitig No Improv. Insul.")) +
  facet_grid(Var_Order~Reg_Order, scales="free_y",labeller=labeller(Reg_Order=reg_labels, Scen=scen_labels, Var_Order=var_labels)) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
BaseEffect

MitigEffect <- ggplot(data=subset(DATA.TRQS, TURQ=="Total" &
                                   !(Variable=="RenovationRate") &
                                   (Scen=="SSP2"|Scen=="SSP2_450"))
                     , aes(x=Year,y = value, colour=Scen)) + 
  geom_line(size=1,alpha=1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  xlim(2010,2100) +
  xlab("") + ylab("") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeStrip, hjust=1), axis.text.y = element_text(size=FSizeStrip)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  scale_colour_manual(values=c("navy","green3","gray"),
                      name="",
                      breaks=c("SSP2","SSP2_450","SSP2_450_NIR"),
                      labels=c("Baseline","Mitig.","Mitig No Improv. Insul.")) +
  facet_grid(Var_Order~Reg_Order, scales="free_y",labeller=labeller(Reg_Order=reg_labels, Scen=scen_labels, Var_Order=var_labels)) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
MitigEffect

AllEffect1 <- ggplot(data=subset(DATA.TRQS, TURQ=="Total" &
                                  !(Variable=="RenovationRate") &
                                  (Scen=="SSP2"|Scen=="SSP2_450"|Scen=="SSP2_450_NIR") & 
                                  !(Scen=="SSP2_450_NIR"&Variable=="UeIntHeat"))
                    , aes(x=Year,y = value, colour=Scen)) + 
  geom_ribbon(aes(ymin=Min,ymax=Max, fill=Variable, colour= NA), alpha="0.5") +
  geom_line(size=1,alpha=1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  xlim(2010,2100) +
  xlab("") + ylab("") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeStrip, hjust=1), axis.text.y = element_text(size=FSizeStrip)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  scale_colour_manual(values=c("navy","green3","red"),
                      name="",
                      breaks=c("SSP2","SSP2_450","SSP2_450_NIR"),
                      labels=c("Baseline","Mitig.","Mitig No Improv. Insul.")) +
  scale_fill_manual(values=c("azure3","azure3","azure3"),
                      name="",
                      breaks=c("UeIntHeat","HeatCoolDemand_pc","ResiCO2EmisHeatCool"),
                      labels=c("","",""), guide=FALSE) +
  facet_grid(Var_Order~Reg_Order, scales="free_y",labeller=labeller(Reg_Order=reg_labels, Scen=scen_labels, Var_Order=var_labels)) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
AllEffect1

AllEffect2 <- ggplot(data=subset(DATA.TRQS, TURQ=="Total" &
                                   !(Variable=="RenovationRate") &
                                   (Scen=="SSP2"|Scen=="SSP2_450"|Scen=="SSP2_450_NIR") & 
                                   !(Scen=="SSP2_450_NIR"&Variable=="UeIntHeat") &
                                   !(Scen=="SSP2_450_NIR"&Variable=="ResiCO2EmisHeatCool"))
                     , aes(x=Year,y = value, colour=Scen)) + 
  geom_ribbon(aes(ymin=Min,ymax=Max, fill=Variable, colour= NA), alpha="0.5") +
  geom_line(size=1,alpha=1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  xlim(2010,2100) +
  xlab("") + ylab("") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeStrip, hjust=1), axis.text.y = element_text(size=FSizeStrip)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  scale_colour_manual(values=c("navy","green3","red"),
                      name="",
                      breaks=c("SSP2","SSP2_450","SSP2_450_NIR"),
                      labels=c("Baseline","Mitig.","Mitig No Improv. Insul.")) +
  scale_fill_manual(values=c("azure3","azure3","azure3"),
                    name="",
                    breaks=c("UeIntHeat","HeatCoolDemand_pc","ResiCO2EmisHeatCool"),
                    labels=c("","",""), guide=FALSE) +
  facet_grid(Var_Order~Reg_Order, scales="free_y",labeller=labeller(Reg_Order=reg_labels, Scen=scen_labels, Var_Order=var_labels)) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
AllEffect2

#
# # ---- OUTPUTS ----
# png(file = "output/BuildStocks/Fig1.png", width = 8*ppi, height = 3.5*ppi, units = "px", res = ppi)
# plot(FuelsEmis.BM)
# dev.off()
# #
# png(file = "output/BuildStocks/Fig1_R.png", width = 7*ppi, height = 8*ppi, units = "px", res = ppi)
# plot(FuelsEmis.BMR)
# dev.off()
# 
# png(file = "output/BuildStocks/Stocks_R.png", width = 8*ppi, height = 8*ppi, units = "px", res = ppi)
# plot(Stck.R)
# dev.off()
# # #
# png(file = "output/BuildStocks/Stocks_S.png", width = 8*ppi, height = 4*ppi, units = "px", res = ppi)
# plot(Stck.S)
# dev.off()
# #
# png(file = "output/BuildStocks/UEInt_S.png", width = 7*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(UEInt.S)
# dev.off()
# 
# png(file = "output/BuildStocks/FEHeat_SR.png", width = 8*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(FEHeat.SR)
# dev.off()
# 
# png(file = "output/BuildStocks/FECool_SR.png", width = 8*ppi, height = 4*ppi, units = "px", res = ppi)
# plot(FECool.SR)
# dev.off()
# 
# png(file = "output/BuildStocks/UECoolHeat_SRV.png", width = 8*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(UECoolHeat.SRV)
# dev.off()
# 
# png(file = "output/BuildStocks/CarbonContent_SRV.png", width = 8*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(CC.SRV)
# dev.off()
# 




#
# ---- FIG: Renovation Rate ----
# RR.T <- ggplot(data=subset(RenovRate, variable=="Total")
#                , aes(x=Region,y = value, fill=Region)) + 
#   geom_bar(stat="identity") +
#   xlab("") + ylab("") +
#   theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
#   theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeStrip, hjust=1), axis.text.y = element_text(size=FSizeStrip)) +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   theme(legend.position="bottom") +
#   # scale_colour_manual(values=c("forestgreen","dodgerblue","firebrick1"),
#   #                     name="",
#   #                     breaks=c("New","Decomissioned","Total"),
#   #                     labels=c("New","Decomissioned","Total")) +
#   # 
#   facet_grid(.~Scen)
# RR.T

# png(file = "output/BuildStocks/RenovRate.png", width = 6*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(RR.T)
# dev.off()
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

