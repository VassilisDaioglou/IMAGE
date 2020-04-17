# R script to process the results of the TIMER Building Stocks & Renovation represetnation
# Vassilis Daioglou, November 2019
# 
#  Make appropriate to read IAMC template outputs
#  Vassilis Daioglou, April 2020
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
            "OCE","RCAM","RSAF","RSAM","RSAS","RUS","SAF","SEAS","STAN","TUR","UKR","USA","WAF","WEU","World")

Years = c("2005","2010","2015","2020","2025","2030","2035","2040","2045",
          "2050","2060","2070","2080","2090","2100")

ActiveRegion <- "World"

Scenarios = c("Full","none","Demand","Floorspace","NoEffImp","NoRetrofit")

data_full <- "data/BuildStocks/BuildingStocks/Full.xlsx"
data_none <- "data/BuildStocks/BuildingStocks/none.xlsx"
data_Demand <- "data/BuildStocks/BuildingStocks/Demand.xlsx"
data_Floorspace <- "data/BuildStocks/BuildingStocks/Floorspace.xlsx"
data_NoEffImp <- "data/BuildStocks/BuildingStocks/NoEffImp.xlsx"
data_NoRetrofit <- "data/BuildStocks/BuildingStocks/NoRetrofit.xlsx"

# set higher RAM capacity for java (used in clsx package)
options(java.parameters = "-Xmx8000m")

# ---- INPUTS: Data ----
  # set directory path 
setwd("C:/Users/Asus/Documents/Github/IMAGE/")
  # Read Data Files for Baseline Scenario
full = read.xlsx(data_full, sheet = "data")
none = read.xlsx(data_none, sheet = "data")
Demand = read.xlsx(data_Demand, sheet = "data")
Floorspace = read.xlsx(data_Floorspace, sheet = "data")
NoEffImp = read.xlsx(data_NoEffImp, sheet = "data")
NoRetrofit = read.xlsx(data_NoRetrofit, sheet = "data")

#
# ---- MUNGING ----
# ---- ***Stocks ----
Stocks$Scen <- "SSP2"
Stocks.450$Scen <- "SSP2_450"
Stocks.450_NR$Scen <- "SSP2_450_NR"
Stocks.450_NIR$Scen <- "SSP2_450_NIR"
Stocks = rbind(Stocks,Stocks.450,Stocks.450_NR,Stocks.450_NIR)
rm(Stocks.450,Stocks.450_NR,Stocks.450_NIR)

colnames(Stocks)[1:9] <- c("Year","Region","TURQ",
                           "1","2","3","4","5","6")
Stocks = melt(Stocks, id.vars=c("Year","Region","TURQ","Scen"))
colnames(Stocks)[5] <- "EffLevel" 
Stocks$value <- Stocks$value / 1e9
#
# ---- ***Insulation Market Shares ----
InsulMS$Scen <- "SSP2"
InsulMS.450$Scen <- "SSP2_450"
InsulMS.450_NR$Scen <- "SSP2_450_NR"
InsulMS.450_NIR$Scen <- "SSP2_450_NIR"
InsulMS = rbind(InsulMS,InsulMS.450,InsulMS.450_NR,InsulMS.450_NIR)
rm(InsulMS.450,InsulMS.450_NR,InsulMS.450_NIR)

colnames(InsulMS)[1:9] <- c("Year","Region","TURQ",
                           "1","2","3","4","5","6")
InsulMS = melt(InsulMS, id.vars=c("Year","Region","TURQ","Scen"))
colnames(InsulMS)[5] <- "EffLevel" 
#
# ---- ***Renovation Rates ----
  # Renovation Rate (Annual)
RenovRate$Scen <- "SSP2"
RenovRate.450$Scen <- "SSP2_450"
RenovRate.450_NR$Scen <- "SSP2_450_NR"
RenovRate.450_NIR$Scen <- "SSP2_450_NIR"
RenovRate = rbind(RenovRate,RenovRate.450,RenovRate.450_NR,RenovRate.450_NIR)
rm(RenovRate.450, RenovRate.450_NR, RenovRate.450_NIR)

colnames(RenovRate)[1:15] <- c("Year","Region","Total","Urban","Rural",
                               "U1","U2","U3","U4","U5",
                               "R1","R2","R3","R4","R5")
RenovRate = melt(RenovRate, id.vars=c("Year","Region","Scen"))
RenovRate$value <- RenovRate$value * 100
#
  # Total Renovation Rate
RenovRate_Ave$Scen <- "SSP2"
RenovRate_Ave.450$Scen <- "SSP2_450"
RenovRate_Ave.450_NR$Scen <- "SSP2_450_NR"
RenovRate_Ave.450_NIR$Scen <- "SSP2_450_NIR"
RenovRate_Ave = rbind(RenovRate_Ave,RenovRate_Ave.450,RenovRate_Ave.450_NR,RenovRate_Ave.450_NIR)
rm(RenovRate_Ave.450, RenovRate_Ave.450_NR,RenovRate_Ave.450_NIR)

colnames(RenovRate_Ave)[1:15] <- c("Year","Region","Total","Urban","Rural",
                               "U1","U2","U3","U4","U5",
                               "R1","R2","R3","R4","R5")
RenovRate_Ave = melt(RenovRate_Ave, id.vars=c("Year","Region","Scen"))
RenovRate_Ave = subset(RenovRate_Ave, Year==2100)
RenovRate_Ave$value <- RenovRate_Ave$value * 100
RenovRate_Ave = spread(RenovRate_Ave,Scen,value)
RenovRate_Ave = RenovRate_Ave %>% mutate (MitigEffect = SSP2_450-SSP2)
# ---- ***Heating Useful Energy Intensity ----
UEIntHeat$Scen <- "SSP2"
UEIntHeat.450$Scen <- "SSP2_450"
UEIntHeat.450_NR$Scen <- "SSP2_450_NR"
UEIntHeat.450_NIR$Scen <- "SSP2_450_NIR"
UEIntHeat = rbind(UEIntHeat,UEIntHeat.450,UEIntHeat.450_NR,UEIntHeat.450_NIR)
rm(UEIntHeat.450,UEIntHeat.450_NR,UEIntHeat.450_NIR)

colnames(UEIntHeat)[1:15] <- c("Year","Region","Total","Urban","Rural",
                               "U1","U2","U3","U4","U5",
                               "R1","R2","R3","R4","R5")
UEIntHeat = melt(UEIntHeat, id.vars=c("Year","Region","Scen"))

  # Normalise to 2020 value
UEIntHeat$ID <- paste(UEIntHeat$Region,UEIntHeat$Scen,UEIntHeat$variable)
UEIntHeat.2010 = subset(UEIntHeat, Year==2010)
UEIntHeat$val_2010 <- UEIntHeat.2010[match(UEIntHeat$ID, UEIntHeat.2010$ID),"value"]
rm(UEIntHeat.2010)
UEIntHeat = UEIntHeat %>% mutate(Normalised_2010 = value/val_2010)

UEIntHeat = subset(UEIntHeat, select=-c(value,ID,val_2010))
colnames(UEIntHeat)[5] <- "value"

#
# ---- ***Heating+Cooling Energy USe ----
UEHeatCool_pc$Scen <- "SSP2"
UEHeatCool_pc.450$Scen <- "SSP2_450"
UEHeatCool_pc.450_NR$Scen <- "SSP2_450_NR"
UEHeatCool_pc.450_NIR$Scen <- "SSP2_450_NIR"
UEHeatCool_pc = rbind(UEHeatCool_pc,UEHeatCool_pc.450,UEHeatCool_pc.450_NR,UEHeatCool_pc.450_NIR)
rm(UEHeatCool_pc.450,UEHeatCool_pc.450_NR,UEHeatCool_pc.450_NIR)

colnames(UEHeatCool_pc)[1:15] <- c("Year","Region","Total","Urban","Rural",
                                "U1","U2","U3","U4","U5",
                                "R1","R2","R3","R4","R5")

UEHeatCool_pc = melt(UEHeatCool_pc, id.vars=c("Year","Region","Scen"))

  # Normalise to 2010 value
UEHeatCool_pc$ID <- paste(UEHeatCool_pc$Region,UEHeatCool_pc$Scen,UEHeatCool_pc$variable)
UEHeatCool_pc.2010 = subset(UEHeatCool_pc, Year==2010)
UEHeatCool_pc$val_2010 <- UEHeatCool_pc.2010[match(UEHeatCool_pc$ID, UEHeatCool_pc.2010$ID),"value"]
rm(UEHeatCool_pc.2010)
UEHeatCool_pc = UEHeatCool_pc %>% mutate(Normalised_2010 = value/val_2010)

UEHeatCool_pc = subset(UEHeatCool_pc, select=-c(value,ID,val_2010))
colnames(UEHeatCool_pc)[5] <- "value"

#
# ---- ***CO2 Emissions ----
CO2EmisHeatCool_pc$Scen <- "SSP2"
CO2EmisHeatCool_pc.450$Scen <- "SSP2_450"
CO2EmisHeatCool_pc.450_NR$Scen <- "SSP2_450_NR"
CO2EmisHeatCool_pc.450_NIR$Scen <- "SSP2_450_NIR"
CO2EmisHeatCool_pc = rbind(CO2EmisHeatCool_pc,CO2EmisHeatCool_pc.450,CO2EmisHeatCool_pc.450_NR,CO2EmisHeatCool_pc.450_NIR)
rm(CO2EmisHeatCool_pc.450,CO2EmisHeatCool_pc.450_NR,CO2EmisHeatCool_pc.450_NIR)

colnames(CO2EmisHeatCool_pc)[1:28] <- c("Year",1,2,3,4,5,6,7,8,9,10,
                                        11,12,13,14,15,16,17,18,19,20,
                                        21,22,23,24,25,26,27)
CO2EmisHeatCool_pc = melt(CO2EmisHeatCool_pc, id.vars=c("Year","Scen"))
colnames(CO2EmisHeatCool_pc)[3] <- c("Region")
CO2EmisHeatCool_pc$variable <- "Total"

  # Normalise to 2020 value
CO2EmisHeatCool_pc$ID <- paste(CO2EmisHeatCool_pc$Region,CO2EmisHeatCool_pc$Scen,CO2EmisHeatCool_pc$variable)
CO2EmisHeatCool_pc.2010 = subset(CO2EmisHeatCool_pc, Year==2010)
CO2EmisHeatCool_pc$val_2010 <- CO2EmisHeatCool_pc.2010[match(CO2EmisHeatCool_pc$ID, CO2EmisHeatCool_pc.2010$ID),"value"]
rm(CO2EmisHeatCool_pc.2010)
CO2EmisHeatCool_pc = CO2EmisHeatCool_pc %>% mutate(Normalised_2010 = value/val_2010)

CO2EmisHeatCool_pc = subset(CO2EmisHeatCool_pc, select=-c(value,ID,val_2010))
colnames(CO2EmisHeatCool_pc)[5] <- "value"

#
# ---- ***Emission Factors ----
  # Electricity
CCElec$Scen <- "SSP2"
CCElec.450$Scen <- "SSP2_450"
CCElec = rbind(CCElec,CCElec.450)
rm(CCElec.450)

colnames(CCElec)[1:28] <- c("Year",1,2,3,4,5,6,7,8,9,10,
                                        11,12,13,14,15,16,17,18,19,20,
                                        21,22,23,24,25,26,27)
CCElec = melt(CCElec, id.vars=c("Year","Scen"))
colnames(CCElec)[3] <- c("Region")
CCElec$variable <- "CCElec"

  # Space Heating
CCSpaceHeat$Scen <- "SSP2"
CCSpaceHeat.450$Scen <- "SSP2_450"
CCSpaceHeat.450_NIR$Scen <- "SSP2_450_NIR"
CCSpaceHeat = rbind(CCSpaceHeat,CCSpaceHeat.450,CCSpaceHeat.450_NIR)
rm(CCSpaceHeat.450,CCSpaceHeat.450_NIR)

colnames(CCSpaceHeat)[1:3] <- c("Year","Region","Total")
CCSpaceHeat = subset(CCSpaceHeat, select = c(Year,Region,Total,Scen))
colnames(CCSpaceHeat)[3] <- "value"
CCSpaceHeat$variable <- "CCSpaceHeat"

  # Combined
CC = rbind(CCElec,CCSpaceHeat)
rm(CCElec,CCSpaceHeat)
#
# ---- ***Cost Components ----
CostComponent$Scen <- "SSP2"
CostComponent.450$Scen <- "SSP2_450"
CostComponent.450_NR$Scen <- "SSP2_450_NR"
CostComponent.450_NIR$Scen <- "SSP2_450_NIR"
CostComponent$class_.4 <-NULL
CostComponent.450$class_.4 <-NULL
CostComponent.450_NR$class_.4 <-NULL
CostComponent.450_NIR$class_.4 <-NULL

CostComponent = rbind(CostComponent,CostComponent.450,CostComponent.450_NR,CostComponent.450_NIR)
rm(CostComponent.450,CostComponent.450_NR,CostComponent.450_NIR)

colnames(CostComponent)[1:8] <- c("Year","Region","TURQ","EffLevel",
                                   "Capital","HeatingFuel","CoolingFuel","Scen")
CostComponent = melt(CostComponent, id.vars=c("Year","Region","TURQ","Scen","EffLevel"))

  # Make relative to 2020 total of EffLevel = 1
CostComponent.2020 = subset(CostComponent, Year==2020&EffLevel==1)
CostComponent.2020 = spread(CostComponent.2020,variable,value)
CostComponent.2020 = CostComponent.2020 %>% mutate(Total=Capital+HeatingFuel+CoolingFuel)
CostComponent.2020$ID = paste(CostComponent.2020$Region,"-",CostComponent.2020$TURQ,CostComponent.2020$Scen)

CostComponent = spread(CostComponent,variable,value)
CostComponent$ID = paste(CostComponent$Region,"-",CostComponent$TURQ,CostComponent$Scen)
CostComponent$Tot2020 <- CostComponent.2020[match(CostComponent$ID,CostComponent.2020$ID),9]
CostComponent$ID <- NULL
rm(CostComponent.2020)

CostComponent = CostComponent %>% mutate(CapitalNorm = Capital / Tot2020)
CostComponent = CostComponent %>% mutate(HeatFNorm = HeatingFuel / Tot2020)
CostComponent = CostComponent %>% mutate(CoolFNorm = CoolingFuel / Tot2020)
CostComponent$Tot2020 <- NULL
CostComponent = melt(CostComponent, id.vars=c("Year","Region","TURQ","Scen","EffLevel"))
#
# ---- DATA AGGREGATION ----
Stocks=subset(Stocks, Year %in% Years)
InsulMS=subset(InsulMS, Year %in% Years)
RenovRate=subset(RenovRate, Year %in% Years)
UEIntHeat=subset(UEIntHeat, Year %in% Years)
UEHeatCool_pc=subset(UEHeatCool_pc, Year %in% Years)
CO2EmisHeatCool_pc=subset(CO2EmisHeatCool_pc, Year %in% Years)
CostComponent=subset(CostComponent, Year %in% Years)

RenovRate$Variable <- "RenovationRate"
UEIntHeat$Variable <- "UeIntHeat"
UEHeatCool_pc$Variable <- "HeatCoolDemand_pc"
CO2EmisHeatCool_pc$Variable <- "ResiCO2EmisHeatCool"

DATA.TRQS = rbind(RenovRate,UEIntHeat,UEHeatCool_pc,CO2EmisHeatCool_pc) 
colnames(DATA.TRQS)[4] <- "TURQ"

DATA.TRQS$Var_Order <- factor(DATA.TRQS$Variable, level=c("RenovationRate",
                                                          "UeIntHeat",
                                                          "HeatCoolDemand_pc",
                                                          "ResiCO2EmisHeatCool"))

DATA.TRQS$Reg_Order <- factor(DATA.TRQS$Region, level=c("2",
                                                          "5",
                                                          "10",
                                                          "11",
                                                          "16",
                                                          "18",
                                                          "20",
                                                          "27"))

DATA.TS = subset(DATA.TRQS, Region == 27&TURQ == "Total")
DATA.TS$TURQ <- NULL
DATA.TRQS = subset(DATA.TRQS, Region %in% Regions)

#
# ---- EFFECT ON DEMAND ----
EneEffect = spread(UEHeatCool_pc,Scen,value)
EneEffect = EneEffect %>% mutate(Tot_Mitig = SSP2 - SSP2_450)
EneEffect = EneEffect %>% mutate(Eff_Mitig = SSP2_450_NIR - SSP2_450)
EneEffect = EneEffect %>% mutate(Eff_Frac = Eff_Mitig / Tot_Mitig)

test = subset(EneEffect, Year==2100 & Region %in% Regions & (variable=="Total"|variable=="Rural"|variable=="Urban"))
#
# MinMax dataframe for effect of insulation
InsulMinMax = subset(DATA.TRQS, (Scen=="SSP2_450"|Scen=="SSP2_450_NIR")&(Variable=="HeatCoolDemand_pc"|Variable=="ResiCO2EmisHeatCool"))
InsulMinMax = spread(InsulMinMax,Scen,value)
InsulMinMax$ID = paste(InsulMinMax$Year,InsulMinMax$Region,InsulMinMax$TURQ,InsulMinMax$Variable)

DATA.TRQS$ID = paste(DATA.TRQS$Year,DATA.TRQS$Region,DATA.TRQS$TURQ,DATA.TRQS$Variable)

DATA.TRQS$Min <- InsulMinMax[match(DATA.TRQS$ID,InsulMinMax$ID),7]
DATA.TRQS$Max <- InsulMinMax[match(DATA.TRQS$ID,InsulMinMax$ID),8]

# ---- LABELS ----
scen_labels <-c("SSP1"="SSP1",
                "SSP2"="Baseline",
                "SSP3"="SSP3",
                "SSP1_450"="SSP1 - 2°C",
                "SSP2_450"="Mitig.",
                "SSP1_20"="SSP1 - 1.5°C",
                "SSP2_20"="SSP2 - 1.5°C",
                "SSP2_450_NR"="Mitig. No Renov.",
                "SSP2_450_NFS"="Mitig. No Fuel Switching",
                "SSP2_450_NIR"="Mitig. No Insulation Improv.")

reg_labels <-c("2"="USA",
               "5"="Brazil",
               "10"="S. Africa",
               "11"="W. Europe",
               "16"="Russia",
               "18"="India",
               "20"="China",
               "27"="World")

var_labels <-c("RenovationRate"="Renovation \nRate \n(%)",
               "UeIntHeat"="Heating & Cooling \nIntensity (desired) \n(2010=1)",
               "HeatCoolDemand_pc"="Heating & Cooling \nDemand \n(2010=1)",
               "ResiCO2EmisHeatCool"="Heating & Cooling \nEmissions \n(2010=1)")
turq_labels <-c("1"="Total","2"="Urban","3"="Rural")

cc_labels <-c("CCElec"="Electricity","CCSpaceHeat"="Heating Fuels")

# ---- FIGURES ----
# ---- FIG: Stocks ----
Stck.TR <- ggplot(data=subset(Stocks, (TURQ==1)& Region %in% Regions & !(Region==27) & (Scen=="SSP2"|Scen=="SSP2_450"))
                 , aes(x=Year,y = value, fill=EffLevel)) + 
  geom_bar(stat="identity") +
  xlim(2010,2100) +
  xlab("") + ylab("mill. m^2") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="none") +
  scale_fill_manual(values=c("firebrick","chocolate1","yellow","cornflowerblue","chartreuse","forestgreen"),
                      name="",
                      breaks=c("1","2","3","4","5","6"),
                      labels=c("1","2","3","4","5","6")) +
  facet_grid(Scen~Region, scales="free_y", labeller=labeller(Region=reg_labels, Scen=scen_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
Stck.TR

Stck.TG <- ggplot(data=subset(Stocks, (TURQ==1)& Region %in% Regions & (Region==27) & (Scen=="SSP2"|Scen=="SSP2_450"))
                  , aes(x=Year,y = value, fill=EffLevel)) + 
  geom_bar(stat="identity") +
  xlim(2010,2100) +
  xlab("") + ylab("") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_fill_manual(values=c("firebrick","chocolate1","yellow","cornflowerblue","chartreuse","forestgreen"),
                    name="",
                    breaks=c("1","2","3","4","5","6"),
                    labels=c("1","2","3","4","5","6")) +
  facet_grid(Scen~Region, scales="free_y", labeller=labeller(Region=reg_labels, Scen=scen_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
Stck.TG

Stck.GUR <- ggplot(data=subset(Stocks, (TURQ==2|TURQ==3) & Region==27 & (Scen=="SSP2"|Scen=="SSP2_450"))
                 , aes(x=Year,y = value, fill=EffLevel)) + 
  geom_bar(stat="identity") +
  xlim(2010,2100) +
  xlab("") + ylab("mill. m^2") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_fill_manual(values=c("firebrick","chocolate1","yellow","cornflowerblue","chartreuse","forestgreen"),
                    name="",
                    breaks=c("1","2","3","4","5","6"),
                    labels=c("1","2","3","4","5","6")) +
  facet_grid(TURQ~Scen, scales="free_y", labeller=labeller(TURQ=turq_labels, Scen=scen_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
Stck.GUR

#
# ---- FIG: Cost Component ----
plot_list = list()
for(i in c(2020,2050,2100)){
  Costs.Abs <- ggplot(data=subset(CostComponent, (TURQ==1) & 
                                    Region %in% Regions & 
                                    (Year==i) &
                                    (Scen=="SSP2"|Scen=="SSP2_450") &
                                    (variable=="CapitalNorm"|variable=="HeatFNorm"|variable=="CoolFNorm"))
                    , aes(x=EffLevel,y = value, fill=variable)) + 
    geom_bar(stat="identity") +
    xlab("") + ylab("") +
    theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
    theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
    theme(legend.position="right") +
    scale_fill_manual(values=c("gray","firebrick","cornflowerblue"),
                      name="",
                      breaks=c("CapitalNorm","HeatFNorm","CoolFNorm"),
                      labels=c("Capital Costs","Heating Fuel","Cooling Fuel")) +
    
    facet_grid(Region~Scen, scales="free_y", labeller=labeller(Region=reg_labels, Scen=scen_labels)) + 
    theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
  Costs.Abs
  plot_list[[i]] = Costs.Abs
}
plot_list[[2020]]
plot_list[[2100]]

Costs.WEU <- ggplot(data=subset(CostComponent, (TURQ==1) & 
                                  Region == 11  & 
                                  (Year==2020|Year==2050|Year==2100) &
                                  (Scen=="SSP2"|Scen=="SSP2_450") &
                                  (variable=="CapitalNorm"|variable=="HeatFNorm"|variable=="CoolFNorm"))
                    , aes(x=EffLevel,y = value, fill=variable)) + 
  geom_bar(stat="identity") +
  xlab("Efficiency Level") + ylab("Cost relative to 2020 - Efficiency Level = 1") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_x_continuous(breaks=c(1,2,3,4,5,6)) + 
  scale_fill_manual(values=c("gray","firebrick","cornflowerblue"),
                      name="",
                      breaks=c("CapitalNorm","HeatFNorm","CoolFNorm"),
                      labels=c("Capital Costs","Heating Fuel","Cooling Fuel")) +
  facet_grid(Year~Scen, scales="free_y", labeller=labeller(Region=reg_labels, Scen=scen_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
Costs.WEU

#
# ---- FIG: Efficiency Level ----
EffMS.UQS <- ggplot(data=subset(InsulMS, (TURQ==4|TURQ==5|TURQ==6|TURQ==7|TURQ==8)&Region==ActiveRegion)
                , aes(x=Year,y = value, fill=EffLevel)) + 
  geom_bar(stat="identity") +
  xlim(2020,2100) +
  xlab("") + ylab("") +
  ggtitle(paste("Region: ",ActiveRegion)) +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  facet_grid(TURQ~Scen) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip))
EffMS.UQS

Eff.TRS <- ggplot(data=subset(InsulMS, TURQ==ActiveTURQ & Region %in% Regions)
                 , aes(x=Year,y = value, fill=EffLevel)) + 
  geom_bar(stat="identity") +
  xlim(2020,2100) +
  xlab("") + ylab("") +
  ggtitle(paste("TURQ: ",ActiveTURQ)) +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  facet_grid(Region~Scen, labeller=labeller(Region=reg_labels,Scen=scen_labels)) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
Eff.TRS

#
# ---- FIG: UE Intensity ----
UEInt.SRT <- ggplot(data=subset(UEIntHeat, variable=="Total" & (Scen=="SSP2"|Scen=="SSP2_450"|Scen=="SSP2_450_NIR"))
                , aes(x=Year,y = value, colour=Scen)) + 
  geom_line(alpha=1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  xlim(2020,2100) +
  ylim(0,1.2) +
  xlab("") + ylab("kJ/cap/HDD") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeLeg, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  scale_colour_manual(values=c("navy","green","gray"),
                      name="",
                      breaks=c("SSP2","SSP2_450","SSP2_450_NIR"),
                      labels=c("Baseline","Mitg.","Mitig. No Improv. Insul.")) +
  facet_wrap(Region~., nrow=3) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
UEInt.SRT
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
# ---- FIG: Carbon Contents ----
CCFig <- ggplot(data=subset(CC, Region %in% Regions & (Scen=="SSP2"|Scen=="SSP2_450") & !(Region==27|Region==5))
                 , aes(x=Year,y = value, colour=Scen)) + 
  geom_line(size=1,alpha=1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') + 
  xlim(2020,2100) +
  xlab("") + ylab("kgC/GJ") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_colour_manual(values=c("navy","green3"),
                      name="",
                      breaks=c("SSP2","SSP2_450"),
                      labels=c("Baseline","Mitig.")) +
  facet_grid(variable~Region, scales="free_y", labeller=labeller(Region=reg_labels, Scen=scen_labels, variable=cc_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
CCFig

#
# # ---- OUTPUTS ----
# png(file = "output/BuildStocks/Stocks_TR.png", width = 8*ppi, height = 4*ppi, units = "px", res = ppi)
# plot(Stck.TR)
# dev.off()
# # #
# png(file = "output/BuildStocks/Stocks_TG.png", width = 3*ppi, height = 4*ppi, units = "px", res = ppi)
# plot(Stck.TG)
# dev.off()
# #
# png(file = "output/BuildStocks/Stocks_GUR.png", width = 6*ppi, height = 5*ppi, units = "px", res = ppi)
# plot(Stck.GUR)
# dev.off()
# #
# png(file = "output/BuildStocks/CostComponent2020.png", width = 6*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(plot_list[[2020]])
# dev.off()
# #
# png(file = "output/BuildStocks/CostComponent2050.png", width = 6*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(plot_list[[2050]])
# dev.off()
# #
# png(file = "output/BuildStocks/CostComponent2100.png", width = 6*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(plot_list[[2100]])
# dev.off()
# #
# png(file = "output/BuildStocks/CostComponentWEU.png", width = 6*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(Costs.WEU)
# dev.off()
# #
# png(file = "output/BuildStocks/UEIntGlobal.png", width = 10*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(UEInt.SRT)
# dev.off()
# #
# png(file = "output/BuildStocks/Effect_Base.png", width = 10*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(BaseEffect)
# dev.off()
# # #
# png(file = "output/BuildStocks/Effect_Mitig.png", width = 10*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(MitigEffect)
# dev.off()
# #
# png(file = "output/BuildStocks/Effect_All1.png", width = 10*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(AllEffect1)
# dev.off()
# 
# png(file = "output/BuildStocks/Effect_All2.png", width = 10*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(AllEffect2)
# dev.off()
#

# png(file = "output/BuildStocks/CarbonContents.png", width = 6*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(CCFig)
# dev.off()
# #

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
# ---- Not Used  OUTPUTS ----
# png(file = "output/BuildStocks/EffLevel_TRS.png", width = 6*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(Eff.TRS)
# dev.off()
# #
# png(file = "output/BuildStocks/UEInt.png", width = 10*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(UEInt.SRT)
# dev.off()
# #
