# R script to process the results of the TIMER Building Stocks & Renovation represetnation
# Vassilis Daioglou, November 2019
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

Regions = c(2,5,10,11,16,20,27)
Years = c("1980","1990","2000","2010","2015","2020","2025","2030","2035","2040","2045",
          "2050","2055","2060","2065","2070","2075","2080","2085","2090","2095","2100")

ActiveRegion <- 11
ActiveTURQ<-6

data_base <- "data/BuildStocks/SSP2.xlsx"
data_miti <- "data/BuildStocks/SSP2_450.xlsx"
data_miti_NR <- "data/BuildStocks/SSP2_450_NR.xlsx"
data_miti_NFS <- "data/BuildStocks/SSP2_450_NFS.xlsx"
# set higher RAM capacity for java (used in clsx package)
options(java.parameters = "-Xmx8000m")

# ---- INPUTS: Data ----
  # set directory path 
setwd("C:/Users/Asus/Documents/Github/IMAGE/")
  # Read Data Files for Baseline Scenario
Stocks = read.xlsx(data_base, sheet = "FSInsul", startRow=4)
InsulMS = read.xlsx(data_base, sheet = "MS_Aggr", startRow=4)
RenovRate = read.xlsx(data_base, sheet = "Renov_Rate", startRow=4)
RenovRate_Ave = read.xlsx(data_base, sheet = "Renov_Rate_ave", startRow=4)
UEHeatCool_pc = read.xlsx(data_base, sheet = "UEHeatCool_pc", startRow=4)
UEIntHeat = read.xlsx(data_base, sheet = "UEintHeat_Fut", startRow=4)
CO2EmisHeatCool_pc = read.xlsx(data_base, sheet = "CO2EmisHeatCool_pc", startRow=4)
CCElec = read.xlsx(data_base, sheet = "CCElec", startRow=4)
CCSpaceHeat = read.xlsx(data_base, sheet = "CCSpaceHeat", startRow=4)
CostComponent = read.xlsx(data_base, sheet = "CostComponent", startRow=4)
CostFrac = read.xlsx(data_base, sheet = "CostFrac", startRow=4)

  # Read Data Files for Mitigation Scenario
Stocks.450 = read.xlsx(data_miti, sheet = "FSInsul", startRow=4)
InsulMS.450 = read.xlsx(data_miti, sheet = "MS_Aggr", startRow=4)
RenovRate.450 = read.xlsx(data_miti, sheet = "Renov_Rate", startRow=4)
RenovRate_Ave.450 = read.xlsx(data_miti, sheet = "Renov_Rate_ave", startRow=4)
UEHeatCool_pc.450 = read.xlsx(data_miti, sheet = "UEHeatCool_pc", startRow=4)
UEIntHeat.450 = read.xlsx(data_miti, sheet = "UEintHeat_Fut", startRow=4)
CO2EmisHeatCool_pc.450 = read.xlsx(data_miti, sheet = "CO2EmisHeatCool_pc", startRow=4)
CCElec.450 = read.xlsx(data_miti, sheet = "CCElec", startRow=4)
CCSpaceHeat.450 = read.xlsx(data_miti, sheet = "CCSpaceHeat", startRow=4)
CostComponent.450 = read.xlsx(data_miti, sheet = "CostComponent", startRow=4)
CostFrac.450 = read.xlsx(data_miti, sheet = "CostFrac", startRow=4)

# Read Data Files for Mitigation Scenario WITHOUT renovation
Stocks.450_NR = read.xlsx(data_miti_NR, sheet = "FSInsul", startRow=4)
InsulMS.450_NR = read.xlsx(data_miti_NR, sheet = "MS_Aggr", startRow=4)
RenovRate.450_NR = read.xlsx(data_miti_NR, sheet = "Renov_Rate", startRow=4)
RenovRate_Ave.450_NR = read.xlsx(data_miti_NR, sheet = "Renov_Rate_ave", startRow=4)
UEHeatCool_pc.450_NR = read.xlsx(data_miti_NR, sheet = "UEHeatCool_pc", startRow=4)
UEIntHeat.450_NR = read.xlsx(data_miti_NR, sheet = "UEintHeat_Fut", startRow=4)
CO2EmisHeatCool_pc.450_NR = read.xlsx(data_miti_NR, sheet = "CO2EmisHeatCool_pc", startRow=4)
CCElec.450_NR = read.xlsx(data_miti_NR, sheet = "CCElec", startRow=4)
CCSpaceHeat.450_NR = read.xlsx(data_miti_NR, sheet = "CCSpaceHeat", startRow=4)
CostComponent.450_NR = read.xlsx(data_miti_NR, sheet = "CostComponent", startRow=4)
CostFrac.450_NR = read.xlsx(data_miti_NR, sheet = "CostFrac", startRow=4)

# Read Data Files for Mitigation Scenario WITHOUT fuel switching
Stocks.450_NFS = read.xlsx(data_miti_NFS, sheet = "FSInsul", startRow=4)
InsulMS.450_NFS = read.xlsx(data_miti_NFS, sheet = "MS_Aggr", startRow=4)
RenovRate.450_NFS = read.xlsx(data_miti_NFS, sheet = "Renov_Rate", startRow=4)
RenovRate_Ave.450_NFS = read.xlsx(data_miti_NFS, sheet = "Renov_Rate_ave", startRow=4)
UEHeatCool_pc.450_NFS = read.xlsx(data_miti_NFS, sheet = "UEHeatCool_pc", startRow=4)
UEIntHeat.450_NFS = read.xlsx(data_miti_NFS, sheet = "UEintHeat_Fut", startRow=4)
CO2EmisHeatCool_pc.450_NFS = read.xlsx(data_miti_NFS, sheet = "CO2EmisHeatCool_pc", startRow=4)
CCElec.450_NFS = read.xlsx(data_miti_NFS, sheet = "CCElec", startRow=4)
CCSpaceHeat.450_NFS = read.xlsx(data_miti_NFS, sheet = "CCSpaceHeat", startRow=4)
CostComponent.450_NFS = read.xlsx(data_miti_NFS, sheet = "CostComponent", startRow=4)
CostFrac.450_NFS = read.xlsx(data_miti_NFS, sheet = "CostFrac", startRow=4)

#
# ---- MUNGING ----
# ---- ***Stocks ----
Stocks$Scen <- "SSP2"
Stocks.450$Scen <- "SSP2_450"
Stocks.450_NR$Scen <- "SSP2_450_NR"
Stocks.450_NFS$Scen <- "SSP2_450_NFS"
Stocks = rbind(Stocks,Stocks.450,Stocks.450_NR,Stocks.450_NFS)
rm(Stocks.450,Stocks.450_NR,Stocks.450_NFS)

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
InsulMS = rbind(InsulMS,InsulMS.450,InsulMS.450_NR)
rm(InsulMS.450,InsulMS.450_NR)

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
RenovRate.450_NFS$Scen <- "SSP2_450_NFS"
RenovRate = rbind(RenovRate,RenovRate.450,RenovRate.450_NR,RenovRate.450_NFS)
rm(RenovRate.450, RenovRate.450_NR, RenovRate.450_NFS)

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
RenovRate_Ave.450_NFS$Scen <- "SSP2_450_NFS"
RenovRate_Ave = rbind(RenovRate_Ave,RenovRate_Ave.450,RenovRate_Ave.450_NR,RenovRate_Ave.450_NFS)
rm(RenovRate_Ave.450, RenovRate_Ave.450_NR,RenovRate_Ave.450_NFS)

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
UEIntHeat.450_NFS$Scen <- "SSP2_450_NFS"
UEIntHeat = rbind(UEIntHeat,UEIntHeat.450,UEIntHeat.450_NR,UEIntHeat.450_NFS)
rm(UEIntHeat.450,UEIntHeat.450_NR,UEIntHeat.450_NFS)

colnames(UEIntHeat)[1:15] <- c("Year","Region","Total","Urban","Rural",
                               "U1","U2","U3","U4","U5",
                               "R1","R2","R3","R4","R5")
UEIntHeat = melt(UEIntHeat, id.vars=c("Year","Region","Scen"))

  # Normalise to 2020 value
UEIntHeat$ID <- paste(UEIntHeat$Region,UEIntHeat$Scen,UEIntHeat$variable)
UEIntHeat.2010 = subset(UEIntHeat, Year==2010)
UEIntHeat$val_2010 = UEIntHeat.2010[match(UEIntHeat$ID, UEIntHeat.2010$ID),"value"]
rm(UEIntHeat.2010)
UEIntHeat = UEIntHeat %>% mutate(Normalised_2010 = value/val_2010)

UEIntHeat = subset(UEIntHeat, select=-c(value,ID,val_2010))
colnames(UEIntHeat)[5] <- "value"

#
# ---- ***Heating+Cooling Energy USe ----
UEHeatCool_pc$Scen <- "SSP2"
UEHeatCool_pc.450$Scen <- "SSP2_450"
UEHeatCool_pc.450_NR$Scen <- "SSP2_450_NR"
UEHeatCool_pc.450_NFS$Scen <- "SSP2_450_NFS"
UEHeatCool_pc = rbind(UEHeatCool_pc,UEHeatCool_pc.450,UEHeatCool_pc.450_NR,UEHeatCool_pc.450_NFS)
rm(UEHeatCool_pc.450,UEHeatCool_pc.450_NR,UEHeatCool_pc.450_NFS)

colnames(UEHeatCool_pc)[1:15] <- c("Year","Region","Total","Urban","Rural",
                                "U1","U2","U3","U4","U5",
                                "R1","R2","R3","R4","R5")

UEHeatCool_pc = melt(UEHeatCool_pc, id.vars=c("Year","Region","Scen"))

  # Normalise to 2010 value
UEHeatCool_pc$ID <- paste(UEHeatCool_pc$Region,UEHeatCool_pc$Scen,UEHeatCool_pc$variable)
UEHeatCool_pc.2010 = subset(UEHeatCool_pc, Year==2010)
UEHeatCool_pc$val_2010 = UEHeatCool_pc.2010[match(UEHeatCool_pc$ID, UEHeatCool_pc.2010$ID),"value"]
rm(UEHeatCool_pc.2010)
UEHeatCool_pc = UEHeatCool_pc %>% mutate(Normalised_2010 = value/val_2010)

UEHeatCool_pc = subset(UEHeatCool_pc, select=-c(value,ID,val_2010))
colnames(UEHeatCool_pc)[5] <- "value"

#
# ---- ***CO2 Emissions ----
CO2EmisHeatCool_pc$Scen <- "SSP2"
CO2EmisHeatCool_pc.450$Scen <- "SSP2_450"
CO2EmisHeatCool_pc.450_NR$Scen <- "SSP2_450_NR"
CO2EmisHeatCool_pc.450_NFS$Scen <- "SSP2_450_NFS"
CO2EmisHeatCool_pc = rbind(CO2EmisHeatCool_pc,CO2EmisHeatCool_pc.450,CO2EmisHeatCool_pc.450_NR,CO2EmisHeatCool_pc.450_NFS)
rm(CO2EmisHeatCool_pc.450,CO2EmisHeatCool_pc.450_NR,CO2EmisHeatCool_pc.450_NFS)

colnames(CO2EmisHeatCool_pc)[1:28] <- c("Year",1,2,3,4,5,6,7,8,9,10,
                                        11,12,13,14,15,16,17,18,19,20,
                                        21,22,23,24,25,26,27)
CO2EmisHeatCool_pc = melt(CO2EmisHeatCool_pc, id.vars=c("Year","Scen"))
colnames(CO2EmisHeatCool_pc)[3] <- c("Region")
CO2EmisHeatCool_pc$variable <- "Total"

  # Normalise to 2020 value
CO2EmisHeatCool_pc$ID <- paste(CO2EmisHeatCool_pc$Region,CO2EmisHeatCool_pc$Scen,CO2EmisHeatCool_pc$variable)
CO2EmisHeatCool_pc.2010 = subset(CO2EmisHeatCool_pc, Year==2010)
CO2EmisHeatCool_pc$val_2010 = CO2EmisHeatCool_pc.2010[match(CO2EmisHeatCool_pc$ID, CO2EmisHeatCool_pc.2010$ID),"value"]
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
CCSpaceHeat.450_NFS$Scen <- "SSP2_450_NFS"
CCSpaceHeat = rbind(CCSpaceHeat,CCSpaceHeat.450,CCSpaceHeat.450_NFS)
rm(CCSpaceHeat.450,CCSpaceHeat.450_NFS)

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
CostComponent.450_NFS$Scen <- "SSP2_450_NFS"
CostComponent$Variable <- "Absolute"
CostComponent.450$Variable <- "Absolute"
CostComponent.450_NR$Variable <- "Absolute"
CostComponent.450_NFS$Variable <- "Absolute"
CostComponent$class_.4 <-NULL
CostComponent.450$class_.4 <-NULL
CostComponent.450_NR$class_.4 <-NULL
CostComponent.450_NFS$class_.4 <-NULL

CostFrac$Scen <- "SSP2"
CostFrac.450$Scen <- "SSP2_450"
CostFrac.450_NR$Scen <- "SSP2_450_NR"
CostFrac.450_NFS$Scen <- "SSP2_450_NFS"
CostFrac$Variable <- "Fraction"
CostFrac.450$Variable <- "Fraction"
CostFrac.450_NR$Variable <- "Fraction"
CostFrac.450_NFS$Variable <- "Fraction"

CostComponent = rbind(CostComponent,CostComponent.450,CostComponent.450_NR,CostComponent.450_NFS,
                      CostFrac,CostFrac.450,CostFrac.450_NR,CostFrac.450_NFS)

rm(CostComponent.450,CostComponent.450_NR,CostComponent.450_NFS,
   CostFrac,CostFrac.450,CostFrac.450_NR,CostFrac.450_NFS)

colnames(CostComponent)[1:9] <- c("Year","Region","TURQ","EffLevel",
                                   "Capital","HeatingFuel","CoolingFuel","Scen","Type")
CostComponent = melt(CostComponent, id.vars=c("Year","Region","TURQ","Scen","EffLevel","Type"))

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
# ---- LABELS ----
scen_labels <-c("SSP1"="SSP1",
                "SSP2"="Baseline",
                "SSP3"="SSP3",
                "SSP1_450"="SSP1 - 2°C",
                "SSP2_450"="Mitig.",
                "SSP1_20"="SSP1 - 1.5°C",
                "SSP2_20"="SSP2 - 1.5°C",
                "SSP2_450_NR"="Mitig. No Renov.",
                "SSP2_450_NFS"="Mitig. No Fuel Switching")

reg_labels <-c("2"="USA",
               "5"="Brazil",
               "10"="S. Africa",
               "11"="W. Europe",
               "16"="Russia",
               "18"="India",
               "20"="China",
               "27"="World")

var_labels <-c("RenovationRate"="Renovation \nRate \n(%)",
               "UeIntHeat"="Heating \nIntensity \n(2010=1)",
               "HeatCoolDemand_pc"="Heating & Cooling \nDemand \n(2010=1)",
               "ResiCO2EmisHeatCool"="Heating & Cooling \nEmissions \n(2010=1)")

# ---- FIGURES ----
# ---- FIG: Stocks ----
Stck.T <- ggplot(data=subset(Stocks, (TURQ==6)& Region %in% Regions)
                  , aes(x=Year,y = value, fill=EffLevel)) + 
  geom_bar(stat="identity") +
  # geom_line(alpha=1) +
  xlim(2010,2100) +
  xlab("") + ylab("mill. m^2") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  # scale_colour_manual(values=c("darkorchid","forestgreen"),
  #                     name="",
  #                     breaks=c("Urban","Rural"),
  #                     labels=c("Urban","Rural")) +
  facet_grid(Region~Scen, scales="free_y", labeller=labeller(Region=reg_labels, Scen=scen_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
Stck.T

#
# ---- FIG: Cost Component ----
plot_list = list()
for(i in c(2020,2050,2100)){
  Costs.Abs <- ggplot(data=subset(CostComponent, (TURQ==1)& Region %in% Regions & Type=="Absolute" & (Year==2100))
                    , aes(x=EffLevel,y = value, fill=variable)) + 
    geom_bar(stat="identity") +
    # xlim(2010,2100) +
    xlab("") + ylab("") +
    theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
    theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
    theme(legend.position="right") +
    facet_grid(Region~Scen, scales="free_y", labeller=labeller(Region=reg_labels, Scen=scen_labels)) + 
    theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
  Costs.Abs
  plot_list[[i]] = Costs.Abs
}
plot_list[[2020]]
plot_list[[2100]]
# ---- FIG: Efficiency Level ----
# EffMS.newGlobTUR = subset(EffMS.new, (TURQ==1|TURQ==2|TURQ==3)&Region==27)

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
UEInt.SRT <- ggplot(data=subset(UEIntHeat, variable=="Total")
                , aes(x=Year,y = value, colour=Scen)) + 
  geom_line(alpha=1) +
  xlim(2020,2100) +
  xlab("") + ylab("kJ/cap/HDD") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeLeg, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  scale_colour_manual(values=c("navy","green","firebrick","gray"),
                      name="",
                      breaks=c("SSP2","SSP2_450","SSP2_450_NR","SSP2_450_NFS"),
                      labels=c("Baseline","Mitig.","Mitig No Rrenov.","Mitig No Fuel Switch")) +
  facet_wrap(Region~., nrow=3) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
UEInt.SRT
#
# ---- FIG: Combined Results ----
TotEffect <- ggplot(data=subset(DATA.TRQS, TURQ=="Total" & !(Variable=="RenovationRate"))
                    , aes(x=Year,y = value, colour=Scen)) + 
  geom_line(alpha=0.8) +
  xlim(2010,2100) +
  xlab("") + ylab("") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeStrip, hjust=1), axis.text.y = element_text(size=FSizeStrip)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_colour_manual(values=c("navy","green","firebrick","gray"),
                      name="",
                      breaks=c("SSP2","SSP2_450","SSP2_450_NR","SSP2_450_NFS"),
                      labels=c("Baseline","Mitig.","Mitig No Rrenov.","Mitig No Fuel Switch")) +
  facet_grid(Var_Order~Reg_Order, scales="free_y",labeller=labeller(Reg_Order=reg_labels, Scen=scen_labels, Var_Order=var_labels)) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
TotEffect
#
# ---- FIG: Carbon Contents ----
CCFig <- ggplot(data=subset(CC, Region %in% Regions)
                 , aes(x=Year,y = value, colour=Scen)) + 
  geom_line(alpha=1) +
  xlim(2010,2100) +
  xlab("") + ylab("kgC/GJ") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_colour_manual(values=c("navy","green","gray"),
                      name="",
                      breaks=c("SSP2","SSP2_450","SSP2_450_NFS"),
                      labels=c("SSP2","SSP2 - 2°C","No Fuel Switching")) +
  facet_grid(Region~variable, scales="free_y", labeller=labeller(Region=reg_labels, Scen=scen_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
CCFig

#
# # ---- OUTPUTS ----
# png(file = "output/BuildStocks/Stocks_T.png", width = 6*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(Stck.T)
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
# png(file = "output/BuildStocks/EffLevel_TRS.png", width = 6*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(Eff.TRS)
# dev.off()
# #
# png(file = "output/BuildStocks/UEInt.png", width = 10*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(UEInt.SRT)
# dev.off()
# #
# png(file = "output/BuildStocks/RenovEffect.png", width = 10*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(TotEffect)
# dev.off()
# #
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