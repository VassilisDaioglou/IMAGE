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

Regions = c(2,5,10,11,16,18,20)
Years = c("1980","1990","2000","2010","2020","2030","2040","2050","2060","2070","2080","2090","2100")

ActiveRegion <- 27
ActiveTURQ<-8

  # set higher RAM capacity for java (used in clsx package)
options(java.parameters = "-Xmx8000m")

# ---- INPUTS: Data ----
  # set directory path 
setwd("C:/Users/Asus/Documents/Github/IMAGE/")
  # Read Data Files for Baseline Scenario
Investment_pc=read.xlsx("data/BuildStocks/SSP2.xlsx", sheet = "Investments_Total_pc", startRow=4)
EffMS_new=read.xlsx("data/BuildStocks/SSP2.xlsx", sheet = "MS_new", startRow=4)
RenovRate=read.xlsx("data/BuildStocks/SSP2.xlsx", sheet = "Renov_Rate", startRow=4)
UEHeatCool_pc=read.xlsx("data/BuildStocks/SSP2.xlsx", sheet = "UEHeatCool_pc", startRow=4)
UEIntHeat_Fut=read.xlsx("data/BuildStocks/SSP2.xlsx", sheet = "UEintHeat_Fut", startRow=4)
CO2EmisHeatCool_pc=read.xlsx("data/BuildStocks/SSP2.xlsx", sheet = "CO2EmisHeatCool_pc", startRow=4)
CCElec=read.xlsx("data/BuildStocks/SSP2.xlsx", sheet = "CCElec", startRow=4)
CCSpaceHeat=read.xlsx("data/BuildStocks/SSP2.xlsx", sheet = "CCSpaceHeat", startRow=4)

  # Read Data Files for Mitigation Scenario
Investment_pc.450=read.xlsx("data/BuildStocks/SSP2_450.xlsx", sheet = "Investments_Total_pc", startRow=4)
EffMS_new.450=read.xlsx("data/BuildStocks/SSP2_450.xlsx", sheet = "MS_new", startRow=4)
RenovRate.450=read.xlsx("data/BuildStocks/SSP2_450.xlsx", sheet = "Renov_Rate", startRow=4)
UEHeatCool_pc.450=read.xlsx("data/BuildStocks/SSP2_450.xlsx", sheet = "UEHeatCool_pc", startRow=4)
UEIntHeat_Fut.450=read.xlsx("data/BuildStocks/SSP2_450.xlsx", sheet = "UEintHeat_Fut", startRow=4)
CO2EmisHeatCool_pc.450=read.xlsx("data/BuildStocks/SSP2_450.xlsx", sheet = "CO2EmisHeatCool_pc", startRow=4)
CCElec.450=read.xlsx("data/BuildStocks/SSP2_450.xlsx", sheet = "CCElec", startRow=4)
CCSpaceHeat.450=read.xlsx("data/BuildStocks/SSP2_450.xlsx", sheet = "CCSpaceHeat", startRow=4)

#
# ---- MUNGING ----
# ---- ***Investments ----
Investment$Scen <- "SSP2"
Investment.450$Scen <- "SSP2_450"
Investment = rbind(Investment,Investment.450)
rm(Investment.450)

colnames(Investment)[1:16] <- c("Year","Region","Total","Urban","Rural",
                                "U1","U2","U3","U4","U5",
                                "R1","R2","R3","R4","R5",
                                "Scen")
Investment = melt(Investment, id.vars=c("Year","Region","Scen"))
  # Normalise to 2020 value
Investment$ID <- paste(Investment$Region,Investment$Scen,Investment$variable)
Investment.2010 = subset(Investment, Year==2010)
Investment$val_2010 = Investment.2010[match(Investment$ID, Investment.2010$ID),"value"]
rm(Investment.2010)
Investment = Investment %>% mutate(Normalised_2010 = value/val_2010)

Investment = subset(Investment, select=-c(value,ID,val_2010))
colnames(Investment)[5] <- "value"
# ---- ***Efficiency Market Shares ----
EffMS.new$Scen <- "SSP2"
EffMS.new450$Scen <- "SSP2_450"
EffMS.new = rbind(EffMS.new,EffMS.new450)
rm(EffMS.new450)

colnames(EffMS.new)[1:8] <- c("Year","Region","TURQ","EffLevel","B1","B2","B3","B4")
EffMS.new = melt(EffMS.new, id.vars=c("Year","Region","TURQ","EffLevel","Scen"))
EffMS.new = subset(EffMS.new, variable=="B1")
# EffMS.new = subset(EffMS.new, !Region==27)
EffMS.new$value <- as.numeric(EffMS.new$value)
EffMS.new$EffLevel <- as.character(EffMS.new$EffLevel)

# ---- ***Renovation Rates ----
  # Renovation Rate (Annual)
RenovRate$Scen <- "SSP2"
RenovRate.450$Scen <- "SSP2_450"
RenovRate = rbind(RenovRate,RenovRate.450)
rm(RenovRate.450)

colnames(RenovRate)[1:15] <- c("Year","Region","Total","Urban","Rural",
                               "U1","U2","U3","U4","U5",
                               "R1","R2","R3","R4","R5")
RenovRate = melt(RenovRate, id.vars=c("Year","Region","Scen"))
RenovRate$value <- RenovRate$value * 100
# ---- ***Heating USeful Energy Intensity ----
UEIntHeat$Scen <- "SSP2"
UEIntHeat.450$Scen <- "SSP2_450"
UEIntHeat = rbind(UEIntHeat,UEIntHeat.450)
rm(UEIntHeat.450)

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
# ---- ***HEating Energy USe ----
EneUseFunc$Scen <- "SSP2"
EneUseFunc.450$Scen <- "SSP2_450"
EneUseFunc = rbind(EneUseFunc,EneUseFunc.450)
rm(EneUseFunc.450)

colnames(EneUseFunc)[1:3] <- c("Year","Region","TURQ")
EneUseFunc = subset(EneUseFunc, select=c(Year,Region,TURQ,class_.5,Scen))
EneUseFunc = spread(EneUseFunc, TURQ,class_.5)
colnames(EneUseFunc)[1:16] <- c("Year","Region","Scen","Total","Urban","Rural",
                               "U1","U2","U3","U4","U5",
                               "R1","R2","R3","R4","R5")
EneUseFunc = melt(EneUseFunc, id.vars=c("Year","Region","Scen"))
EneUseFunc = subset(EneUseFunc, !Region==27)
EneUseFunc$Region[EneUseFunc$Region==28] <- 27

  # Normalise to 2020 value
EneUseFunc$ID <- paste(EneUseFunc$Region,EneUseFunc$Scen,EneUseFunc$variable)
EneUseFunc.2010 = subset(EneUseFunc, Year==2010)
EneUseFunc$val_2010 = EneUseFunc.2010[match(EneUseFunc$ID, EneUseFunc.2010$ID),"value"]
rm(EneUseFunc.2010)
EneUseFunc = EneUseFunc %>% mutate(Normalised_2010 = value/val_2010)

EneUseFunc = subset(EneUseFunc, select=-c(value,ID,val_2010))
colnames(EneUseFunc)[5] <- "value"

#
# ---- ***CO2 Emissions ----
CO2Emis$Scen <- "SSP2"
CO2Emis.450$Scen <- "SSP2_450"
CO2Emis = rbind(CO2Emis,CO2Emis.450)
rm(CO2Emis.450)

colnames(CO2Emis)[1:2] <- c("Year","Region")
CO2Emis = subset(CO2Emis, select=c(Year,Region,class_.5,Scen))
colnames(CO2Emis)[3] <- c("value")
CO2Emis = subset(CO2Emis, !Region==27)
CO2Emis$Region[CO2Emis$Region==28] <- 27
CO2Emis$variable <- "Total"

  # Normalise to 2020 value
CO2Emis$ID <- paste(CO2Emis$Region,CO2Emis$Scen,CO2Emis$variable)
CO2Emis.2010 = subset(CO2Emis, Year==2010)
CO2Emis$val_2010 = CO2Emis.2010[match(CO2Emis$ID, CO2Emis.2010$ID),"value"]
rm(CO2Emis.2010)
CO2Emis = CO2Emis %>% mutate(Normalised_2010 = value/val_2010)

CO2Emis = subset(CO2Emis, select=-c(value,ID,val_2010))
colnames(CO2Emis)[5] <- "value"

#
# ---- DATA AGGREGATION ----

Stocks=subset(Stocks, Year %in% Years)
Investment=subset(Investment, Year %in% Years)
EffMS.new=subset(EffMS.new, Year %in% Years)
RenovRate=subset(RenovRate, Year %in% Years)
UEIntHeat=subset(UEIntHeat, Year %in% Years)
EneUseFunc=subset(EneUseFunc, Year %in% Years)
CO2Emis=subset(CO2Emis, Year %in% Years)

Investment$Variable <- "Investments"
RenovRate$Variable <- "RenovationRate"
UEIntHeat$Variable <- "UeIntHeat"
EneUseFunc$Variable <- "HeatingDemand"
CO2Emis$Variable <- "ResiCO2Emis"

DATA.TRQS = rbind(Investment,RenovRate,UEIntHeat,EneUseFunc,CO2Emis) 
colnames(DATA.TRQS)[4] <- "TURQ"

DATA.TRQS$Var_Order <- factor(DATA.TRQS$Variable, level=c("Investments",
                                                          "RenovationRate",
                                                          "UeIntHeat",
                                                          "HeatingDemand",
                                                          "ResiCO2Emis"))

DATA.TS = subset(DATA.TRQS, Region == 27&TURQ == "Total" & Year %in% Years)
DATA.TS$TURQ <- NULL
DATA.TRQS = subset(DATA.TRQS, Region %in% Regions & Year %in% Years)

#
# ---- LABELS ----
scen_labels <-c("SSP1"="SSP1","SSP2"="SSP2","SSP3"="SSP3",
                "SSP1_450"="SSP1 - 2°C",
                "SSP2_450"="SSP2 - 2°C",
                "SSP1_20"="SSP1 - 1.5°C",
                "SSP2_20"="SSP2 - 1.5°C")

reg_labels <-c("2"="USA",
               "5"="Brazil",
               "10"="S. Africa",
               "11"="W. Europe",
               "16"="Russia",
               "18"="India",
               "20"="China")

var_labels <-c("Investments"="Investments \n(2010=1)",
               "RenovationRate"="Renovation \nRate \n(%)",
               "UeIntHeat"="Heating \nIntensity \n(2010=1)",
               "HeatingDemand"="Heating \nDemand \n(2010=1)",
               "ResiCO2Emis"="Residential \nEmissions \n(2010=1)")

# ---- FIGURES ----
# ---- FIG: Investments ----
Inv.UR <- ggplot(data=subset(Investment, (variable=="Urban"|variable=="Rural")& Region %in% Regions)
                  , aes(x=Year,y = value, colour=variable)) + 
  # geom_bar(stat="identity") +
  geom_line(alpha=1) +
  xlim(2020,2100) +
  xlab("") + ylab("2010 = 1") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_colour_manual(values=c("darkorchid","forestgreen"),
                      name="",
                      breaks=c("Urban","Rural"),
                      labels=c("Urban","Rural")) +
  facet_grid(Region~Scen, scales="free_y", labeller=labeller(Region=reg_labels, Scen=scen_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
Inv.UR

#
# ---- FIG: Efficiency Level ----
# EffMS.newGlobTUR = subset(EffMS.new, (TURQ==1|TURQ==2|TURQ==3)&Region==27)

Eff.UQS <- ggplot(data=subset(EffMS.new, (TURQ==1|TURQ==2|TURQ==3)&Region==ActiveRegion)
                , aes(x=Year,y = value, fill=EffLevel)) + 
  geom_bar(stat="identity") +
  xlim(2020,2100) +
  xlab("") + ylab("") +
  ggtitle(paste("Region: ",ActiveRegion)) +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  # scale_colour_manual(values=c("forestgreen","dodgerblue","firebrick1"),
  #                     name="",
  #                     breaks=c("New","Decomissioned","Total"),
  #                     labels=c("New","Decomissioned","Total")) +
  # 
  facet_grid(TURQ~Scen) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip))
Eff.UQS

Eff.TRS <- ggplot(data=subset(EffMS.new, TURQ==ActiveTURQ & Region %in% Regions)
                 , aes(x=Year,y = value, fill=EffLevel)) + 
  geom_bar(stat="identity") +
  xlim(2020,2100) +
  xlab("") + ylab("") +
  ggtitle(paste("TURQ: ",ActiveTURQ)) +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  # scale_colour_manual(values=c("forestgreen","dodgerblue","firebrick1"),
  #                     name="",
  #                     breaks=c("New","Decomissioned","Total"),
  #                     labels=c("New","Decomissioned","Total")) +
  # 
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
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeStrip, hjust=1), axis.text.y = element_text(size=FSizeStrip)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  # scale_colour_manual(values=c("forestgreen","dodgerblue","firebrick1"),
  #                     name="",
  #                     breaks=c("New","Decomissioned","Total"),
  #                     labels=c("New","Decomissioned","Total")) +
  # 
  facet_wrap(Region~.) 
UEInt.SRT
#
# ---- FIG: Combined Results ----
RenovEffect <- ggplot(data=subset(DATA.TRQS, TURQ=="Total")
                    , aes(x=Year,y = value, colour=Scen)) + 
  geom_line(alpha=0.8) +
  xlim(2010,2100) +
  xlab("") + ylab("") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeStrip, hjust=1), axis.text.y = element_text(size=FSizeStrip)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_colour_manual(values=c("navy","green"),
                      name="",
                      breaks=c("SSP2","SSP2_450"),
                      labels=c("SSP2","SSP2 - 2°C")) +
  facet_grid(Var_Order~Region, scales="free_y",labeller=labeller(Region=reg_labels, Scen=scen_labels, Var_Order=var_labels)) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
RenovEffect
#
# ---- OUTPUTS ----
# png(file = "output/BuildStocks/Investments_UR.png", width = 6*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(Inv.UR)
# dev.off()
#
# png(file = "output/BuildStocks/EffLevel_TRS.png", width = 6*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(Eff.TRS)
# dev.off()
# 
# png(file = "output/BuildStocks/UEInt.png", width = 6*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(UEInt.SRT)
# dev.off()
# 
# png(file = "output/BuildStocks/RenovEffect.png", width = 10*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(RenovEffect)
# dev.off()
# 
# ---- FIG: Stocks ----
# Stocks.fig <- ggplot(data=subset(Stocks, !(variable=="Total")&TURQ==1), aes(x=Year,y = value, colour=variable)) + 
#   geom_line(alpha=1) + 
#   xlim(1971,2100) +
#   xlab("") + 
#   theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
#   theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeStrip, hjust=1), axis.text.y = element_text(size=FSizeStrip)) +
#   theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
#   theme(legend.position="bottom") +
#   scale_colour_manual(values=c("forestgreen","dodgerblue","firebrick1"),
#                       name="",
#                       breaks=c("New","Decomissioned","Total"),
#                       labels=c("New","Decomissioned","Total")) +
#   
#   facet_grid(Region~Scen, scales="free_y") 
# Stocks.fig

# png(file = "output/BuildStocks/Stocks.png", width = 6*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(Stocks.fig)
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