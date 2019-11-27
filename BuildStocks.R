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
UEIntHeat=read.xlsx("data/BuildStocks/SSP2.xlsx", sheet = "UEintHeat_Fut", startRow=4)
CO2EmisHeatCool_pc=read.xlsx("data/BuildStocks/SSP2.xlsx", sheet = "CO2EmisHeatCool_pc", startRow=4)
CCElec=read.xlsx("data/BuildStocks/SSP2.xlsx", sheet = "CCElec", startRow=4)
CCSpaceHeat=read.xlsx("data/BuildStocks/SSP2.xlsx", sheet = "CCSpaceHeat", startRow=4)

  # Read Data Files for Mitigation Scenario
Investment_pc.450=read.xlsx("data/BuildStocks/SSP2_450.xlsx", sheet = "Investments_Total_pc", startRow=4)
EffMS_new.450=read.xlsx("data/BuildStocks/SSP2_450.xlsx", sheet = "MS_new", startRow=4)
RenovRate.450=read.xlsx("data/BuildStocks/SSP2_450.xlsx", sheet = "Renov_Rate", startRow=4)
UEHeatCool_pc.450=read.xlsx("data/BuildStocks/SSP2_450.xlsx", sheet = "UEHeatCool_pc", startRow=4)
UEIntHeat.450=read.xlsx("data/BuildStocks/SSP2_450.xlsx", sheet = "UEintHeat_Fut", startRow=4)
CO2EmisHeatCool_pc.450=read.xlsx("data/BuildStocks/SSP2_450.xlsx", sheet = "CO2EmisHeatCool_pc", startRow=4)
CCElec.450=read.xlsx("data/BuildStocks/SSP2_450.xlsx", sheet = "CCElec", startRow=4)
CCSpaceHeat.450=read.xlsx("data/BuildStocks/SSP2_450.xlsx", sheet = "CCSpaceHeat", startRow=4)

#
# ---- MUNGING ----
# ---- ***Investments ----
Investment_pc$Scen <- "SSP2"
Investment_pc.450$Scen <- "SSP2_450"
Investment_pc = rbind(Investment_pc,Investment_pc.450)
rm(Investment_pc.450)

colnames(Investment_pc)[1:16] <- c("Year","Region","Total","Urban","Rural",
                                "U1","U2","U3","U4","U5",
                                "R1","R2","R3","R4","R5",
                                "Scen")
Investment_pc = melt(Investment_pc, id.vars=c("Year","Region","Scen"))
  # Normalise to 2020 value
Investment_pc$ID <- paste(Investment_pc$Region,Investment_pc$Scen,Investment_pc$variable)
Investment_pc.2010 = subset(Investment_pc, Year==2010)
Investment_pc$val_2010 = Investment_pc.2010[match(Investment_pc$ID, Investment_pc.2010$ID),"value"]
rm(Investment_pc.2010)
Investment_pc = Investment_pc %>% mutate(Normalised_2010 = value/val_2010)

Investment_pc = subset(Investment_pc, select=-c(value,ID,val_2010))
colnames(Investment_pc)[5] <- "value"
#
# ---- ***Efficiency Market Shares ----
EffMS_new$Scen <- "SSP2"
EffMS_new.450$Scen <- "SSP2_450"
EffMS_new = rbind(EffMS_new,EffMS_new.450)
rm(EffMS_new.450)

colnames(EffMS_new)[1:8] <- c("Year","Region","TURQ","EffLevel","B1","B2","B3","B4")
EffMS_new = melt(EffMS_new, id.vars=c("Year","Region","TURQ","EffLevel","Scen"))
EffMS_new = subset(EffMS_new, variable=="B1")
EffMS_new$value <- as.numeric(EffMS_new$value)
EffMS_new$EffLevel <- as.character(EffMS_new$EffLevel)
#
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
#
# ---- ***Heating Useful Energy Intensity ----
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
# ---- ***Heating+Cooling Energy USe ----
UEHeatCool_pc$Scen <- "SSP2"
UEHeatCool_pc.450$Scen <- "SSP2_450"
UEHeatCool_pc = rbind(UEHeatCool_pc,UEHeatCool_pc.450)
rm(UEHeatCool_pc.450)

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
CO2EmisHeatCool_pc = rbind(CO2EmisHeatCool_pc,CO2EmisHeatCool_pc.450)
rm(CO2EmisHeatCool_pc.450)

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
CCSpaceHeat = rbind(CCSpaceHeat,CCSpaceHeat.450)
rm(CCSpaceHeat.450)

colnames(CCSpaceHeat)[1:3] <- c("Year","Region","Total")
CCSpaceHeat = subset(CCSpaceHeat, select = c(Year,Region,Total,Scen))
colnames(CCSpaceHeat)[3] <- "value"
CCSpaceHeat$variable <- "CCSpaceHeat"

  # Combined
CC = rbind(CCElec,CCSpaceHeat)
rm(CCElec,CCSpaceHeat)
#
# ---- DATA AGGREGATION ----
Investment_pc=subset(Investment_pc, Year %in% Years)
EffMS_new=subset(EffMS_new, Year %in% Years)
RenovRate=subset(RenovRate, Year %in% Years)
UEIntHeat=subset(UEIntHeat, Year %in% Years)
UEHeatCool_pc=subset(UEHeatCool_pc, Year %in% Years)
CO2EmisHeatCool_pc=subset(CO2EmisHeatCool_pc, Year %in% Years)

Investment_pc$Variable <- "Investments_pc"
RenovRate$Variable <- "RenovationRate"
UEIntHeat$Variable <- "UeIntHeat"
UEHeatCool_pc$Variable <- "HeatCoolDemand_pc"
CO2EmisHeatCool_pc$Variable <- "ResiCO2EmisHeatCool"

DATA.TRQS = rbind(Investment_pc,RenovRate,UEIntHeat,UEHeatCool_pc,CO2EmisHeatCool_pc) 
colnames(DATA.TRQS)[4] <- "TURQ"

DATA.TRQS$Var_Order <- factor(DATA.TRQS$Variable, level=c("Investments_pc",
                                                          "RenovationRate",
                                                          "UeIntHeat",
                                                          "HeatCoolDemand_pc",
                                                          "ResiCO2EmisHeatCool"))

DATA.TS = subset(DATA.TRQS, Region == 27&TURQ == "Total")
DATA.TS$TURQ <- NULL
DATA.TRQS = subset(DATA.TRQS, Region %in% Regions)

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

var_labels <-c("Investments_pc"="Investments \n(2010=1)",
               "RenovationRate"="Renovation \nRate \n(%)",
               "UeIntHeat"="Heating \nIntensity \n(2010=1)",
               "HeatCoolDemand_pc"="Heating & Cooling \nDemand \n(2010=1)",
               "ResiCO2EmisHeatCool"="Heating & Cooling \nEmissions \n(2010=1)")

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