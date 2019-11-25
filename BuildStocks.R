# R script to process the results of the TIMER Building Stocks & Renovation represetnation
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

# ---- INPUTS: BioTool ----
ppi <- 300
FSizeStrip = 6.5
FSizeLeg = 6.5
FSizeAxis = 9

  # set higher RAM capacity for java (used in clsx package)
options(java.parameters = "-Xmx8000m")

  # set directory path 
setwd("C:/Users/Asus/Documents/Github/Biomass_SSP_Scenarios/")
  # Read Data Files for Baseline Scenario
Stocks=read.xlsx("data/BuildStocks/SSP2.xlsx", sheet = "StockFlows", startRow=4)
Investment=read.xlsx("data/BuildStocks/SSP2.xlsx", sheet = "Investments_Total", startRow=4)
# Investment.Renov=read.xlsx("data/BuildStocks/SSP2.xlsx", sheet = "Investments_RenovBuild", startRow=4)
EffMS.new=read.xlsx("data/BuildStocks/SSP2.xlsx", sheet = "MS_new", startRow=4)
RenovRate=read.xlsx("data/BuildStocks/SSP2.xlsx", sheet = "Renov_Rate", startRow=4)
EneUseFunc=read.xlsx("data/BuildStocks/SSP2.xlsx", sheet = "EnUseFunction_TURQ", startRow=4)
UEIntHeat=read.xlsx("data/BuildStocks/SSP2.xlsx", sheet = "UEIntHeat_new_Fut", startRow=4)
CO2Emis=read.xlsx("data/BuildStocks/SSP2.xlsx", sheet = "CO2Spec", startRow=4)

  # Read Data Files for Mitigation Scenario
# Stocks=read.xlsx("data/BuildStocks/SSP2_450.xlsx", sheet = "StockFlows", startRow=4)
Investment.450=read.xlsx("data/BuildStocks/SSP2_450.xlsx", sheet = "Investments_Total", startRow=4)
# Investment.Renov=read.xlsx("data/BuildStocks/SSP2_450.xlsx", sheet = "Investments_RenovBuild", startRow=4)
EffMS.new450=read.xlsx("data/BuildStocks/SSP2_450.xlsx", sheet = "MS_new", startRow=4)
RenovRate.450=read.xlsx("data/BuildStocks/SSP2_450.xlsx", sheet = "Renov_Rate", startRow=4)
EneUseFunc.450=read.xlsx("data/BuildStocks/SSP2_450.xlsx", sheet = "EnUseFunction_TURQ", startRow=4)
UEIntHeat.450=read.xlsx("data/BuildStocks/SSP2_450.xlsx", sheet = "UEIntHeat_new_Fut", startRow=4)
CO2Emis.450=read.xlsx("data/BuildStocks/SSP2_450.xlsx", sheet = "CO2Spec", startRow=4)

#
# ---- MUNGING ----
# ---- ***Stocks ----
colnames(Stocks)[1:6] <- c("Year","Region","TURQ","New","Decomissioned","Total")
Stocks = melt(Stocks, id.vars=c("Year","Region","TURQ"))

# ---- ***Investments ----
colnames(Investment)[1:15] <- c("Year","Region","Total","Urban","Rural",
                            "U1","U2","U3","U4","U5",
                            "R1","R2","R3","R4","R5")
Investment = melt(Investment, id.vars=c("Year","Region"))
Investment = subset(Investment, Year=="1980"|Year=="1990"|Year=="2000"|Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050"|Year=="2060"|Year=="2070"|Year=="2080"|Year=="2090"|Year=="2100")
Investment$Scen <- "SSP2"

colnames(Investment.450)[1:15] <- c("Year","Region","Total","Urban","Rural",
                                "U1","U2","U3","U4","U5",
                                "R1","R2","R3","R4","R5")
Investment.450 = melt(Investment.450, id.vars=c("Year","Region"))
Investment.450 = subset(Investment.450, Year=="1980"|Year=="1990"|Year=="2000"|Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050"|Year=="2060"|Year=="2070"|Year=="2080"|Year=="2090"|Year=="2100")
Investment.450$Scen <- "SSP2_450"

Investment = rbind(Investment,Investment.450)
rm(Investment.450)

# ---- ***Efficiency Market Shares ----
EffMS.new$Scen <- "SSP2"
EffMS.new450$Scen <- "SSP2_450"
EffMS.new = rbind(EffMS.new,EffMS.new450)
rm(EffMS.new450)

colnames(EffMS.new)[1:8] <- c("Year","Region","TURQ","EffLevel","B1","B2","B3","B4")
EffMS.new = melt(EffMS.new, id.vars=c("Year","Region","TURQ","EffLevel","Scen"))
EffMS.new = subset(EffMS.new, Year=="1980"|Year=="1990"|Year=="2000"|Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050"|Year=="2060"|Year=="2070"|Year=="2080"|Year=="2090"|Year=="2100")
EffMS.new = subset(EffMS.new, variable=="B1")
EffMS.new = subset(EffMS.new, !Region==27)
EffMS.new$value <- as.numeric(EffMS.new$value)

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
RenovRate = subset(RenovRate, Year=="1980"|Year=="1990"|Year=="2000"|Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050"|Year=="2060"|Year=="2070"|Year=="2080"|Year=="2090"|Year=="2100")

# ---- ***Heating USeful Energy Intensity ----
UEIntHeat$Scen <- "SSP2"
UEIntHeat.450$Scen <- "SSP2_450"
UEIntHeat = rbind(UEIntHeat,UEIntHeat.450)
rm(UEIntHeat.450)

colnames(UEIntHeat)[1:15] <- c("Year","Region","Total","Urban","Rural",
                               "U1","U2","U3","U4","U5",
                               "R1","R2","R3","R4","R5")
UEIntHeat = melt(UEIntHeat, id.vars=c("Year","Region","Scen"))
UEIntHeat = subset(UEIntHeat, Year=="1980"|Year=="1990"|Year=="2000"|Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050"|Year=="2060"|Year=="2070"|Year=="2080"|Year=="2090"|Year=="2100")

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
EneUseFunc = subset(EneUseFunc, Year=="1980"|Year=="1990"|Year=="2000"|Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050"|Year=="2060"|Year=="2070"|Year=="2080"|Year=="2090"|Year=="2100")
EneUseFunc = subset(EneUseFunc, !Region==27)
EneUseFunc$Region[EneUseFunc$Region==28] <- 27

# ---- ***CO2 Emissions ----
CO2Emis$Scen <- "SSP2"
CO2Emis.450$Scen <- "SSP2_450"
CO2Emis = rbind(CO2Emis,CO2Emis.450)
rm(CO2Emis.450)

colnames(CO2Emis)[1:2] <- c("Year","Region")
CO2Emis = subset(CO2Emis, select=c(Year,Region,class_.5,Scen))
colnames(CO2Emis)[3] <- c("value")
CO2Emis = subset(CO2Emis, Year=="1980"|Year=="1990"|Year=="2000"|Year=="2010"|Year=="2020"|Year=="2030"|Year=="2040"|Year=="2050"|Year=="2060"|Year=="2070"|Year=="2080"|Year=="2090"|Year=="2100")
CO2Emis = subset(CO2Emis, !Region==27)
CO2Emis$Region[CO2Emis$Region==28] <- 27
CO2Emis$variable <- "Total"

#
# ---- DATA AGGREGATION
Investment$Variable <- "Investments"
RenovRate$Variable <- "RenovationRate"
UEIntHeat$Variable <- "UeIntHeat"
EneUseFunc$Variable <- "HeatingDemand"
CO2Emis$Variable <- "ResiCO2Emis"

DATA.TRQS = rbind(Investment,RenovRate,UEIntHeat,EneUseFunc,CO2Emis) 
colnames(DATA.TRQS)[3] <- "TURQ"
# ---- FIGURES ----
# ---- FIG: Investments ----
Inv.S <- ggplot(data=subset(Investment, (variable=="Urban"|variable=="Rural")&Region==27)
                  , aes(x=Year,y = value, fill=variable)) + 
  geom_bar(stat="identity") +
  # geom_line(alpha=1) + 
  xlim(2020,2100) +
  xlab("") + ylab("Bn$/yr") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeStrip, hjust=1), axis.text.y = element_text(size=FSizeStrip)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  # scale_colour_manual(values=c("forestgreen","dodgerblue","firebrick1"),
  #                     name="",
  #                     breaks=c("New","Decomissioned","Total"),
  #                     labels=c("New","Decomissioned","Total")) +
  # 
  facet_wrap(Scen~.) 
Inv.S

Inv.R <- ggplot(data=subset(Investment, Scen=="SSP2_450"&!(variable=="Total"|variable=="Urban"|variable=="Rural"))
                , aes(x=Year,y = value, fill=variable)) + 
  geom_bar(stat="identity") +
  xlim(2020,2100) +
  xlab("") + ylab("Bn$/yr") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeStrip, hjust=1), axis.text.y = element_text(size=FSizeStrip)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  facet_wrap(Region~., scales="free_y") 
Inv.R


#
# ---- FIG: Efficiency Level ----
Eff.UQ <- ggplot(data=subset(EffMS.new, (TURQ==4|TURQ==5|TURQ==6|TURQ==7|TURQ==8)&Region==1)
                , aes(x=Year,y = value, fill=EffLevel)) + 
  geom_bar(stat="identity") +
  xlim(2020,2100) +
  xlab("") + ylab("") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeStrip, hjust=1), axis.text.y = element_text(size=FSizeStrip)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  # scale_colour_manual(values=c("forestgreen","dodgerblue","firebrick1"),
  #                     name="",
  #                     breaks=c("New","Decomissioned","Total"),
  #                     labels=c("New","Decomissioned","Total")) +
  # 
  facet_grid(TURQ~Scen)
Eff.UQ

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
  facet_wrap(.~Region) 
UEInt.SRT
#
# ---- FIG: Combined Results ----
RenovEffect <- ggplot(data=subset(DATA.TRQS, Region==27&TURQ=="Total")
                    , aes(x=Year,y = value, colour=Scen)) + 
  geom_line(alpha=1) +
  xlim(1980,2100) +
  xlab("") + ylab("") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeStrip, hjust=1), axis.text.y = element_text(size=FSizeStrip)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  # scale_colour_manual(values=c("forestgreen","dodgerblue","firebrick1"),
  #                     name="",
  #                     breaks=c("New","Decomissioned","Total"),
  #                     labels=c("New","Decomissioned","Total")) +
  # 
  facet_wrap(.~Variable, nrow=1, scales="free_y") 
RenovEffect
#

# ---- OUTPUTS ----
# png(file = "output/BuildStocks/InvestmentsR.png", width = 6*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(Inv.R)
# dev.off()
#
# png(file = "output/BuildStocks/EffLevel.png", width = 6*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(Eff.UQ)
# dev.off()
#
# png(file = "output/BuildStocks/UEInt.png", width = 6*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(UEInt.SRT)
# dev.off()
# 
# png(file = "output/BuildStocks/RenovEffect.png", width = 6*ppi, height = 2*ppi, units = "px", res = ppi)
# plot(RenovEffect)
# dev.off()
# 
# ---- FIG: Stocks ----
Stocks.fig <- ggplot(data=subset(Stocks, !(variable=="Total")&TURQ==1&Region==20), aes(x=Year,y = value, colour=variable)) + 
  geom_line(alpha=1) + 
  xlim(1971,2100) +
  xlab("") + 
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeStrip, hjust=1), axis.text.y = element_text(size=FSizeStrip)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  scale_colour_manual(values=c("forestgreen","dodgerblue","firebrick1"),
                      name="",
                      breaks=c("New","Decomissioned","Total"),
                      labels=c("New","Decomissioned","Total")) +
  
  facet_wrap(Region~., scales="free_y") 
Stocks.fig

# png(file = "output/BuildStocks/Stocks.png", width = 6*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(Stocks.fig)
# dev.off()
#
#



# ---- FIG: Renovation Rate ----
RR.T <- ggplot(data=subset(RenovRate, variable=="Total")
               , aes(x=Region,y = value, fill=Region)) + 
  geom_bar(stat="identity") +
  xlab("") + ylab("") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeStrip, hjust=1), axis.text.y = element_text(size=FSizeStrip)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom") +
  # scale_colour_manual(values=c("forestgreen","dodgerblue","firebrick1"),
  #                     name="",
  #                     breaks=c("New","Decomissioned","Total"),
  #                     labels=c("New","Decomissioned","Total")) +
  # 
  facet_grid(.~Scen)
RR.T

# png(file = "output/BuildStocks/RenovRate.png", width = 6*ppi, height = 6*ppi, units = "px", res = ppi)
# plot(RR.T)
# dev.off()
# 

#
