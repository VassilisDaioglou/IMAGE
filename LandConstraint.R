# R script to evaluate the effect of land constraint on biomass potentials
# Dray Yield-supply curves under different land constraints
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
# set higher RAM capacity for java (used in clsx package)
options(java.parameters = "-Xmx8000m")

# set directory path 
setwd("C:/Users/Asus/Documents/Github/Biomass_SSP_Scenarios/")
# Read Data Files for Yield-supply curves
PotGJ_NoConst=read.xlsx("data/Harper/BioI2TData.xlsx", sheet = 2, startRow=4)
PotGJ_Aban=read.xlsx("data/Harper/BioI2TData.xlsx", sheet = 4, startRow=4)
PotGJ_BioRes=read.xlsx("data/Harper/BioI2TData.xlsx", sheet = 6, startRow=4)
PotGJ_Degr=read.xlsx("data/Harper/BioI2TData.xlsx", sheet = 8, startRow=4)
PotGJ_WaterSh=read.xlsx("data/Harper/BioI2TData.xlsx", sheet = 10, startRow=4)
PotGJ_WetLand=read.xlsx("data/Harper/BioI2TData.xlsx", sheet = 12, startRow=4)
PotGJ_All=read.xlsx("data/Harper/BioI2TData.xlsx", sheet = 14, startRow=4)

YieldAgg_NoConst=read.xlsx("data/Harper/BioI2TData.xlsx", sheet = 3, startRow=4)
YieldAgg_Aban=read.xlsx("data/Harper/BioI2TData.xlsx", sheet = 5, startRow=4)
YieldAgg_BioRes=read.xlsx("data/Harper/BioI2TData.xlsx", sheet = 7, startRow=4)
YieldAgg_Degr=read.xlsx("data/Harper/BioI2TData.xlsx", sheet = 9, startRow=4)
YieldAgg_WaterSh=read.xlsx("data/Harper/BioI2TData.xlsx", sheet = 11, startRow=4)
YieldAgg_WetLand=read.xlsx("data/Harper/BioI2TData.xlsx", sheet = 13, startRow=4)
YieldAgg_All=read.xlsx("data/Harper/BioI2TData.xlsx", sheet = 15, startRow=4)

# Data files for land-supply curves
LandHa_WOODY_Cat=read.xlsx("data/Harper/BioI2TData2.xlsx", sheet = 2, startRow=4)
Yield_WOODY_Cat=read.xlsx("data/Harper/BioI2TData2.xlsx", sheet = 3, startRow=4)

LandHa_SUGAR_Cat=read.xlsx("data/Harper/BioI2TData2.xlsx", sheet = 4, startRow=4)
Yield_SUGAR_Cat=read.xlsx("data/Harper/BioI2TData2.xlsx", sheet = 5, startRow=4)

LandHa_MAIZE_Cat=read.xlsx("data/Harper/BioI2TData2.xlsx", sheet = 6, startRow=4)
Yield_MAIZE_Cat=read.xlsx("data/Harper/BioI2TData2.xlsx", sheet = 7, startRow=4)

LandHa_NWOOD_Cat=read.xlsx("data/Harper/BioI2TData2.xlsx", sheet = 8, startRow=4)
Yield_NWOOD_Cat=read.xlsx("data/Harper/BioI2TData2.xlsx", sheet = 9, startRow=4)

# Residues available potential
ResPot=read.xlsx("data/Harper/BioI2TData2.xlsx", sheet = 10, startRow=4)

# ---- CLEAN: Residues ----
ResPot=subset(ResPot, select=c(t,class_.27))
colnames(ResPot)[1:2] <- c("YEAR","Potential_GJ")
ResPot$Potential_EJ = ResPot$Potential_GJ/1e9
#
# ---- CLEAN: Yield-Supply ----
# Clean, arrange and merge files
PotGJ_NoConst$SCENARIO <- "NoConstraints"
PotGJ_Aban$SCENARIO <- "NoAbandoned"
PotGJ_BioRes$SCENARIO <- "NoBioReserve"
PotGJ_Degr$SCENARIO <- "NoDegraded"
PotGJ_WaterSh$SCENARIO <- "NoWaterShort"
PotGJ_WetLand$SCENARIO <- "NoWetLand"
PotGJ_All$SCENARIO <- "NoAll"

YieldAgg_NoConst$SCENARIO <- "NoConstraints"
YieldAgg_Aban$SCENARIO <- "NoAbandoned"
YieldAgg_BioRes$SCENARIO <- "NoBioReserve"
YieldAgg_Degr$SCENARIO <- "NoDegraded"
YieldAgg_WaterSh$SCENARIO <- "NoWaterShort"
YieldAgg_WetLand$SCENARIO <- "NoWetLand"
YieldAgg_All$SCENARIO <- "NoAll"

PotGJ = rbind(PotGJ_All,PotGJ_WetLand,PotGJ_WaterSh,PotGJ_Degr,PotGJ_BioRes,PotGJ_Aban,PotGJ_NoConst)
YieldAgg = rbind(YieldAgg_All,YieldAgg_WetLand,YieldAgg_WaterSh,YieldAgg_Degr,YieldAgg_BioRes,YieldAgg_Aban,YieldAgg_NoConst)

PotGJ$VARIABLE <- "Potential"
YieldAgg$VARIABLE <- "Yield"

PotGJ$UNIT <- "GJ"
YieldAgg$UNIT <- "GJ/Ha"

DATA = rbind(PotGJ,YieldAgg)

rm(PotGJ_All,PotGJ_WetLand,PotGJ_WaterSh,PotGJ_Degr,PotGJ_BioRes,PotGJ_Aban,PotGJ_NoConst)
rm(YieldAgg_All,YieldAgg_WetLand,YieldAgg_WaterSh,YieldAgg_Degr,YieldAgg_BioRes,YieldAgg_Aban,YieldAgg_NoConst)
rm(PotGJ,YieldAgg)

colnames(DATA)[1:7] <- c("YEAR","REGION","WOODY","MAIZE","SUGAR","OilCrop","NWOOD")
DATA=melt(DATA, id.vars=c("YEAR","REGION","SCENARIO","VARIABLE","UNIT"), na.rm=TRUE)
colnames(DATA)[6] <- "CROP"
DATA$YEAR = as.numeric(DATA$YEAR)
DATA$value = as.numeric(DATA$value)
DATA$REGION = as.numeric(DATA$REGION)

DATA2 = subset(DATA, select=-c(UNIT))
DATA2 = spread(DATA2, VARIABLE, value)

DATA2$Potential<-DATA2$Potential / 1000000000

DATA2$ScenOrder = factor(DATA2$SCENARIO, levels=c("NoConstraints","NoAbandoned","NoBioReserve","NoDegraded","NoWetLand","NoWaterShort","NoAll"))

# ---- CLEAN: Land-Supply ----
colnames(LandHa_WOODY_Cat)[1:10] <-c("YEAR","REGION","CATEGORY","NoConstraints","NoBioReserve","NoDegraded","NoWaterShort","NoWetLand","NoAbandoned","NoAll")
colnames(LandHa_SUGAR_Cat)[1:10] <-c("YEAR","REGION","CATEGORY","NoConstraints","NoBioReserve","NoDegraded","NoWaterShort","NoWetLand","NoAbandoned","NoAll")
colnames(LandHa_MAIZE_Cat)[1:10] <-c("YEAR","REGION","CATEGORY","NoConstraints","NoBioReserve","NoDegraded","NoWaterShort","NoWetLand","NoAbandoned","NoAll")
colnames(LandHa_NWOOD_Cat)[1:10] <-c("YEAR","REGION","CATEGORY","NoConstraints","NoBioReserve","NoDegraded","NoWaterShort","NoWetLand","NoAbandoned","NoAll")
colnames(Yield_WOODY_Cat)[1:10] <-c("YEAR","REGION","CATEGORY","NoConstraints","NoBioReserve","NoDegraded","NoWaterShort","NoWetLand","NoAbandoned","NoAll")
colnames(Yield_SUGAR_Cat)[1:10] <-c("YEAR","REGION","CATEGORY","NoConstraints","NoBioReserve","NoDegraded","NoWaterShort","NoWetLand","NoAbandoned","NoAll")
colnames(Yield_MAIZE_Cat)[1:10] <-c("YEAR","REGION","CATEGORY","NoConstraints","NoBioReserve","NoDegraded","NoWaterShort","NoWetLand","NoAbandoned","NoAll")
colnames(Yield_NWOOD_Cat)[1:10] <-c("YEAR","REGION","CATEGORY","NoConstraints","NoBioReserve","NoDegraded","NoWaterShort","NoWetLand","NoAbandoned","NoAll")

LandHa_WOODY_Cat = subset(LandHa_WOODY_Cat, YEAR==2100&!REGION==27)
LandHa_MAIZE_Cat = subset(LandHa_MAIZE_Cat, YEAR==2100&!REGION==27)
LandHa_SUGAR_Cat = subset(LandHa_SUGAR_Cat, YEAR==2100&!REGION==27)
LandHa_NWOOD_Cat = subset(LandHa_NWOOD_Cat, YEAR==2100&!REGION==27)
Yield_WOODY_Cat = subset(Yield_WOODY_Cat, YEAR==2100&!REGION==27)
Yield_MAIZE_Cat = subset(Yield_MAIZE_Cat, YEAR==2100&!REGION==27)
Yield_SUGAR_Cat = subset(Yield_SUGAR_Cat, YEAR==2100&!REGION==27)
Yield_NWOOD_Cat = subset(Yield_NWOOD_Cat, YEAR==2100&!REGION==27)

LandHa_WOODY_Cat = melt(LandHa_WOODY_Cat, id.vars=c("YEAR","REGION","CATEGORY"), variable.name="SCENARIO", na.rm=FALSE)
colnames(LandHa_WOODY_Cat)[5] <- "Land_Ha"
Yield_WOODY_Cat = melt(Yield_WOODY_Cat, id.vars=c("YEAR","REGION","CATEGORY"), variable.name="SCENARIO", na.rm=FALSE)
colnames(Yield_WOODY_Cat)[5] <- "Yield_GJHa"
WOODY_Cat = cbind(LandHa_WOODY_Cat,Yield_WOODY_Cat[5])
WOODY_Cat$CROP <- "WOODY"
WOODY_Cat_Sorted <- WOODY_Cat[with(WOODY_Cat, order(-Yield_GJHa)),] 
WOODY_Cat_Sorted = WOODY_Cat_Sorted %>% mutate(Potential_GJ = Land_Ha * Yield_GJHa)
WOODY_Cat_Sorted$CumPot_EJ <- ave(WOODY_Cat_Sorted$Potential_GJ, WOODY_Cat_Sorted$SCENARIO, FUN=cumsum)
WOODY_Cat_Sorted$CumPot_EJ <- WOODY_Cat_Sorted$CumPot_EJ / 1e9
WOODY_Cat_Sorted$CumLand_MHa <- ave(WOODY_Cat_Sorted$Land_Ha, WOODY_Cat_Sorted$SCENARIO, FUN=cumsum)
WOODY_Cat_Sorted$CumLand_MHa <- WOODY_Cat_Sorted$CumLand_MHa / 1e6

LandHa_NWOOD_Cat = melt(LandHa_NWOOD_Cat, id.vars=c("YEAR","REGION","CATEGORY"), variable.name="SCENARIO", na.rm=FALSE)
colnames(LandHa_NWOOD_Cat)[5] <- "Land_Ha"
Yield_NWOOD_Cat = melt(Yield_NWOOD_Cat, id.vars=c("YEAR","REGION","CATEGORY"), variable.name="SCENARIO", na.rm=FALSE)
colnames(Yield_NWOOD_Cat)[5] <- "Yield_GJHa"
NWOOD_Cat = cbind(LandHa_NWOOD_Cat,Yield_NWOOD_Cat[5])
NWOOD_Cat$CROP <- "NWOOD"
NWOOD_Cat_Sorted <- NWOOD_Cat[with(NWOOD_Cat, order(-Yield_GJHa)),] 
NWOOD_Cat_Sorted = NWOOD_Cat_Sorted %>% mutate(Potential_GJ = Land_Ha * Yield_GJHa)
NWOOD_Cat_Sorted$CumPot_EJ <- ave(NWOOD_Cat_Sorted$Potential_GJ, NWOOD_Cat_Sorted$SCENARIO, FUN=cumsum)
NWOOD_Cat_Sorted$CumPot_EJ <- NWOOD_Cat_Sorted$CumPot_EJ / 1e9
NWOOD_Cat_Sorted$CumLand_MHa <- ave(NWOOD_Cat_Sorted$Land_Ha, NWOOD_Cat_Sorted$SCENARIO, FUN=cumsum)
NWOOD_Cat_Sorted$CumLand_MHa <- NWOOD_Cat_Sorted$CumLand_MHa / 1e6

LandHa_MAIZE_Cat = melt(LandHa_MAIZE_Cat, id.vars=c("YEAR","REGION","CATEGORY"), variable.name="SCENARIO", na.rm=FALSE)
colnames(LandHa_MAIZE_Cat)[5] <- "Land_Ha"
Yield_MAIZE_Cat = melt(Yield_MAIZE_Cat, id.vars=c("YEAR","REGION","CATEGORY"), variable.name="SCENARIO", na.rm=FALSE)
colnames(Yield_MAIZE_Cat)[5] <- "Yield_GJHa"
MAIZE_Cat = cbind(LandHa_MAIZE_Cat,Yield_MAIZE_Cat[5])
MAIZE_Cat$CROP <- "MAIZE"
MAIZE_Cat_Sorted <- MAIZE_Cat[with(MAIZE_Cat, order(-Yield_GJHa)),] 
MAIZE_Cat_Sorted = MAIZE_Cat_Sorted %>% mutate(Potential_GJ = Land_Ha * Yield_GJHa)
MAIZE_Cat_Sorted$CumPot_EJ <- ave(MAIZE_Cat_Sorted$Potential_GJ, MAIZE_Cat_Sorted$SCENARIO, FUN=cumsum)
MAIZE_Cat_Sorted$CumPot_EJ <- MAIZE_Cat_Sorted$CumPot_EJ / 1e9
MAIZE_Cat_Sorted$CumLand_MHa <- ave(MAIZE_Cat_Sorted$Land_Ha, MAIZE_Cat_Sorted$SCENARIO, FUN=cumsum)
MAIZE_Cat_Sorted$CumLand_MHa <- MAIZE_Cat_Sorted$CumLand_MHa / 1e6

LandHa_SUGAR_Cat = melt(LandHa_SUGAR_Cat, id.vars=c("YEAR","REGION","CATEGORY"), variable.name="SCENARIO", na.rm=FALSE)
colnames(LandHa_SUGAR_Cat)[5] <- "Land_Ha"
Yield_SUGAR_Cat = melt(Yield_SUGAR_Cat, id.vars=c("YEAR","REGION","CATEGORY"), variable.name="SCENARIO", na.rm=FALSE)
colnames(Yield_SUGAR_Cat)[5] <- "Yield_GJHa"
SUGAR_Cat = cbind(LandHa_SUGAR_Cat,Yield_SUGAR_Cat[5])
SUGAR_Cat$CROP <- "SUGAR"
SUGAR_Cat_Sorted <- SUGAR_Cat[with(SUGAR_Cat, order(-Yield_GJHa)),] 
SUGAR_Cat_Sorted = SUGAR_Cat_Sorted %>% mutate(Potential_GJ = Land_Ha * Yield_GJHa)
SUGAR_Cat_Sorted$CumPot_EJ <- ave(SUGAR_Cat_Sorted$Potential_GJ, SUGAR_Cat_Sorted$SCENARIO, FUN=cumsum)
SUGAR_Cat_Sorted$CumPot_EJ <- SUGAR_Cat_Sorted$CumPot_EJ / 1e9
SUGAR_Cat_Sorted$CumLand_MHa <- ave(SUGAR_Cat_Sorted$Land_Ha, SUGAR_Cat_Sorted$SCENARIO, FUN=cumsum)
SUGAR_Cat_Sorted$CumLand_MHa <- SUGAR_Cat_Sorted$CumLand_MHa / 1e6

DATA_Land = rbind(WOODY_Cat_Sorted,SUGAR_Cat_Sorted,MAIZE_Cat_Sorted,NWOOD_Cat_Sorted)
DATA_Land$ScenOrder = factor(DATA_Land$SCENARIO, levels=c("NoConstraints","NoAbandoned","NoBioReserve","NoDegraded","NoWetLand","NoWaterShort","NoAll"))
DATA_Land = subset(DATA_Land, !Yield_GJHa==0)
DATA_Land$CumPot_EJ_Res = DATA_Land$CumPot_EJ + ResPot$Potential_EJ[ResPot$YEAR==2100]

rm(LandHa_WOODY_Cat,LandHa_SUGAR_Cat,LandHa_MAIZE_Cat,LandHa_NWOOD_Cat)
rm(Yield_WOODY_Cat,Yield_SUGAR_Cat,Yield_MAIZE_Cat,Yield_NWOOD_Cat)
rm(WOODY_Cat,SUGAR_Cat,MAIZE_Cat,NWOOD_Cat)
rm(WOODY_Cat_Sorted,SUGAR_Cat_Sorted,MAIZE_Cat_Sorted,NWOOD_Cat_Sorted)

# ---- LABELS ----
scen_labels <- c("NoConstraints"="No Land Constraints",
                "NoAbandoned"="Excl. (future) Abandoned Lands",
                "NoBioReserve"="Excl. Biodiversity Reserves",
                "NoDegraded"="Excl. Degraded Lands",
                "NoWetLand"="Excl. Wetlands",
                "NoWaterShort"="Excl. Water-short Areas",
                "NoAll"="Excl. All of the Above")
                
#
# ---- FIG: Yield-Supply Curves ----
DATA3 = subset(DATA2, YEAR>2015&REGION=="27")
DATA3 = subset(DATA3, !(CROP=="OilCrop"|SCENARIO=="NoDegraded"))

YieldSup <-ggplot(data=DATA3, aes(x=Yield, y=Potential, colour=ScenOrder, fill=ScenOrder)) + 
  geom_line(size=0.5)+
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ylim(0,150) +
  # Text
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(legend.title=element_text(size=FSizeLeg, face="bold"), legend.position="right", legend.text=element_text(size=FSizeLeg)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste("Potential ",EJ[Prim],"/yr",""))) +
  xlab(expression(paste("Yield ",GJ[Prim],"/Ha",""))) +
  # Legend
  scale_colour_manual(values=c("black","red","forestgreen","blue","cyan","firebrick"),
                      name ="Land Constraint:",
                      breaks=c("NoConstraints","NoAbandoned","NoBioReserve","NoWetLand","NoWaterShort","NoAll"),
                      labels=c("No Land Constraints","Excl. (future) Abandoned Lands","Excl. Biodiversity Reserves",
                               "Excl. Wetlands","Excl. Water-short Areas","Excl. All of the Above")
  ) +
  # scale_linetype_manual(values=c("twodash","solid"),
  #                       name ="Agricultural Commodity:",
  #                       breaks=c("AgriProdFood","AgriProdEnergyCrops"),
  #                       labels=c("Food","Energy Crops")
  # ) +
  facet_grid(. ~ CROP, labeller=labeller(ScenOrder=scen_labels),scales="free_y") +
  theme(strip.text.x = element_text(size = FSizeStrip), strip.text.y = element_text(size = FSizeStrip))
YieldSup

# #
# ---- FIG: Land-Supply Curves ----
DATA_Land = subset(DATA_Land, !SCENARIO=="NoDegraded")

LandSup <-ggplot(data=DATA_Land, aes(x=CumLand_MHa, y=CumPot_EJ_Res, colour=ScenOrder, fill=ScenOrder)) + 
  geom_line(size=0.3)+
  geom_hline(aes(yintercept=ResPot$Potential_EJ[ResPot$YEAR==2100], linetype='Residues'),size = 0.3, colour='black') +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ylim(0,220) +
  # Text
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(legend.title=element_text(size=FSizeLeg, face="bold"), legend.position="right", legend.text=element_text(size=FSizeLeg)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste("Potential - ",EJ[Prim],"/yr",""))) +
  xlab("Land Use - MHa") +
  # Legend
  scale_colour_manual(values=c("black","red","forestgreen","blue","cyan","firebrick"),
                      name ="Land Constraint:",
                      breaks=c("NoConstraints","NoAbandoned","NoBioReserve","NoWetLand","NoWaterShort","NoAll"),
                      labels=c("No Land Constraints","Excl. (future) Abandoned Lands","Excl. Biodiversity Reserves",
                               "Excl. Wetlands","Excl. Water-short Areas","Excl. All of the Above")
  ) +
  # Specifically for residues
  scale_linetype_manual(name = "Other Biomass:", values = 2, 
                        guide = guide_legend(override.aes = list(color = 'black'))) +
  
  facet_grid(. ~ CROP, labeller=labeller(ScenOrder=scen_labels),scales="free_y") +
  theme(strip.text.x = element_text(size = FSizeStrip), strip.text.y = element_text(size = FSizeStrip))
LandSup

# #
# # ---- OUTPUTS ----
# png(file = "output/hARPER/Yield_Supply.png", width = 8*ppi, height = 3*ppi, units = "px", res = ppi)
# plot(YieldSup)
# dev.off()
# 
# png(file = "output/Harper/Land_Supply.png", width = 6*ppi, height = 2*ppi, units = "px", res = ppi)
# plot(LandSup)
# dev.off()
# 





