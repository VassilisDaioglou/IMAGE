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
library(ggpubr)

# ---- INPUTS: BioTool ----
ppi <- 300
FSizeStrip = 6.5
FSizeLeg = 6.5

# set directory path 
setwd("C:/Users/Asus/Documents/Github/Biomass_SSP_Scenarios/")
# Read Data Files
PotGJ_NoConst=read.xlsx("data/Harper/BioI2TData.xlsx", sheetName="BioPotGJCrop_NoConst", startRow=4)
PotGJ_Aban=read.xlsx("data/Harper/BioI2TData.xlsx", sheetName="BioPotGJCrop_Aban", startRow=4)
PotGJ_BioRes=read.xlsx("data/Harper/BioI2TData.xlsx", sheetName="BioPotGJCrop_BioRes", startRow=4)
PotGJ_Degr=read.xlsx("data/Harper/BioI2TData.xlsx", sheetName="BioPotGJCrop_Degr", startRow=4)
PotGJ_WaterSh=read.xlsx("data/Harper/BioI2TData.xlsx", sheetName="BioPotGJCrop_WaterSh", startRow=4)
PotGJ_WetLand=read.xlsx("data/Harper/BioI2TData.xlsx", sheetName="BioPotGJCrop_WetLand", startRow=4)
PotGJ_All=read.xlsx("data/Harper/BioI2TData.xlsx", sheetName="BioPotGJCrop_All", startRow=4)

YieldAgg_NoConst=read.xlsx("data/Harper/BioI2TData.xlsx", sheetName="YieldAgg_NoConst", startRow=4)
YieldAgg_Aban=read.xlsx("data/Harper/BioI2TData.xlsx", sheetName="YieldAgg_Aban", startRow=4)
YieldAgg_BioRes=read.xlsx("data/Harper/BioI2TData.xlsx", sheetName="YieldAgg_BioRes", startRow=4)
YieldAgg_Degr=read.xlsx("data/Harper/BioI2TData.xlsx", sheetName="YieldAgg_Degr", startRow=4)
YieldAgg_WaterSh=read.xlsx("data/Harper/BioI2TData.xlsx", sheetName="YieldAgg_WaterSh", startRow=4)
YieldAgg_WetLand=read.xlsx("data/Harper/BioI2TData.xlsx", sheetName="YieldAgg_WetLand", startRow=4)
YieldAgg_All=read.xlsx("data/Harper/BioI2TData.xlsx", sheetName="YieldAgg_All", startRow=4)

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

# ---- LABELS ----
scen_labels <- c("NoConstraints"="No Land Constraints",
                "NoAbandoned"="Excl. (future) Abandoned Lands",
                "NoBioReserve"="Excl. Biodiversity Reserves",
                "NoDegraded"="Excl. Degraded Lands",
                "NoWetLand"="Excl. Wetlands",
                "NoWaterShort"="Excl. Water-short Areas",
                "NoAll"="Excl. All of the Above")
                

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
# # ---- OUTPUTS ----
# png(file = "output/hARPER/Yield_Supply.png", width = 8*ppi, height = 3*ppi, units = "px", res = ppi)
# plot(YieldSup)
# dev.off()
# 





