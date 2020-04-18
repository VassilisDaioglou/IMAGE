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

rm(data_full,data_Demand,data_Floorspace,data_NoEffImp,data_none,data_NoRetrofit)
#
# ---- MUNGING ----
# Create Single Dataset
full = melt(full, id.vars=c("Model","Scenario","Region","Variable","Unit"))
none = melt(none, id.vars=c("Model","Scenario","Region","Variable","Unit"))
Demand = melt(Demand, id.vars=c("Model","Scenario","Region","Variable","Unit"))
Floorspace = melt(Floorspace, id.vars=c("Model","Scenario","Region","Variable","Unit"))
NoEffImp = melt(NoEffImp, id.vars=c("Model","Scenario","Region","Variable","Unit"))
NoRetrofit = melt(NoRetrofit, id.vars=c("Model","Scenario","Region","Variable","Unit"))

DATA = rbind(full,none,Demand,Floorspace,NoEffImp,NoRetrofit)
rm(full,none,Demand,Floorspace,NoEffImp,NoRetrofit)
DATA$Model <- NULL
colnames(DATA)[5] <- "Year"

# Rename Variables
DATA$Variable <- gsub("[[:punct:]]","",DATA$Variable,fixed=F)
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
DATA$Variable <- gsub("[[:space:]]","",DATA$Variable,fixed=F)

# Separate datasets
  # Final Energy
DATA.FE <- subset(DATA, Variable=="FECoolElec"|Variable=="FEHeatCoal"|Variable=="FEHeatElec"|Variable=="FEHeatGas"|Variable=="FEHeatHydrogen"
                 |Variable=="FEHeatModBio"|Variable=="FEHeatOil"|Variable=="FEHeatSecHeat"|Variable=="FEHeatTradBio")
DATA.FE$Prim <- DATA.FE$Variable
DATA.FE$Prim <- gsub("FECool","",DATA.FE$Prim,fixed=F)
DATA.FE$Prim <- gsub("FEHeat","",DATA.FE$Prim,fixed=F)
DATA.FE$Variable = substr(DATA.FE$Variable, start=1, stop=6)
DATA.FE <- DATA.FE[c("Scenario","Region","Year","Variable","Prim","Unit","value")]

DATA.FE <- spread(DATA.FE,Prim,value)
DATA.FE = DATA.FE %>% mutate(Total = Coal + Elec + Gas + Hydrogen + ModBio + Oil + SecHeat + TradBio)
DATA.FE = melt(DATA.FE, id.vars=c("Scenario","Region","Year","Variable","Unit"))
colnames(DATA.FE)[6] <- "Prim"
DATA.FE = subset(DATA.FE, !(Variable=="FECool"&(Prim=="Coal"|Prim=="Oil"|Prim=="Gas"|Prim=="TradBio"|
                                                  Prim=="ModBio"|Prim=="Hydrogen"|Prim=="SecHeat"|Prim=="Total")))
temp = subset(DATA.FE, Variable=="FECool"|(Variable=="FEHeat"&Prim=="Total"))
temp$Prim <- "Total"
temp = spread(temp,Variable,value)
temp = temp %>% mutate(FECoolHeat = FECool + FEHeat)
temp = melt(temp, id.vars=c("Scenario","Region","Year","Unit","Prim"))
colnames(temp)[6] <- "Variable"
  
DATA.FE = rbind(DATA.FE,temp)
rm(temp)

  # Carbon Contents
DATA.CC <- subset(DATA, Variable=="CCElec"|Variable=="CCHeat")

  # Floorspace
DATA.FS <- subset(DATA, Variable=="InsulFloorspaceLevel1"|Variable=="InsulFloorspaceLevel2"|Variable=="InsulFloorspaceLevel3"|
                    Variable=="InsulFloorspaceLevel4"|Variable=="InsulFloorspaceLevel5"|Variable=="InsulFloorspaceLevel6")
DATA.FS$InsulLevel <- DATA.FS$Variable
DATA.FS$InsulLevel <- gsub("InsulFloorspaceLevel","",DATA.FS$InsulLevel,fixed=F)
DATA.FS$Variable = substr(DATA.FS$Variable, start=6, stop=15)
DATA.FS <- DATA.FS[c("Scenario","Region","Year","Variable","InsulLevel","Unit","value")]

  # Investments
DATA.INV <- subset(DATA, Variable=="InvInsulRenov"|Variable=="InvInsulTotal")

  # Useful Energy
DATA.UE <- subset(DATA, Variable=="UEHeatCoolpc"|Variable=="UEHeatCoolpfs"|Variable=="UEHeatCool"|Variable=="UEIntHeat")

  # Normalise to 2020 value
# UEIntHeat$ID <- paste(UEIntHeat$Region,UEIntHeat$Scen,UEIntHeat$variable)
# UEIntHeat.2010 = subset(UEIntHeat, Year==2010)
# UEIntHeat$val_2010 <- UEIntHeat.2010[match(UEIntHeat$ID, UEIntHeat.2010$ID),"value"]
# rm(UEIntHeat.2010)
# UEIntHeat = UEIntHeat %>% mutate(Normalised_2010 = value/val_2010)
# 
# UEIntHeat = subset(UEIntHeat, select=-c(value,ID,val_2010))
# colnames(UEIntHeat)[5] <- "value"

#
# Stocks=subset(Stocks, Year %in% Years)
# InsulMS=subset(InsulMS, Year %in% Years)
# RenovRate=subset(RenovRate, Year %in% Years)
# UEIntHeat=subset(UEIntHeat, Year %in% Years)
# UEHeatCool_pc=subset(UEHeatCool_pc, Year %in% Years)
# CO2EmisHeatCool_pc=subset(CO2EmisHeatCool_pc, Year %in% Years)
# CostComponent=subset(CostComponent, Year %in% Years)

#
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
scen_labels <-c("Full"="Full",
                "none"="None",
                "Demand"="Demand",
                "Floorspace"="Constant \nFloorspace",
                "NoEffImp"="No Efficiency \nImprovement",
                "NoRetrofit"="No \nRetrofit",
                "SSP1_450"="SSP1 - 2°C",
                "SSP2_450"="Mitig.",
                "SSP1_20"="SSP1 - 1.5°C",
                "SSP2_20"="SSP2 - 1.5°C",
                "SSP2_450_NR"="Mitig. No Renov.",
                "SSP2_450_NFS"="Mitig. No Fuel Switching",
                "SSP2_450_NIR"="Mitig. No Insulation Improv.")

reg_labels <-c("BRA"="Brazil","CAN"="Canada","CEU"="Central Europe","CHN"="China+","EAF"="Eastern Africa",
               "INDIA"="India","INDO"="Indonesia","JAP"="Japan","KOR"="Korean Penunsila","ME"="Middle East",
               "MEX"="Mexico","NAF"="Northern Africa","OCE"="Oceania","RCAM"="Rest of Central America","RSAF"="Rest of Southern Africa",
               "RSAM"="Rest of Southern America","RSAS"="Rest of Southern Asia","RUS"="Russia","SAF"="South Africa","SEAS"="Southeast Asia",
               "STAN"="Kazakhstan +","TUR"="Turkey","UKR"="Ukraine","USA"="USA","WAF"="Western Africa","WEU"="Western Europe",
               "World"="Global")

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
