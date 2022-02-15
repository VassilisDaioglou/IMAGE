# R script to produce figures to be used in BioBased Economy Lecure
# Utrecht University
# ---- START ----
# clear memory
rm(list=ls()) 

# sit higher RAM capacity for java (used in clsx package)
options(java.parameters = "-Xmx8000m")
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
library(gridExtra)
library(gdata)

# ---- CONSTANTS ----
ppi <- 300
FSizeTitle = 10
FSizeStrip = 9
FSizeAxis = 9
FSizeLeg = 9

ppi <- 300
CH42CO2 <- 28 # 100YR GWP, Myhre, G., D. Sindell, F.-M. BrÃ©on, W. Collins, J. Fuglestvedt, J. Huang, D. Koch, J.-F. Lamarque, D. Lee, B. Mendoza, T. Nakajima, A. Robock, G. Stephens, T. Takemura and H. Zhang, 2013: Anthropogenic and Natural Radiative Forcing. In: Climate Change 2013: The Physical Science Basis. Contribution of Working Group I to the Fifth Assessment Report of the Intergovernmental Panel on Climate Change [Stocker, T.F., D. Qin, G.-K. Plattner, M. Tignor, S.K.Allen, J. Boschung, A. Nauels, Y. Xia, V. Bex and P.M. Midgley(eds.)]. Cambridge University Press, Cambridge, United Kingdom and New York, NY, USA.
N2O2CO2 <- 265 # 100YR GWP, Myhre, G., D. Sindell, F.-M. BrÃ©on, W. Collins, J. Fuglestvedt, J. Huang, D. Koch, J.-F. Lamarque, D. Lee, B. Mendoza, T. Nakajima, A. Robock, G. Stephens, T. Takemura and H. Zhang, 2013: Anthropogenic and Natural Radiative Forcing. In: Climate Change 2013: The Physical Science Basis. Contribution of Working Group I to the Fifth Assessment Report of the Intergovernmental Panel on Climate Change [Stocker, T.F., D. Qin, G.-K. Plattner, M. Tignor, S.K.Allen, J. Boschung, A. Nauels, Y. Xia, V. Bex and P.M. Midgley(eds.)]. Cambridge University Press, Cambridge, United Kingdom and New York, NY, USA.

IMAGERegion = as.data.frame(rbind(c(1,"CAN"),c(2,"USA"),c(3,"MEX"),c(4,"RCAM"),c(5,"BRA"),c(6,"RSAM"),c(7,"NAF"),c(8,"WAF"),c(9,"EAF"),c(10,"SAF"),c(11,"WEU"),c(12,"CEU"),c(13,"TUR"),c(14,"UKR"),c(15,"STAN"),
                                  c(16,"RUS"),c(17,"ME"),c(18,"INDIA"),c(19,"KOR"),c(20,"CHN"),c(21,"SEAS"),c(22,"INDO"),c(23,"JAP"),c(24,"OCE"),c(25,"RSAS"),c(26,"RSAF"),c(27,"World")))

Scenarios = c("SSP1","SSP2","SSP3",
              "SSP1_SPA1_19I_D", "SSP2_SPA2_19I_D", "SSP3_MT")

Scenarios_base = c("SSP1","SSP2","SSP3")

Scenarios_mitig = c("SSP1_SPA1_19I_D", "SSP2_SPA2_19I_D", "SSP3_MT")

ActiveYears = c("2010","2020","2030","2040","2050","2060","2070","2080","2090","2100")
ActiveYears2 = c("2030","2050","2075","2100")

# ---- INPUTS: IMAGE ----
# set directory path 
# setwd("~/disks/y/ontwapps/Timer/Users/Vassilis/Papers/6. Synthesis/For Submission/Data Processing - R")
setwd("C:/Users/Asus/Documents/GitHub/IMAGE")

# Read Data File
SSP1=read.xlsx("data/BBE/SSP1.xlsx", sheet="data")
SSP2=read.xlsx("data/BBE/SSP2.xlsx", sheet="data")
SSP3=read.xlsx("data/BBE/SSP3.xlsx", sheet="data")
SSP1.19=read.xlsx("data/BBE/SSP1_SPA1_19I_D.xlsx", sheet="data")
SSP2.19=read.xlsx("data/BBE/SSP2_SPA2_19I_D.xlsx", sheet="data")
SSP3.MT=read.xlsx("data/BBE/SSP3_MT.xlsx", sheet="data")

# Create Single Dataset
SSP=rbind(SSP1,SSP2,SSP3,
          SSP1.19,SSP2.19,SSP3.MT)

rm(SSP1, SSP1.19, SSP2, SSP2.19, SSP3, SSP3.MT)
#
# ---- MUNGING ----
SSP <- SSP %>%
  melt(id.vars=c("Model","Scenario","Region","Variable","Unit")) %>%
  subset(select=-Model) %>%
  rename(Year = variable) %>%
  mutate(ScenType = replace(Scenario, Scenario %in% Scenarios_base,"Baseline")) %>%
  mutate(ScenType = replace(ScenType, ScenType %in% Scenarios_mitig,"Mitigation"))

SSP <- SSP %>%
  # Rename Variables
  mutate(Variable = gsub("[[:punct:]]", "", Variable)) %>%
  mutate(Variable = gsub("Production","Prod", Variable)) %>%
  mutate(Variable = gsub("Demand","Dem", Variable)) %>%
  mutate(Variable = gsub("Supply","Sup", Variable)) %>%
  # Energy Vars
  mutate(Variable = gsub("Primary Energy","PE", Variable)) %>%
  mutate(Variable = gsub("Final Energy","FE", Variable)) %>%
  mutate(Variable = gsub("Secondary Energy","SE", Variable)) %>%
  mutate(Variable = gsub("Non-Energy Use","Feedstocks", Variable)) %>%
  mutate(Variable = gsub("Electricity","Elec", Variable)) %>%
  mutate(Variable = gsub("BiomassModern","MBio", Variable)) %>%
  mutate(Variable = gsub("BiomassTraditional","TradBio", Variable)) %>%
  mutate(Variable = gsub("Energy","Ene", Variable)) %>%
  mutate(Variable = gsub("Industry","Indu", Variable)) %>%
  mutate(Variable = gsub("Transportation","Trans", Variable)) %>%
  mutate(Variable = gsub("Residential","Resi", Variable)) %>%
  mutate(Variable = gsub("Commercial","Serv", Variable)) %>%
  mutate(Variable = gsub("Renewables","Ren", Variable)) %>%
  # Emissions
  mutate(Variable = gsub("Emissions","Emis", Variable)) %>%
  mutate(Variable = gsub("CH4Ene Sup and Dem", "CH4Ene", Variable)) %>%
  mutate(Variable = gsub("N2OEne Sup and Dem", "N2OEne", Variable)) %>%
  mutate(Variable = gsub("LandUse", "LU", Variable)) %>%
  # Other
  mutate(Variable = gsub("[[:space:]]","", Variable)) %>%
  mutate(Variable = gsub("Bioenergy2ndgeneration","Biomass2ndGen", Variable)) %>%
  mutate(Variable = gsub("FENonEneUseLiquidsBiomass","SENonEnBiomass", Variable)) %>%
  mutate(Year = as.numeric(substr(Year, start=1, stop=4))) %>%
  mutate(ScenOrder = factor(Scenario, levels =c("SSP1","SSP2","SSP3",
                                                "SSP1_SPA1_19I_D","SSP2_SPA2_19I_D","SSP3_MT"))) 

#
# SEPARATE DATASETS
# ---- ***Primary Energy (PE)*** ----
DATA.PE <- subset(SSP, 
                  Variable=="PEBiomass"|Variable=="PEBiomassEneCrops"|Variable=="PEMBio"|Variable=="PETradBio"
                  |Variable=="PECoal"|Variable=="PEGas"|Variable=="PEOil"
                  |Variable=="PENonBiomassRen")

DATA.PE$VarOrder = factor(DATA.PE$Variable, levels =c("PENonBiomassRen","PEMBio","PETradBio","PEGas","PEOil","PECoal","PEBiomass","PEBiomassEneCrops"))

#
# ---- ***Secondary Energy (SE)*** ----
DATA.SE <- subset(SSP, Variable=="SE"
                  |Variable=="SEElecBiomass"|Variable=="SEElecBiomasswCCS"|Variable=="SEElecBiomasswoCCS"
                  |Variable=="SEHeatBiomass"|Variable=="SENonEnBiomass"
                  |Variable=="SEHydrogenBiomass"|Variable=="SEHydrogenBiomasswCCS"|Variable=="SEHydrogenBiomasswoCCS"
                  |Variable=="SELiquidsBiofuels"|Variable=="SELiquidsBiomasswCCS"|Variable=="SELiquidsBiomasswoCCS"
                  |Variable=="SESolidsBiomass")

DATA.SE$value[DATA.SE$Variable=="SENonEnBiomass"] <- (DATA.SE$value[DATA.SE$Variable=="SENonEnBiomass"]/1000)

DATA.SE$VarOrder = factor(DATA.SE$Variable, levels =c("SENonEnBiomass","SEElecBiomass","SEHeatBiomass","SEHydrogenBiomass","SELiquidsBiofuels","SESolidsBiomass"))

#
# ---- ***Final Energy (FE)*** ----
DATA.FE <- subset(SSP, 
                  Variable=="FEIndu"|Variable=="FETrans"|Variable=="FEResi"|Variable=="FEServ"|Variable=="FEOtherSector"
                  )

DATA.FE$VarOrder = factor(DATA.FE$Variable, levels =c("FEIndu","FETrans","FEResi","FEServ","FEOtherSector"))

# ---- ***Emissions (EM)*** ----
DATA.EM <- subset(SSP, Variable=="EmisCO2Ene"
                  |Variable=="EmisCO2EneDem"|Variable=="EmisCO2EneSup"
                  |Variable=="EmisCO2LU"
                  |Variable=="EmisCH4Ene"|Variable=="EmisN2OEne")

DATA.EM$VarOrder = factor(DATA.EM$Variable, levels =c("EmisCO2Ene","EmisCO2EneDem","EmisCO2EneSup","EmisCO2LU",
                                                      "EmisCH4Ene","EmisN2OEne"))

#
# ---- **** Global Results **** ----
Glob.SE = subset(DATA.SE, Region == "World" & (Year == "2050"|Year == "2100") & (Scenario == "SSP2" | Scenario == "SSP2_SPA2_19I_D"))

Glob.PE = subset(DATA.PE, Region == "World" & (Year == "2050"|Year == "2100") & (Scenario == "SSP2" | Scenario == "SSP2_SPA2_19I_D"))

#
# ---- LABELS ----
reg_labels <-c("CAN"="Canada", "USA"="USA", "MEX"="Mexico", "RCAM"="Rest of \nCentral America", "BRA"="Brazil",
               "RSAM"="Rest of \nSouthern America", "NAF"="Northern Africa", "WAF"="Western Africa", "EAF"="Eastern Africa", "SAF"="South Africa",
               "WEU"="Western Europe", "CEU"="Central Europe", "TUR"="Turkey", "UKR"="Ukraine", "STAN"="Kazakhstan +",
               "RUS"="Russia", "ME"="Middle East", "INDIA"="India", "KOR"="Korean Peninsula", "CHN"="China+", 
               "SEAS"="Southeast Asia", "INDO"="Indonesia","JAP"="Japan", "OCE"="Oceania", "RSAS"="Rest of \nSouthern Asia",
               "RSAF"="Rest of \nSouthern Africa", "World"="Global")

scen_labels <-c("SSP1"="SSP1",
                "SSP2"="SSP2",
                "SSP3"="SSP3",
                "SSP1_SPA1_19I_D"="SSP1 \n1.5°C",
                "SSP2_SPA2_19I_D"="SSP2 \n1.5°C",
                "SSP3_MT"="SSP3 \n(Maximum Mitigation)")

var_labels <-c("PEBiomass"="Biomass","PEBiomassEneCrops"="Energy Crops",
               "PEMBio"="Modern Biomass","PETradBio"="Traditional Biomass",
               "PECoal"="Coal","PEGas"="Natural Gas","PEOil"="Oil",
               "SE"="Secondary Energy",
               "SENonEnBiomass"="Feedstocks",
               "SEElecBiomass"="Electricity","SEElecBiomasswCCS"="Electricity with CCS","SEElecBiomasswoCCS"="Electricity without CCS",
               "SEHeatBiomass"="Heat Production",
               "SEHydrogenBiomass"="Hydrogen","SEHydrogenBiomasswCCS"="Hydrogen with CCS","SEHydrogenBiomasswoCCS"="Hydrogen without CCS",
               "SELiquidsBiofuels"="Liquid Biofuels","SELiquidsBiomasswCCS"="Liquid Biofuels with CCS","SELiquidsBiomasswoCCS"="Liquid Biofuels without CCS",
               "SESolidsBiomass"="Solids (pellets)",
               "FEIndu"="Industry",
               "FEOtherSector"="Other",
               "FERes"="Residential",
               "FEServ"="Services",
               "FETrans"="Transport",
               "EmisCH4Ene"=expression(paste("Energy CH"[4])),
               "EmisCO2Ene"=expression(paste("Energy CO"[2])),
               "EmisCO2EneDem"=expression(paste("Energy Demand CO"[2])),
               "EmisCO2EneSup"=expression(paste("Energy Supply CO"[2])),
               "EmisCO2LU"=expression(paste("Land-use CO"[2])),
               "EmisN2OEne"=expression(paste("Energy N"[2],"O")))

unit_labels <-c("PJ/yr" = "PJ/yr",
                "EJ/yr" = "EJ/yr",
                "Mt CO2eq/yr" = expression(paste("MtCO"[2],"-eq/yr")),
                "Mt CO2/yr" = expression(paste("MtCO"[2],"-eq/yr")),
                "t DM/ha/yr" = expression(paste("t"[DM],"/ha/yr")))

#
# ---- COLOURS ----
var_colours.df <- data.frame(Variable = c("FEIndu","FETrans","FEResi","FEServ","FEOtherSector",
                                          "PENonBiomassRen","PEMBio","PETradBio","PEGas","PEOil","PECoal","PEBiomass","PEBiomassEneCrops",
                                          "EmisCO2Ene","EmisCO2EneDem","EmisCO2EneSup","EmisCO2LU","EmisCH4Ene","EmisN2OEne",
                                          "SENonEnBiomass","SEElecBiomass","SEHeatBiomass","SEHydrogenBiomass","SELiquidsBiofuels","SESolidsBiomass"),
                             label = c("Industry","Transport","Residential","Services","Other",
                                       "Non-Biomass Renewables","Modern Biomass","Traditional Biomass","Natural Gas","Oil","Coall","Biomass","Energy Crops",
                                       "CO2 Energy","CO2 (Energy Demand)","CO2 (Energy Supply)","CO2 (Land-Use)","CH4 (Energy)","N2O (Energy)",
                                       "Chemicals","Electricity","Heat production","Hydrogen","Liquids (biofuels)","Solids (pellets)"),
                             Colour = c("gray","hotpink","tomato4","dodgerblue","yellow3",
                                        "gray","forestgreen","tomato4","pink","dodgerblue","black","forestgreen","gorestgreen",
                                        "tomato4","dodgerblue","tomato4","forestgreen","goldenrod1","purple",
                                        "purple","gray","goldenrod1","dodgerblue","darkblue","tomato4"))

var_colours.df$Colour <- as.character(var_colours.df$Colour)

#
# ---- FIGURES ----
# ---- Final Energy - SSP2 - Column----
FE_Sector_SSP2 <- 
  ggplot(data=subset(DATA.FE, Year %in% ActiveYears & Region=="World" & Scenario == "SSP2"),
         aes(x=Year,y = value, fill=VarOrder)) + 
  geom_bar(colour="black", stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  labs(x = "",
       y = expression(paste("Final Enegy [EJ"[Final],"/yr]")),
       title = "Final Energy per Demand Sector")+
  theme_bw() +
  theme(plot.title = element_text(size = FSizeTitle, face = "bold"),
        plot.margin = margin(t = 0.15, r = 0.15, b = 0.15, l = 0.15, "cm"),
        text = element_text(size=FSizeStrip, face="plain"), 
        axis.title.x = element_text(size = FSizeAxis),
        axis.title.y = element_text(size = FSizeAxis),
        axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), 
        axis.text.y = element_text(size=FSizeAxis),
        axis.line = element_line(colour = "black", size = 0.5),
        strip.background = element_rect(colour = "black", fill = "white", size = 0.5),
        strip.text.x = element_text(size = FSizeStrip, face="bold"), 
        strip.text.y = element_text(size = FSizeStrip, face="bold"), 
        legend.position = "right",
        legend.box = "vertical", 
        legend.direction = "vertical", 
        legend.spacing.x = unit(0.1, 'cm'),
        legend.spacing.y = unit(0.01,"cm"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(colour="gray80", size = 0.3),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.background = element_blank()) +
  scale_fill_manual(name="",
                    values=var_colours.df$Colour,
                    breaks=var_colours.df$Variable,
                    labels=var_colours.df$label,
                    guide="legend")
FE_Sector_SSP2  

#
# ---- Primary Energy - SSP2 - Column----
PE_EC_SSP2 <- 
  ggplot(data=subset(DATA.PE, Year %in% ActiveYears & Region=="World" & Scenario == "SSP2"
                     & !(Variable == "PEBiomass" | Variable == "PEBiomassEneCrops")),
         aes(x=Year,y = value, fill=VarOrder)) + 
  geom_bar(colour="black", stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  labs(x = "",
       y = expression(paste("Primary Enegy [EJ"[Prim],"/yr]")),
       title = "Primary Energy per Demand Energy Carrier")+
  theme_bw() +
  theme(plot.title = element_text(size = FSizeTitle, face = "bold"),
        plot.margin = margin(t = 0.15, r = 0.15, b = 0.15, l = 0.15, "cm"),
        text = element_text(size=FSizeStrip, face="plain"), 
        axis.title.x = element_text(size = FSizeAxis),
        axis.title.y = element_text(size = FSizeAxis),
        axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), 
        axis.text.y = element_text(size=FSizeAxis),
        axis.line = element_line(colour = "black", size = 0.5),
        strip.background = element_rect(colour = "black", fill = "white", size = 0.5),
        strip.text.x = element_text(size = FSizeStrip, face="bold"), 
        strip.text.y = element_text(size = FSizeStrip, face="bold"), 
        legend.position = "right",
        legend.box = "vertical", 
        legend.direction = "vertical", 
        legend.spacing.x = unit(0.1, 'cm'),
        legend.spacing.y = unit(0.01,"cm"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(colour="gray80", size = 0.3),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.background = element_blank()) +
  scale_fill_manual(name="",
                    values=var_colours.df$Colour,
                    breaks=var_colours.df$Variable,
                    labels=var_colours.df$label,
                    guide="legend")
PE_EC_SSP2  

#
# ---- Emissions - SSP2 - Column----
EM_EC_SSP2 <- 
  ggplot(data=subset(DATA.EM, Year %in% ActiveYears & Region=="World" & Scenario == "SSP2" &
                       !(Variable=="EmisCO2Ene" | Variable=="EmisCO2LU")),
         aes(x=Year,y = value/1000, fill=VarOrder)) + 
  geom_bar(colour="black", stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  labs(x = "",
       y = expression(paste("Emissions [GtCO"[2],"/yr]")),
       title = "Emissions per Source")+
  theme_bw() +
  theme(plot.title = element_text(size = FSizeTitle, face = "bold"),
        plot.margin = margin(t = 0.15, r = 0.15, b = 0.15, l = 0.15, "cm"),
        text = element_text(size=FSizeStrip, face="plain"), 
        axis.title.x = element_text(size = FSizeAxis),
        axis.title.y = element_text(size = FSizeAxis),
        axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), 
        axis.text.y = element_text(size=FSizeAxis),
        axis.line = element_line(colour = "black", size = 0.5),
        strip.background = element_rect(colour = "black", fill = "white", size = 0.5),
        strip.text.x = element_text(size = FSizeStrip, face="bold"), 
        strip.text.y = element_text(size = FSizeStrip, face="bold"), 
        legend.position = "right",
        legend.box = "vertical", 
        legend.direction = "vertical", 
        legend.spacing.x = unit(0.1, 'cm'),
        legend.spacing.y = unit(0.01,"cm"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(colour="gray80", size = 0.3),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.background = element_blank()) +
  scale_fill_manual(name="",
                    values=var_colours.df$Colour,
                    breaks=var_colours.df$Variable,
                    labels=var_colours.df$label,
                    guide="legend")
EM_EC_SSP2  

#
# ---- Secondary Bioenergy - SSP2 - Column----
SE_Biomass_SSP2 <- 
  ggplot(data=subset(DATA.SE, Year %in% ActiveYears & Region=="World" & Scenario == "SSP2" &
                       !(Variable=="SE" | Variable=="SEElecBiomasswCCS"|Variable=="SEElecBiomasswoCCS"
                         |Variable=="SEHydrogenBiomasswCCS"|Variable=="SEHydrogenBiomasswoCCS"
                         |Variable=="SELiquidsBiomasswCCS"|Variable=="SELiquidsBiomasswoCCS")),
         aes(x=Year,y = value, fill=VarOrder)) + 
  geom_bar(colour="black", stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  labs(x = "",
       y = expression(paste("Secondary Energy [EJ"[Secondary],"/yr]")),
       title = "Secondary bioenergy production")+
  theme_bw() +
  theme(plot.title = element_text(size = FSizeTitle, face = "bold"),
        plot.margin = margin(t = 0.15, r = 0.15, b = 0.15, l = 0.15, "cm"),
        text = element_text(size=FSizeStrip, face="plain"), 
        axis.title.x = element_text(size = FSizeAxis),
        axis.title.y = element_text(size = FSizeAxis),
        axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), 
        axis.text.y = element_text(size=FSizeAxis),
        axis.line = element_line(colour = "black", size = 0.5),
        strip.background = element_rect(colour = "black", fill = "white", size = 0.5),
        strip.text.x = element_text(size = FSizeStrip, face="bold"), 
        strip.text.y = element_text(size = FSizeStrip, face="bold"), 
        legend.position = "right",
        legend.box = "vertical", 
        legend.direction = "vertical", 
        legend.spacing.x = unit(0.1, 'cm'),
        legend.spacing.y = unit(0.01,"cm"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(colour="gray80", size = 0.3),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.background = element_blank()) +
  scale_fill_manual(name="",
                    values=var_colours.df$Colour,
                    breaks=var_colours.df$Variable,
                    labels=var_colours.df$label,
                    guide="legend")
SE_Biomass_SSP2  
#
#---- LUC vs. Biomass ----
DATA.LUCBio = subset(SSP, Variable == "PEBiomassEneCrops" | Variable == "EmisCO2LU")
DATA.LUCBio = subset(DATA.LUCBio, select = c("Scenario", "Region", "Year", "Variable", "value"))
DATA.LUCBio = spread(DATA.LUCBio,Variable,value)

FigLUCBio<-
  ggplot(data=subset(DATA.LUCBio, Year %in% ActiveYears & Region=="World")) + 
  geom_line(aes(x=Year, y=PEBiomassEneCrops), size=1, colour="black") +
  geom_line(aes(x=Year, y=EmisCO2LU/1000), size=1, colour="gray", linetype = "dashed") +
  scale_y_continuous(sec.axis = sec_axis(~., name = expression(paste(EJ[Prim],"/yr")))) +
  #xlim(2010,2100) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(plot.title = element_text(size = FSizeTitle, face = "bold"),
        plot.margin = margin(t = 0.15, r = 0.15, b = 0.15, l = 0.15, "cm"),
        text = element_text(size=FSizeStrip, face="plain"), 
        axis.title.x = element_text(size = FSizeAxis),
        axis.title.y = element_text(size = FSizeAxis),
        axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), 
        axis.text.y = element_text(size=FSizeAxis),
        axis.line = element_line(colour = "black", size = 0.5),
        strip.background = element_rect(colour = "black", fill = "white", size = 0.5),
        strip.text.x = element_text(size = FSizeStrip, face="bold"), 
        strip.text.y = element_text(size = FSizeStrip, face="bold"), 
        legend.position = "right",
        legend.box = "vertical", 
        legend.direction = "vertical", 
        legend.spacing.x = unit(0.1, 'cm'),
        legend.spacing.y = unit(0.01,"cm"),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(colour="gray80", size = 0.3),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        panel.background = element_blank()) +
  theme(legend.position=c(0.9,0.13), legend.text = element_text(size=6, face="plain"), legend.background = element_rect(fill=alpha('blue', 0))) +
  facet_wrap(~Scenario, nrow=2, labeller=labeller(Scenario = scen_labels))
FigLUCBio

# # ---- OUTPUTS ----
# png(file = paste0(getwd(),"/output/BBE/FE_Sector_SSP2.png"), width = 5*ppi, height = 3*ppi, units = "px", res = ppi)
# plot(FE_Sector_SSP2)
# dev.off()
# # # #
# png(file = paste0(getwd(),"/output/BBE/PE_EC_SSP2.png"), width = 5*ppi, height = 3*ppi, units = "px", res = ppi)
# plot(PE_EC_SSP2)
# dev.off()
# # # #
# png(file = paste0(getwd(),"/output/BBE/EM_EC_SSP2.png"), width = 5*ppi, height = 3*ppi, units = "px", res = ppi)
# plot(EM_EC_SSP2)
# dev.off()
# # # #
# png(file = paste0(getwd(),"/output/BBE/SE_Biomass_SSP2.png"), width = 5*ppi, height = 3*ppi, units = "px", res = ppi)
# plot(SE_Biomass_SSP2)
# dev.off()
# # # #




