# R script to process results from the SR15 database
# https://data.ene.iiasa.ac.at/iamc-1.5c-explorer
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

# ---- INPUTS ----
ppi <- 300
FSizeStrip = 10
FSizeAxis = 9
FSizeLeg = 10

# set higher RAM capacity for java (used in clsx package)
options(java.parameters = "-Xmx8000m")

# set directory path 
setwd("C:/Users/Asus/Documents/Github/IMAGE/")
# Read Data from SR1.5C DB
DATA=read.csv("data/SR15/iamc-1.5c-explorer_snapshot_1574413368.csv", sep=",", dec=".", stringsAsFactors = FALSE)
DATA2=read.csv("data/SR15/iamc-1.5c-explorer_snapshot_1574423068.csv", sep=",", dec=".", stringsAsFactors = FALSE) # Separate dataset with Prim Bio in baselines
AFOLU=read.csv("data/SR15/iamc-1.5c-explorer_snapshot_1574430547.csv", sep=",", dec=".", stringsAsFactors = FALSE) # Separate dataset with AFOLU emissions in Mitigation Scenarios
PRIM=read.csv("data/SR15/iamc-1.5c-explorer_snapshot_1585815642.csv", sep=",", dec=".", stringsAsFactors = FALSE) # Separate dataset with AFOLU emissions in Mitigation Scenarios

# ---- DATA STRUCTURE ----
# Re-structure and Clean dataframe
  # First bind DATA and DATA2 DFs (Data 2 contains primary bioenergy in baselines, which are declared under "Biomass|Modern|w/o CCS")
DATA = subset(DATA, select=-c(X2000,X2005,X2015,X2025,X2035,X2045,X2055,X2065,X2075,X2085,X2095))
DATA2 = subset(DATA2, select=-c(X2005))
AFOLU = subset(AFOLU, select=-c(X2000,X2005,X2015,X2025,X2035,X2045,X2055,X2065,X2075,X2085,X2095))
PRIM = subset(PRIM, select=-c(X2000,X2005,X2015,X2025,X2035,X2045,X2055,X2065,X2075,X2085,X2095))

DATA = rbind(DATA,DATA2,AFOLU,PRIM)

rm(DATA2, AFOLU, PRIM)
  # Fix years and values
DATA=melt(DATA, id.vars=c("Model","Scenario","Region","Variable","Unit"), na.rm=TRUE)
colnames(DATA)[6] <- "Year"
DATA$Year <- gsub("X","",DATA$Year,fixed=F)
DATA$Year = as.numeric(DATA$Year)
DATA$value = as.numeric(DATA$value)

  # Demarcate different project
DATA$Project = substr(DATA$Scenario, start=1, stop=5)
DATA$Project <- gsub("CD-LI","CDLinks",DATA$Project,fixed=F)
DATA$Project <- gsub("DAC15","DAC",DATA$Project,fixed=F)
DATA$Project <- gsub("DAC2_","DAC",DATA$Project,fixed=F)
DATA$Project <- gsub("ADVAN","ADVANCE",DATA$Project,fixed=F)
DATA$Project <- gsub("SFCM_","SCFM",DATA$Project,fixed=F)
DATA$Project <- gsub("TERL_","TERL",DATA$Project,fixed=F)
DATA$Project <- gsub("Faste","IEA_FTS",DATA$Project,fixed=F)
DATA$Project <- gsub("GEA_E","GEA",DATA$Project,fixed=F)
DATA$Project <- gsub("GEA_M","GEA",DATA$Project,fixed=F)
DATA$Project <- gsub("EMC_D","EMC",DATA$Project,fixed=F)
DATA$Project <- gsub("EMC_L","EMC",DATA$Project,fixed=F)
DATA$Project <- gsub("EMC_N","EMC",DATA$Project,fixed=F)
DATA$Project <- gsub("EMC_l","EMC",DATA$Project,fixed=F)
DATA$Project <- gsub("CEMIC","CEMICS",DATA$Project,fixed=F)
DATA$Project <- gsub("PEP_1","PEP",DATA$Project,fixed=F)
DATA$Project <- gsub("PEP_2","PEP",DATA$Project,fixed=F)
DATA$Project <- gsub("SMP_1","SMP",DATA$Project,fixed=F)
DATA$Project <- gsub("SMP_2","SMP",DATA$Project,fixed=F)
DATA$Project <- gsub("-","",DATA$Project,fixed=F)

  # Demarcate climate targets
DATA$Target <- str_extract(DATA$Scenario,"(15|1p5|1.5|400|LowEnergyDemand|19|WB2C|WB2|Med2C|2_66|1000|2D|26|2C|2.0|1600|Baseline)")
DATA$Target <- gsub("15","1.5C",DATA$Target,fixed=F)
DATA$Target <- gsub("1.5","1.5C",DATA$Target,fixed=F)
DATA$Target <- gsub("400","1.5C",DATA$Target,fixed=F)
DATA$Target <- gsub("LowEnergyDemand","1.5C",DATA$Target,fixed=F)
DATA$Target <- gsub("1p5","1.5C",DATA$Target,fixed=F)
DATA$Target <- gsub("CC","C",DATA$Target,fixed=F)
DATA$Target <- gsub("19","1.5C",DATA$Target,fixed=F)
DATA$Target <- gsub("WB2C","1.5C",DATA$Target,fixed=F)  # Set WB2C as 1.5C
DATA$Target <- gsub("2_66","2C",DATA$Target,fixed=F)
DATA$Target <- gsub("Med2C","2C",DATA$Target,fixed=F)
DATA$Target <- gsub("1000","2C",DATA$Target,fixed=F)
DATA$Target <- gsub("2D","2C",DATA$Target,fixed=F)
DATA$Target <- gsub("26","2C",DATA$Target,fixed=F)
DATA$Target <- gsub("2.0","2C",DATA$Target,fixed=F)

  # Clean Variable names
DATA$Variable <- gsub("[[:punct:]]","",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Land Cover","LandCover",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Primary Energy","Prim",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Modern","",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Cropland","",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Emissions","Emis",DATA$Variable,fixed=F)
DATA$Variable <- gsub("[[:space:]]","",DATA$Variable,fixed=F)
DATA$Variable <- gsub("woCCS","",DATA$Variable,fixed=F)

DATA$Variable <- paste(DATA$Variable,"-",DATA$Unit)
DATA$Variable <- gsub("[[:space:]]","",DATA$Variable,fixed=F)
DATA$Variable <- gsub("million","M",DATA$Variable,fixed=F)

  # Re-structure to have variables as columns
DATA$Unit <- NULL
DATA=spread(DATA,Variable,value)
#
# ---- DATA PROCESSING ----
# Calculate Fraction of Biomass in TPES
DATA = DATA %>% mutate(FracBio = `PrimBiomass-EJ/yr`/`Prim-EJ/yr`)

# Have to generate datasets containting the minimum and maximum for each of the "Targets" 
# This will aid in drawing shaded areas for each target

# Baselines
Baselines = subset(DATA, Target=="Baseline")
Baselines = melt(Baselines, id.vars=c("Model","Scenario","Region","Year","Project","Target"), na.rm=TRUE)

Baselines.Min <- aggregate(Baselines$value, by=list(Year=Baselines$Year, Project=Baselines$Project, variable=Baselines$variable), FUN=min, na.rm=TRUE) 
colnames(Baselines.Min)[4] <- "Minimum"
Baselines.Min$ID = paste(Baselines.Min$Project,Baselines.Min$variable,Baselines.Min$Year)

Baselines.Max <- aggregate(Baselines$value, by=list(Year=Baselines$Year, Project=Baselines$Project, variable=Baselines$variable), FUN=max, na.rm=TRUE) 
colnames(Baselines.Max)[4] <- "Maximum"
Baselines.Max$ID = paste(Baselines.Min$Project,Baselines.Min$variable,Baselines.Max$Year)

Baselines.MinMax = Baselines.Min
Baselines.MinMax$Maximum <- Baselines.Max[match(Baselines.MinMax$ID, Baselines.Max$ID),"Maximum"]

rm(Baselines.Min,Baselines.Max)

#
# Mitigation Scenarios
Mitigation = subset(DATA, !Target=="Baseline")
Mitigation = Mitigation %>% mutate(FracCCS = `PrimBiomasswCCS-EJ/yr`/`PrimBiomass-EJ/yr`)
Mitigation = subset(Mitigation, !(FracCCS>1))  # Ignore some observations which return values >1

Mitigation = melt(Mitigation, id.vars=c("Model","Scenario","Region","Year","Project","Target"))

Mitigation$var_Order = factor(Mitigation$variable, levels=c("Prim-EJ/yr",
                                                            "PrimBiomass-EJ/yr",
                                                            "FracBio",
                                                            "PrimBiomasswCCS-EJ/yr",
                                                            "FracCCS",
                                                            "LandCoverEnergyCrops-Mha",
                                                            "EmisCO2AFOLU-MtCO2/yr"))
#
# Sensitivity Runs
Sensitivity = subset(Mitigation, Project=="EMF33")

Sensitivity$Test <- str_extract(Sensitivity$Scenario,"(cost100|full|nobeccs|nofuel|none|limbio)")

Sensitivity$Test_Order = factor(Sensitivity$Test, levels=c("full",
                                                           "cost100",
                                                           "nobeccs",
                                                           "nofuel",
                                                           "none",
                                                           "limbio"))

Sensitivity.Min <- aggregate(Sensitivity$value, by=list(Test=Sensitivity$Test, Year=Sensitivity$Year, variable=Sensitivity$variable), FUN=min, na.rm=TRUE) 
colnames(Sensitivity.Min)[4] <- "Minimum"
Sensitivity.Min$ID = paste(Sensitivity.Min$Test,Sensitivity.Min$variable,Sensitivity.Min$Year)

Sensitivity.Max <- aggregate(Sensitivity$value, by=list(Test=Sensitivity$Test, Year=Sensitivity$Year, variable=Sensitivity$variable), FUN=max, na.rm=TRUE) 
colnames(Sensitivity.Max)[4] <- "Maximum"
Sensitivity.Max$ID = paste(Sensitivity.Min$Test,Sensitivity.Min$variable,Sensitivity.Max$Year)

Sensitivity.MinMax = Sensitivity.Min
Sensitivity.MinMax$Maximum <- Sensitivity.Max[match(Sensitivity.MinMax$ID, Sensitivity.Max$ID),"Maximum"]

rm(Sensitivity.Max,Sensitivity.Min)

Sensitivity.MinMax$Test_Order = factor(Sensitivity.MinMax$Test, levels=c("full",
                                                           "cost100",
                                                           "nobeccs",
                                                           "nofuel",
                                                           "none",
                                                           "limbio"))

Sensitivity.MinMax$var_Order = factor(Sensitivity.MinMax$variable, levels=c("Prim-EJ/yr",
                                                                            "PrimBiomass-EJ/yr",
                                                                            "FracBio",
                                                                            "PrimBiomasswCCS-EJ/yr",
                                                                            "FracCCS",
                                                                            "LandCoverEnergyCrops-Mha",
                                                                            "EmisCO2AFOLU-MtCO2/yr"))
#


#
# ---- DATA Checks ----
# Ad hoc checks on data to inspect results

  # Check if there are scenarios which do not reply on negative emissions
NoNeg = subset(Mitigation, variable=="EmisCO2AFOLU-MtCO2/yr"|variable=="PrimBiomasswCCS-EJ/yr")
NoNeg = subset(NoNeg, select=-c(var_Order,ID))
NoNeg = spread(NoNeg, variable,value)
colnames(NoNeg)[7:8] <- c("AFOLU","BioCCS")
NoNeg = subset(NoNeg, BioCCS == 0)
NoNeg = subset(NoNeg, !(Year==2010|Year==2020))
NoNeg = subset(NoNeg, AFOLU > 0)

  # Determine number of scenarios consistent with Paris Targets
Paris = subset(Mitigation, select=c(Model,Scenario))
Paris$ID = paste(Paris$Model,Paris$Scenario)
length(unique(Paris$ID))

# 
# ---- LABELS ----
#var labels with text wraps                
var_labels <- c("Prim-EJ/yr"="Total Primary Energy \nEJ/yr",
                "LandCoverEnergyCrops-Mha"="Land Cover Energy Crops \nMHa",
                "PrimBiomass-EJ/yr"="Primary Biomass \nEJ/yr",
                "PrimBiomasswCCS-EJ/yr"="Primary Biomass with CCS \nEJ/yr",
                "FracCCS"="Fraction Combined \nwith CCS",
                "FracBio"="Fraction of Biomass \nin TPES",
                "EmisCO2AFOLU-MtCO2/yr"="AFOLU CO2 Emissions \nMtCO2/yr")


# ---- FIG: BASELINES ----
# Baselines projections
BaseProj <- ggplot(subset(Baselines.MinMax, !variable=="PrimBiomasswCCS-EJ/yr"), aes(x=Year, y=Minimum)) + 
  geom_line(aes(y = Minimum), alpha = 0.1) + 
  geom_line(aes(y = Maximum), alpha = 0.1) +
  geom_ribbon(aes(ymin=Minimum,ymax=Maximum, fill=Project), alpha="0.5") +
  xlab("") + 
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeStrip, hjust=1), axis.text.y = element_text(size=FSizeStrip)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="none") +
  scale_fill_manual(values=c("forestgreen","dodgerblue","firebrick1","darkolivegreen1","darkorchid"),
                    name="",
                    breaks=c("SSP1","SSP2","SSP3","SSP4","SSP5"),
                    labels=c("SSP1","SSP2","SSP3","SSP4","SSP5")
                    ,guide=FALSE) +
  
  facet_grid(variable~Project) 
BaseProj

# ---- FIG: MITIGATION ----
# Mitigation Projections
# Mitig = subset(Mitigation, select=-c(Model,Scenario,Region,Project))
Mitigation$ID = paste(Mitigation$Model,Mitigation$Scenario,Mitigation$Region,Mitigation$Project)

MitigProj <- ggplot(data=subset(Mitigation, !Target=="1600"), aes(x=Year,y = value, colour=Target, fill=ID)) + 
  geom_line(alpha=0.75) + 
  xlim(2010,2100) +
  xlab("") + 
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeStrip, hjust=1), axis.text.y = element_text(size=FSizeStrip)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="none") +
  scale_colour_manual(values=c("forestgreen","dodgerblue","firebrick1","darkorchid"),
                      name="",
                      breaks=c("1.5C","2C","WB2C","1600"),
                      labels=c("1.5C","2C","WB2C",">2C")
                      ,guide=FALSE) +
  
  facet_grid(variable~Target, scales="free_y") 
MitigProj

# Mitigation Projections with targets superimposed
MitigProj2 <- ggplot(Mitigation, aes(x=Year,y = value, colour=Target, fill=ID)) + 
  geom_line(data=subset(Mitigation, Target=="2C"), alpha=0.6) + 
  geom_line(data=subset(Mitigation, Target=="1.5C"), alpha=0.5) + 
  geom_hline(yintercept=0,size = 0.3, colour='black') +
  xlim(2010,2100) +
  xlab("") + ylab("") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeAxis, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_colour_manual(values=c("forestgreen","darkorchid"),
                      name="",
                      breaks=c("1.5C","2C"),
                      labels=c("1.5C","2C")
                      ) +
  facet_wrap(.~var_Order, nrow=2, scales="free_y", labeller = labeller(var_Order=var_labels)) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip))
MitigProj2

# ---- FIG: MITIGATION: Biomass importance ----
BioFrac <- ggplot(subset(Mitigation, variable=="PrimBiomass-EJ/yr"|variable=="FracBio"), 
                  aes(x=Year,y = value, colour=Target, fill=ID)) + 
  geom_line(data=subset(subset(Mitigation, variable=="PrimBiomass-EJ/yr"|variable=="FracBio"), Target=="2C"), alpha=0.6) + 
  geom_line(data=subset(subset(Mitigation, variable=="PrimBiomass-EJ/yr"|variable=="FracBio"), Target=="1.5C"), alpha=0.5) + 
  geom_hline(yintercept=0,size = 0.3, colour='black') +
  xlim(2010,2100) +
  xlab("") + ylab("") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeAxis, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_colour_manual(values=c("forestgreen","darkorchid"),
                      name="",
                      breaks=c("1.5C","2C"),
                      labels=c("1.5C","2C")
  ) +
  facet_wrap(.~var_Order, nrow=1, scales="free_y", labeller = labeller(var_Order=var_labels)) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip))
BioFrac

#
# ---- FIG: SENSITIVITIES ----

Sens <- ggplot(subset(Sensitivity.MinMax, Year=="2020"|Year=="2040"|Year=="2060"|Year=="2080"|Year=="2100"),
               aes(x=Year, ymin = Minimum,ymax = Maximum, lower = Minimum, upper = Maximum, middle = Minimum,
                                       colour=Test_Order, fill=Test_Order)) + 
  geom_boxplot(stat = 'identity') +
  geom_hline(yintercept=0,size = 0.3, colour='black') +
  geom_vline(xintercept=c(2030,2050,2070,2090), size=0.1, colour="gray30") +
  xlim(2010,2110) +
  xlab("") + ylab("") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeAxis, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="bottom", legend.text = element_text(size=FSizeLeg)) +
  scale_colour_manual(values=c("forestgreen","peachpuff1","darkmagenta","chocolate1","lightskyblue4","seagreen1"),
                      name="",
                      breaks=c("full","cost100","nobeccs","nofuel","none","limbio"),
                      labels=c("No Constraint","Costs x2","No BECCS","No Adv. Fuels","No Adv. BECCS or Adv. Fuels","<100 EJ/yr Potential"),
                      guide=FALSE) + 
  scale_fill_manual(values=c("forestgreen","peachpuff1","darkmagenta","chocolate1","lightskyblue4","seagreen1"),
                      name="",
                      breaks=c("full","cost100","nobeccs","nofuel","none","limbio"),
                      labels=c("No Constraint","Costs x2","No BECCS","No Adv. Fuels","No Adv. Technologies","<100 EJ/yr Potential")) + 
  facet_wrap(.~var_Order, nrow=2, scales="free_y", labeller = labeller(var_Order=var_labels)) +
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip)) + 
  guides(fill = guide_legend(nrow = 1))
Sens
#
# ---- OUTPUTS ----
# png(file = "output/SR15/BaseProj.png", width = 6*ppi, height = 4*ppi, units = "px", res = ppi)
# plot(BaseProj)
# dev.off()
# #
# png(file = "output/SR15/MitigProj.png", width = 6*ppi, height = 8*ppi, units = "px", res = ppi)
# plot(MitigProj)
# dev.off()
# #
# png(file = "output/SR15/MitigProj2.png", width = 10.5*ppi, height = 5.5*ppi, units = "px", res = ppi)
# plot(MitigProj2)
# dev.off()
# 
# png(file = "output/SR15/Sensitivity.png", width = 10.5*ppi, height = 7*ppi, units = "px", res = ppi)
# plot(Sens)
# dev.off()
# 
# png(file = "output/SR15/FracBio.png", width = 5*ppi, height = 2.5*ppi, units = "px", res = ppi)
# plot(BioFrac)
# dev.off()
