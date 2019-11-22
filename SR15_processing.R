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
FSizeStrip = 6.5
FSizeLeg = 6.5

# set higher RAM capacity for java (used in clsx package)
options(java.parameters = "-Xmx8000m")

# set directory path 
setwd("C:/Users/Asus/Documents/Github/Biomass_SSP_Scenarios/")
# Read Data from SR1.5C DB
DATA=read.csv("data/SR15/iamc-1.5c-explorer_snapshot_1574413368.csv", sep=",", dec=".", stringsAsFactors = FALSE)

# ---- DATA STRUCTURE ----
# Re-structure and Clean dataframe
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
DATA$Target <- gsub("2_66","2C",DATA$Target,fixed=F)
DATA$Target <- gsub("Med2C","2C",DATA$Target,fixed=F)
DATA$Target <- gsub("1000","2C",DATA$Target,fixed=F)
DATA$Target <- gsub("2D","2C",DATA$Target,fixed=F)
DATA$Target <- gsub("26","2C",DATA$Target,fixed=F)
DATA$Target <- gsub("2.0","2C",DATA$Target,fixed=F)

  # Clean Variable names
DATA$Variable <-gsub( "[[:punct:]]","",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Land Cover","LandCover",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Primary Energy","Prim",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Modern","",DATA$Variable,fixed=F)
DATA$Variable <- gsub("Cropland","",DATA$Variable,fixed=F)
DATA$Variable <-paste(DATA$Variable,"-",DATA$Unit)
DATA$Variable <- gsub("million","M",DATA$Variable,fixed=F)
DATA$Variable <-gsub( "[[:space:]]","",DATA$Variable,fixed=F)

  # Re-structure to have variables as columns
DATA$Unit <- NULL
DATA=spread(DATA,Variable,value)


# ---- FIGURES ----
