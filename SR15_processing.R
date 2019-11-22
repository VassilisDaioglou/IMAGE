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

# ---- INPUTS: BioTool ----
ppi <- 300
FSizeStrip = 6.5
FSizeLeg = 6.5

# set higher RAM capacity for java (used in clsx package)
options(java.parameters = "-Xmx8000m")

# set directory path 
setwd("C:/Users/Asus/Documents/Github/Biomass_SSP_Scenarios/")
# Read Data Files for Yield-supply curves
DATA=read.xlsx("data/Harper/DATA.csv", sheet = 2, startRow=4)
DATA=read.csv("data/SR15/xxx.csv", sep=",", dec=".", stringsAsFactors = FALSE)
