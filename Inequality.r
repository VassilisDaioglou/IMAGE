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

Regions = c(2,5,10,11,18)
Years = c("1980","1990","2000","2010","2015","2020","2025","2030","2035","2040","2045",
          "2050","2055","2060","2065","2070","2075","2080","2085","2090","2095","2100")

ActiveRegion <- 10

data <- "data/Inqeuality/TIMER_RESULTS.xlsx"
# set higher RAM capacity for java (used in clsx package)
options(java.parameters = "-Xmx8000m")

# ---- INPUTS: Data ----
  # set directory path 
setwd("C:/Users/Asus/Documents/Github/IMAGE/")
  # Read Data Files for Baseline Scenario
EnFunc = read.xlsx(data, sheet = "EnUseFunction", startRow=4)
EnUse = read.xlsx(data, sheet = "TotalEnUseTURQ", startRow=4)
FSInsul = read.xlsx(data, sheet = "FSInsul", startRow=4)
RenovRate = read.xlsx(data, sheet = "Renov_Rate_ave", startRow=4)
