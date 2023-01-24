# ---- INFORMATION ----
# R script to process material demand projections in the IMAGE framework
# Based on original script found here: https://github.com/VassilisDaioglou/SHAPE/blob/master/script/material_demand.R
# Material production (steel, cement) provided by material flow modelling (Sebastiaan Deetman) - BUMA/VEMA/EMLA
# Data provided on an IMAGE Region level
#
# Materials Production Provided:
#     
#     1. Total Steel          
#     2. Virgin Steel        
#     3. Total Cement                 
#     4. Virgin Cement                               
# 
# Output:
# Factor showing annual change of total consumption for steel & concrete (separately)
# 
#
# This script produced as part of the Global Resource Outlook (https://www.resourcepanel.org/)
# Vassilis Daioglou, January 2023
# 
# Process:
#   1. Get growth rates for 
#     (i)   Total Steel
#     (ii)  Total Cement
#   2. Material data only runs til 2060. After that continue trend at a lower rate 
#
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
library(openxlsx)
library(grid)
library(RColorBrewer)
library(gridExtra)
library(ggnewscale)

setwd("C:/Users/daioglouv/Documents/git/IMAGE/")

# ---- INPUTS: Constants ----
ppi <- 300

FSizeTitle = 10
FSizeStrip = 9
FSizeAxis = 8
FSizeLeg = 9

scenarios <- c('SSP2_CP','SSP2_CP_26')
variables <- c('demand_concrete', 'demand_steel')

t_scen <- 2020

RMapping <- data.frame(IMAGE_Region = c(1,2,3,4,5,6,7,8,9,10,
                                        11,12,13,14,15,16,17,18,19,20,
                                        21,22,23,24,25,26,27),
                       Region_ID = c("CAN","USA","MEX","RCAM","BRA","RSAM","NAF","WAF","EAF","SAF",
                                     "WEU","CEU","TUR","UKR","STAN","RUS","ME","INDIA","KOR","CHN",
                                     "SEAS","INDO","JAP","OCE","RSAS","RSAF","World"))

Active_indicators = c("demand_steel","demand_concrete")

# DATA LOCATIONS
DATA_date <- '240123'
DATA_location <- paste0(getwd(),"/data/material_demand/")
output_location <- paste0(getwd(),"/output/material_demand/")

# set higher RAM capacity for java (used in clsx package)
options(java.parameters = "-Xmx8000m")

# ---- INPUTS: Data ----
# Make empty dataframe and populate by looping thorugh the datasets which are per 'scenarios' and 'variables'
columns = c("region","ünit","scenario","variable","year","value")
DATA = data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(DATA) = columns

for (i in scenarios) {
  for (j in variables){
    data_in <- paste0(i,"/",i,"_",j,"_",DATA_date,".csv")
    data = read.csv(paste0(DATA_location, data_in), header = TRUE)
    data <- data %>%
      mutate(scenario = i) %>%
      mutate(variable = j) %>%
      gather(key = year, value = value, -region, -unit, -variable, -scenario)
    DATA = rbind(DATA, data)
  }
}

rm(DATA_location, DATA_date, 
   columns, data_in, data,
   i, j)

#
# ---- MUNGING ----
DATA <- DATA %>%
  # Reshape and organise
  mutate(year = gsub("X","", year)) %>%
  mutate(year = as.numeric(substr(year, start=1, stop=4))) %>%
  mutate(value = as.numeric(substr(value, start=1, stop=20))) %>%
  # Remove historic data
  subset(year >= t_scen) %>%
  # Remove potential negative numbers
  mutate(value = replace(value, value < 0, 0.0))
  
# ---- ANNUAL CHANGES ----
# Determine annual growth rate, per material category ('Indicator')
growthrate <- DATA %>%
  # First sort so that you have consecutive years for each Scenario-Indicator-Region pair
  arrange(region, variable, scenario, year) %>%
  mutate(diff_year = year - lag(year),                            # Difference in time (just in case there are gaps)
         # Diff_growth = value - lag(value),                        # Difference in route between years
         # Change = 1+(Diff_growth / Diff_year)/value) %>%          # growth rate in percent
         change = value / lag(value)) %>%
  mutate(unit = gsub("Mt/yr", "Change", unit)) %>%
  subset(select = -c(value, diff_year)) %>%
  subset(!year==t_scen)                                          # Remove start year since these values are irrelevant
  
growthrate$ID <- paste(growthrate$variable, growthrate$region, growthrate$scenario, growthrate$year, sep="_")

# ---- FINAL DATASET GROWTHRATE ----
FINAL.gr <- growthrate
FINAL.gr$IMAGE_region = RMapping[match(FINAL.gr$region, RMapping$Region_ID), "IMAGE_Region"] 

FINAL.gr <- FINAL.gr %>% 
  subset(select = -c(region, ID)) %>%
  arrange(IMAGE_region, variable, year)

# ---- FINAL DATASET ABSOLUTE ----
FINAL.abs <- DATA
FINAL.abs$IMAGE_region = RMapping[match(FINAL.abs$region, RMapping$Region_ID), "IMAGE_Region"] 
FINAL.abs <- FINAL.abs %>% 
  subset(select = -region) %>%
  arrange(IMAGE_region, variable, scenario, year)

#
# ---- FIGURES ----
# ---- Fig: Material Growth per region ----
for (i in variables) {
  MatGr <- 
    ggplot(data=subset(FINAL.gr, variable = i)) +
    geom_line(aes(x=year, y=change, colour = scenario), alpha = 1, size = 1) +
    geom_hline(yintercept=0,size = 0.1, colour='black') +
    labs(x = "",
         y = unique(FINAL.gr$Unit),
         title = i)+
    ylim(min(FINAL.gr$change) * 0.9, max(FINAL.gr$change) * 1.05) +
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
    facet_wrap(IMAGE_region~., scale="free_y")
  MatGr  
  
  png(file = paste0(getwd(),"/output/material_demand/growthrate_",i,".png"), width = 9*ppi, height = 7*ppi, units = "px", res = ppi)
  plot(MatGr)
  dev.off()

}
#
# ---- Fig: Material Demand per region ----
for (i in variables) {
  MatDem <- 
    ggplot(data=subset(FINAL.abs, variable == i)) +
    geom_line(aes(x=year, y=value, colour = scenario), alpha = 1, size = 1) +
    geom_hline(yintercept=0,size = 0.1, colour='black') +
    labs(x = "",
         y = unique(FINAL.abs$Unit),
         title = i)+
    #ylim(min(FINAL.abs$value) * 0.9, max(FINAL.abs$value) * 1.05) +
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
    facet_wrap(IMAGE_region~., scale="free_y")
  MatDem  
  
  png(file = paste0(getwd(),"/output/material_demand/demand_",i,".png"), width = 9*ppi, height = 7*ppi, units = "px", res = ppi)
  plot(MatDem)
  dev.off()
  
}

# ---- OUTPUTS ----
# Make dataframe which can be exported in a format relevant for TIMER
ForTIMER <- FINAL.abs %>%
  subset(select=-unit) %>%
  rename("t" = "year") %>%
  mutate(IMAGE_region = gsub("^","class_", IMAGE_region)) %>%
  spread(key = 'IMAGE_region', value = 'value') %>%
  select(variable, t, class_1, class_2, class_3, class_4, class_5, class_6, class_7, class_8, class_9, class_10,
         class_11, class_12, class_13, class_14, class_15, class_16, class_17, class_18, class_19, class_20, 
         class_21, class_22, class_23, class_24, class_25, class_26, class_27)

# Separate datastes for Concrete and Steel and add historic and future values
ForTIMER.ce <- ForTIMER %>%
  subset(variable == "demand_concrete") %>%
  subset(select=-variable) %>%
  # Add historic values
  rbind(c(1970,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)) %>%
  rbind(c(t_scen-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)) %>%
  # Add end of projection values (i.e. no exogenous change after 2060)
  rbind(c(2061,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)) %>%
  arrange(t)

ForTIMER.st <- ForTIMER %>%
  subset(variable == "demand_steel") %>%
  subset(select=-variable)%>%
  # Add historic values
  rbind(c(1970,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)) %>%
  rbind(c(t_scen-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)) %>%
  # Add end of projection values (i.e. no exogenous change after 2060)
  rbind(c(2061,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)) %>%
  arrange(t)

# Dataset per 'Scenario' with annual changes 
dataset_names <- list("concrete" = ForTIMER.ce,
                      'steel' = ForTIMER.st)
#export each data frames to separate sheets in same Excel file
openxlsx::write.xlsx(dataset_names, file = paste0(output_location,"demand_changes.xlsx"))
  
