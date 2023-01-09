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

scenario <- "SSP2_CP"

t_scen <- 2025

RMapping <- data.frame(IMAGE_Region = c(1,2,3,4,5,6,7,8,9,10,
                                        11,12,13,14,15,16,17,18,19,20,
                                        21,22,23,24,25,26,27),
                       Region_ID = c("CAN","USA","MEX","RCAM","BRA","RSAM","NAF","WAF","EAF","SAF",
                                     "WEU","CEU","TUR","UKR","STAN","RUS","ME","INDIA","KOR","CHN",
                                     "SEAS","INDO","JAP","OCE","RSAS","RSAF","World"))

Active_indicators = c("demand_steel","demand_concrete")

# DATA LOCATIONS
DATA_location <- paste0(getwd(),"/data/material_demand/")
DATA_concrete <- paste0(scenario,"/SSP2_CP_demand_concrete.csv")
DATA_steel <- paste0(scenario,"/SSP2_CP_demand_steel.csv")
output_location <- paste0(getwd(),"/output/material_demand/")
# set higher RAM capacity for java (used in clsx package)
options(java.parameters = "-Xmx8000m")

# ---- INPUTS: Data ----
DATA.concrete = read.csv(paste0(DATA_location, DATA_concrete), header = TRUE)
DATA.concrete$variable <- "demand_concrete"

DATA.steel = read.csv(paste0(DATA_location, DATA_steel), header = TRUE)
DATA.steel$variable <- "demand_steel"

DATA = rbind(DATA.concrete, DATA.steel)

rm(DATA_location, DATA_concrete, DATA_steel, DATA.concrete, DATA.steel)

#
# ---- MUNGING ----
DATA <- DATA %>%
  # Reshape and organise
  gather(key = year, value = value, -region, -unit, -variable)  %>%
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
  arrange(region, variable, year) %>%
  mutate(diff_year = year - lag(year),                            # Difference in time (just in case there are gaps)
         # Diff_growth = value - lag(value),                        # Difference in route between years
         # Change = 1+(Diff_growth / Diff_year)/value) %>%          # growth rate in percent
         change = value / lag(value)) %>%
  mutate(unit = gsub("Mt/yr", "Change", unit)) %>%
  subset(select = -c(value, diff_year)) %>%
  subset(!year==t_scen)                                          # Remove start year since these values are irrelevant
  
growthrate$ID <- paste(growthrate$variable, growthrate$region, growthrate$year, sep="_")

# ---- FINAL DATASET ----
# FINAL <- data.frame(variable = unique(growthrate$variable),
#                     unit = unique(growthrate$unit)) %>%
#         arrange(variable) # Sort by indicator
# 
# # Expand selection and add region column (IMAGE Region #)
# FINAL <- FINAL %>%
#   slice(rep(1:n(), each = length(unique(RMapping$IMAGE_Region))))
# FINAL$region <- 1:27
# 
# # Expand selection and add year column (2021-2060)
# FINAL <- FINAL %>%
#   slice(rep(1:n(), each = length(unique(growthrate$year))))
# FINAL$year <- min(unique((growthrate$year))):max(unique((growthrate$year)))
# 
# # Populate values of growthrates
# FINAL$region_ID <- RMapping[match(FINAL$region, RMapping$IMAGE_Region), "Region_ID"] 
# FINAL$ID <- paste(FINAL$variable, FINAL$region_ID, FINAL$year, sep="_")
# FINAL$value <- growthrate[match(FINAL$ID, growthrate$ID), "change"] 
# FINAL <- FINAL %>% 
#   subset(select=-c(region_ID, ID)) %>%
#   replace(is.na(.), 0) 

FINAL <- growthrate
FINAL$IMAGE_region = RMapping[match(FINAL$region, RMapping$Region_ID), "IMAGE_Region"] 

FINAL <- FINAL %>% 
  subset(select = -c(region, ID)) %>%
  arrange(IMAGE_region, variable, year)
  

# Calculate dacadal averages  
FINAL <- FINAL %>%
  mutate(Decade = (as.numeric(substr(Year, start=3, stop=3)) + 1) * 10) 
FINAL <- aggregate(FINAL$value, by=list(Indicator=FINAL$Indicator,
                                          Unit=FINAL$Unit,
                                          Region=FINAL$Region,
                                          Scenario=FINAL$Scenario,
                                          Decade=FINAL$Decade), FUN = mean, na.rm=F) %>%
  rename(., value = x) %>%
  subset(!Decade==70) %>%
  spread(Region, value) 
#
# ---- FIGURES ----
# ---- Fig: Material demand per region ----
MatDem <- 
  ggplot(data=FINAL) +
  geom_line(aes(x=year, y=change, colour = variable), alpha = 1, size = 1) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  labs(x = "",
       y = unique(FINAL$Unit),
       title = "Demand for materials")+
  ylim(min(FINAL$change) * 0.9, max(FINAL$change) * 1.1) +
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
  scale_colour_manual(name="",
                      values=c("midnightblue","goldenrod3"),
                      breaks=c("demand_concrete","demand_steel"),
                      labels=c("Concrete","Steel"),
                      guide="legend") +
  facet_wrap(IMAGE_region~., scale="free_y")
MatDem  

png(file = paste0(getwd(),"/output/material_demand/MaterialDemand.png"), width = 9*ppi, height = 7*ppi, units = "px", res = ppi)
plot(MatDem)
dev.off()

#
# ---- OUTPUTS ----
# Make dataframe which can be exported in a format relevant for TIMER
ForTIMER <- FINAL %>%
  subset(select=-unit) %>%
  rename("t" = "year") %>%
  mutate(IMAGE_region = gsub("^","class_", IMAGE_region)) %>%
  spread(key = 'IMAGE_region', value = 'change') %>%
  select(variable, t, class_1, class_2, class_3, class_4, class_5, class_6, class_7, class_8, class_9, class_10,
         class_11, class_12, class_13, class_14, class_15, class_16, class_17, class_18, class_19, class_20, 
         class_21, class_22, class_23, class_24, class_25, class_26, class_27)

# Separate datastes for Concrete and Steel and add historic and future values
ForTIMER.ce <- ForTIMER %>%
  subset(variable == "demand_concrete") %>%
  subset(select=-variable) %>%
  # Add historic values
  rbind(c(1970,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)) %>%
  rbind(c(2019,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)) %>%
  # Add COVID-19 effect
  rbind(c(2020,0.97,0.985,0.94,0.94,0.94,0.94,0.93,0,0,0,0.92,0.94,0.95,0.95,0.95,0.95,0.93,0.9,0.99,0.9,0.95,0.95,0.99,0.93,0.95,0,0)) %>%
  rbind(c(2021,0.98,0.976,0.96,0.96,0.96,0.96,0.9533,0,0,0,0.9467,0.96,0.9667,0.9667,0.9667,0.9667,0.9533,0.9333,0.9933,0.9333,0.9667,0.9667,0.9933,0.9533,0.9667,0,0)) %>%
  # Add end of projection values (i.e. no exogenous change after 2060)
  rbind(c(2061,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)) %>%
  arrange(t)

ForTIMER.st <- ForTIMER %>%
  subset(variable == "demand_steel") %>%
  subset(select=-variable)%>%
  # Add historic values
  rbind(c(1970,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)) %>%
  rbind(c(2019,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)) %>%
  # Add COVID-19 effect
  rbind(c(2020,0.97,0.985,0.94,0.94,0.94,0.94,0.93,0,0,0,0.92,0.94,0.95,0.95,0.95,0.95,0.93,0.9,0.99,0.9,0.95,0.95,0.99,0.93,0.95,0,0)) %>%
  rbind(c(2021,0.98,0.976,0.96,0.96,0.96,0.96,0.9533,0,0,0,0.9467,0.96,0.9667,0.9667,0.9667,0.9667,0.9533,0.9333,0.9933,0.9333,0.9667,0.9667,0.9933,0.9533,0.9667,0,0)) %>%
  # Add end of projection values (i.e. no exogenous change after 2060)
  rbind(c(2061,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)) %>%
  arrange(t)

# Dataset per 'Scenario' with annual changes 
dataset_names <- list("concrete" = ForTIMER.ce,
                      'steel' = ForTIMER.st)
#export each data frames to separate sheets in same Excel file
openxlsx::write.xlsx(dataset_names, file = paste0(output_location,"demand_changes.xlsx"))
  
