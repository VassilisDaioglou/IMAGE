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

Regions = c(2,5,10,11,18,20,21,22,27,28)
Years = c("1980","1990","2000","2010","2015","2020","2025","2030","2035","2040","2045",
          "2050","2055","2060","2065","2070","2075","2080","2085","2090","2095","2100")
Quintiles = c(4,5,6,7,8,9,10,11,12,13)
UQuint = c(4,5,6,7,8)
RQuint = c(9,10,11,12,13)

ActiveRegion <- 21
Global1 <- 27
Global <- 28

TURQ_ID <- data.frame("TURQ"=c(1,2,3,4,5,6,7,8,9,10,11,12,13),
                      "LABEL"=c("Total","Urban","Rural","U1","U2","U3","U4","U5","R1","R2","R3","R4","R5"),
                      "Demography"=c("Total","Total Urban","Total Rural","Urban","Urban","Urban","Urban","Urban","Rural","Rural","Rural","Rural","Rural"),
                      "Quintile"=c("Total","Urban","Rural","Q1","Q2","Q3","Q4","Q5","Q1","Q2","Q3","Q4","Q5"))


# set higher RAM capacity for java (used in clsx package)
options(java.parameters = "-Xmx8000m")

# ---- INPUTS: Data ----
# set directory path 
setwd("C:/Users/Asus/Documents/Github/IMAGE/")
data <- "data/Inequality/TIMER_RESULTS.xlsx"
# Read Data Files for Baseline Scenario
EnFunc = read.xlsx(data, sheet = "EnUseFunction_TURQ", startRow=4)
EnUse = read.xlsx(data, sheet = "TotalEnUseTURQ", startRow=4)
FSInsul = read.xlsx(data, sheet = "FSInsul", startRow=4)
RenovRate = read.xlsx(data, sheet = "Renov_Rate_ave", startRow=4)
Pop = read.xlsx(data, sheet = "POP_q", startRow=4)

# ---- MUNGING ----
# ---- *** Population ----
colnames(Pop)[1:15] <- c("Year","Region",
                         "1","2","3",
                         "4","5","6","7","8",
                         "9","10","11","12","13")
Pop = melt(Pop, id.vars=c("Year","Region"))
colnames(Pop)[3] <- "TURQ"
Pop = spread(Pop,Region,value)
Pop$"27" <- NULL
Pop$Tot <- apply(Pop[,c(3:28)],1,sum)
Pop$Tot2 <- apply(Pop[,c(3:28)],1,sum)
Pop = melt(Pop, id.vars=c("Year","TURQ"))
colnames(Pop)[3] <- "Region"
levels(Pop$Region)[levels(Pop$Region)=="Tot"] <- "27"
levels(Pop$Region)[levels(Pop$Region)=="Tot2"] <- "28"

Pop$ID = paste(Pop$Year,"_",Pop$Region,"_",Pop$TURQ, sep="")
# ---- ***Energy Functions ----
colnames(EnFunc)[1:9] <- c("Year","Region","TURQ",
                           "APPL","LIGHT","COOK","WHEAT","SHEAT","COOL")
EnFunc = melt(EnFunc, id.vars=c("Year","Region","TURQ"))
EnFunc$Unit <- "GJ/yr"
colnames(EnFunc)[4] <- "Function" 
EnFunc$Quintile <- TURQ_ID[match(EnFunc$TURQ,TURQ_ID$TURQ),4]
EnFunc$Demographic <- TURQ_ID[match(EnFunc$TURQ,TURQ_ID$TURQ),3]
EnFunc$Func_Order <- factor(EnFunc$Function, level=c("COOK",
                                                      "LIGHT",
                                                      "WHEAT",
                                                      "SHEAT",
                                                     "COOL",
                                                      "APPL"))

EnFunc$ID = paste(EnFunc$Year,"_",EnFunc$Region,"_",EnFunc$TURQ, sep="")
EnFunc$Pop <- Pop[match(EnFunc$ID,Pop$ID),4]
EnFunc$ID <- NULL

EnFunc = EnFunc %>% mutate(EnFunc_pc = value/Pop)

# ---- ***Energy Use ----
colnames(EnUse)[1:11] <- c("Year","Region","TURQ",
                           "COAL","OIL","NGAS","MBIO","TBIO","H2","SHEAT","ELEC")
EnUse = melt(EnUse, id.vars=c("Year","Region","TURQ"))
EnUse$Unit <- "GJ/yr"
colnames(EnUse)[4] <- "Carrier" 
EnUse$Quintile <- TURQ_ID[match(EnUse$TURQ,TURQ_ID$TURQ),4]
EnUse$Demographic <- TURQ_ID[match(EnUse$TURQ,TURQ_ID$TURQ),3]
EnUse$En_Order <- factor(EnUse$Carrier, level=c("COAL",
                                                     "OIL",
                                                     "NGAS",
                                                     "MBIO",
                                                     "TBIO",
                                                     "H2",
                                                      "SHEAT",
                                                      "ELEC"))

EnUse$ID = paste(EnUse$Year,"_",EnUse$Region,"_",EnUse$TURQ, sep="")
EnUse$Pop <- Pop[match(EnUse$ID,Pop$ID),4]
EnUse$ID <- NULL

EnUse = EnUse %>% mutate(EnUse_pc = value/Pop)

# ---- ***Insulation Levels ----
colnames(FSInsul)[1:9] <- c("Year","Region","TURQ",
                            "1","2","3","4","5","6")
FSInsul = melt(FSInsul, id.vars=c("Year","Region","TURQ"))
FSInsul$value <-FSInsul$value / 1e9
FSInsul$Unit <- "Bill. m^2"
colnames(FSInsul)[4] <- "EffLevel" 
FSInsul$Quintile <- TURQ_ID[match(FSInsul$TURQ,TURQ_ID$TURQ),4]
FSInsul$Demographic <- TURQ_ID[match(FSInsul$TURQ,TURQ_ID$TURQ),3]

# ---- ***Renovation Rate ----
colnames(RenovRate)[1:15] <- c("Year","Region",
                               "1","2","3",
                               "4","5","6","7","8",
                               "9","10","11","12","13")
RenovRate = melt(RenovRate, id.vars=c("Year","Region"))
RenovRate$value <- RenovRate$value * 100
colnames(RenovRate)[3] <- "TURQ"
RenovRate$Quintile <- TURQ_ID[match(RenovRate$TURQ,TURQ_ID$TURQ),4]
RenovRate$Demographic <- TURQ_ID[match(RenovRate$TURQ,TURQ_ID$TURQ),3]

# ---- ***Filtering
# Subset Data in order to reduce computational requirements
EnFunc = subset(EnFunc, Region %in% Regions & Year %in% Years)
EnUse = subset(EnUse, Region %in% Regions & Year %in% Years)
FSInsul = subset(FSInsul, Region %in% Regions & Year %in% Years)
RenovRate = subset(RenovRate, Region %in% Regions & Year %in% Years)

#
# ---- LABELS ----
reg_labels <-c("2"="USA",
               "5"="Brazil",
               "10"="S. Africa",
               "11"="W. Europe",
               "16"="Russia",
               "18"="India",
               "20"="China",
               "27"="World")

func_labels <-c("APPL"="Appliances","LIGHT"="Lighting","COOK"="Cooking",
                "WHEAT"="Water Heating","SHEAT"="Space Heating","COOL"="Space Cooling")

EC_labels <-c("COAL"="Coal","OIL"="Liquid (Fossil)","NGAS"="Natural Gas",
                "MBIO"="Modern Biofuel","TBIO"="Traditional Biofuel","H2"="Hydrogen",
              "SHEAT"="District Heating","ELEC"="Electricity")

turq_labels <-c("1"="Total","2"="Urban","3"="Rural",
                "4"="U1","5"="U2","6"="U3","7"="U4","8"="U5",
                "9"="R1","10"="R2","11"="R3","12"="R4","13"="R5")

q_labels <-c("4"="Q1","5"="Q2","6"="Q3","7"="Q4","8"="Q5",
                "9"="Q1","10"="Q2","11"="Q3","12"="Q4","13"="Q5")

#
# ---- FIGURES ----
# ---- FIG: Functions ----
Func.Glo <- ggplot(data=subset(EnFunc, TURQ %in% Quintiles & Region==Global)
                  , aes(x=Year,y = EnFunc_pc, fill=Func_Order)) + 
  geom_bar(stat="identity") +
  xlim(2020,2100) +
  xlab("") + ylab("GJ/cap") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_fill_manual(values=c("chartreuse","yellow","chocolate1","firebrick","cornflowerblue","darkgray"),
                    name="Energy\nFunction",
                    breaks=c("COOK","LIGHT","WHEAT","SHEAT","COOL","APPL"),
                    labels=c("Cooking","Lighting","Water Heating","Space Heating","Space Cooling","Appliances")) +
  facet_grid(Demographic~Quintile) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
Func.Glo

Func.R <- ggplot(data=subset(EnFunc, TURQ %in% Quintiles & Region==ActiveRegion)
                   , aes(x=Year,y = EnFunc_pc, fill=Func_Order)) + 
  geom_bar(stat="identity") +
  xlim(2020,2100) +
  xlab("") + ylab("GJ/cap") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_fill_manual(values=c("chartreuse","yellow","chocolate1","firebrick","cornflowerblue","darkgray"),
                    name="Energy\nFunction",
                    breaks=c("COOK","LIGHT","WHEAT","SHEAT","COOL","APPL"),
                    labels=c("Cooking","Lighting","Water Heating","Space Heating","Space Cooling","Appliances")) +
  facet_grid(Demographic~Quintile) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
Func.R

#
# ---- FIG: Carriers ----
EC.Glo <- ggplot(data=subset(EnUse, TURQ %in% Quintiles & Region==Global & !(Carrier=="H2"))
                   , aes(x=Year,y = EnUse_pc, fill=En_Order)) + 
  geom_bar(stat="identity") +
  xlim(2020,2100) +
  xlab("") + ylab("GJ/cap") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_fill_manual(values=c("gray22","darkorange4","cornflowerblue","brown","forestgreen","darksalmon","darkgray"),
                    name="Energy\nCarrier",
                    breaks=c("COAL","OIL","NGAS","MBIO","TBIO","SHEAT","ELEC"),
                    labels=c("Coal","Liquid (Fossil)","Natural Gas","Modern Biofuel","Traditional Biofuel","District Heating","Electricity")) +
  facet_grid(Demographic~Quintile) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
EC.Glo

EC.R <- ggplot(data=subset(EnUse, TURQ %in% Quintiles & Region==ActiveRegion & !(Carrier=="H2"))
                 , aes(x=Year,y = EnUse_pc, fill=En_Order)) + 
  geom_bar(stat="identity") +
  xlim(2020,2100) +
  xlab("") + ylab("GJ/cap") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_fill_manual(values=c("gray22","darkorange4","cornflowerblue","brown","forestgreen","darksalmon","darkgray"),
                    name="Energy\nCarrier",
                    breaks=c("COAL","OIL","NGAS","MBIO","TBIO","SHEAT","ELEC"),
                    labels=c("Coal","Liquid (Fossil)","Natural Gas","Modern Biofuel","Traditional Biofuel","District Heating","Electricity")) +
  facet_grid(Demographic~Quintile) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
EC.R

#
# ---- FIG: Insulation ----
FSInsul.Glo <- ggplot(data=subset(FSInsul, TURQ %in% Quintiles & Region==Global1)
                 , aes(x=Year,y = value, fill=EffLevel)) + 
  geom_bar(stat="identity") +
  xlim(2020,2100) +
  xlab("") + ylab("Bill. m^2/yr") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_fill_manual(values=c("firebrick","chocolate1","darkgray","cornflowerblue","chartreuse","forestgreen"),
                    name="Insulation\nLevel",
                    breaks=c("1","2","3","4","5","6"),
                    labels=c("1","2","3","4","5","6")) +
  facet_grid(Demographic~Quintile, scales = "free_y") + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
FSInsul.Glo

#
# ---- FIG: RenovationRate ----
RenovRate.Glo <- ggplot(data=subset(RenovRate, TURQ %in% Quintiles & Region==27)
                      , aes(x=Year,y = value, colour=Quintile)) + 
  geom_line(alpha=1, size=1.5) +
  xlim(2020,2100) +
  xlab("") + ylab("%") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_colour_manual(values=c("gray22","gray50","Darkorange1","dodgerblue","forestgreen"),
                    name="Quintile",
                    breaks=c("Q1","Q2","Q3","Q4","Q5"),
                    labels=c("Q1","Q2","Q3","Q4","Q5")) +
  facet_grid(Demographic~.) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
RenovRate.Glo

RenovRate.R <- ggplot(data=subset(RenovRate, TURQ %in% Quintiles & (Region==18|Region==11))
                        , aes(x=Year,y = value, colour=Quintile)) + 
  geom_line(alpha=1, size=1.5) +
  xlim(2020,2100) +
  xlab("") + ylab("%") +
  theme_bw() +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) + 
  theme(text= element_text(size=FSizeStrip, face="plain"), axis.text.x = element_text(angle=66, size=FSizeAxis, hjust=1), axis.text.y = element_text(size=FSizeAxis)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  theme(legend.position="right") +
  scale_colour_manual(values=c("gray22","gray50","Darkorange1","dodgerblue","forestgreen"),
                      name="Quintile",
                      breaks=c("Q1","Q2","Q3","Q4","Q5"),
                      labels=c("Q1","Q2","Q3","Q4","Q5")) +
  facet_grid(Demographic~Region, labeller=labeller(Region=reg_labels)) + 
  theme(strip.text.x = element_text(size = FSizeStrip, face="bold"), strip.text.y = element_text(size = FSizeStrip, face="bold"))
RenovRate.R

#
# # ---- OUTPUTS ----
png(file = "output/Inequality/Functions_Global.png", width = 8*ppi, height = 4*ppi, units = "px", res = ppi)
plot(Func.Glo)
dev.off()

png(file = "output/Inequality/Functions_R.png", width = 8*ppi, height = 4*ppi, units = "px", res = ppi)
plot(Func.SA)
dev.off()

png(file = "output/Inequality/Carriers_Global.png", width = 8*ppi, height = 4*ppi, units = "px", res = ppi)
plot(EC.Glo)
dev.off()

png(file = "output/Inequality/Carriers_R.png", width = 8*ppi, height = 4*ppi, units = "px", res = ppi)
plot(EC.SA)
dev.off()

png(file = "output/Inequality/Insulation_Global.png", width = 8*ppi, height = 4*ppi, units = "px", res = ppi)
plot(FSInsul.Glo)
dev.off()

png(file = "output/Inequality/RenovRate_Regional.png", width = 8*ppi, height = 4*ppi, units = "px", res = ppi)
plot(RenovRate.R)
dev.off()

# # #

