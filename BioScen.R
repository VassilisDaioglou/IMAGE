# R script to process results for "Integrated Biomass Scenarios" Paper
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
library(ggpubr)
library(gridExtra)
library(gdata)

# ---- CONSTANTS ----
ppi <- 300
CH42CO2 <- 28 # 100YR GWP, Myhre, G., D. Sindell, F.-M. Bréon, W. Collins, J. Fuglestvedt, J. Huang, D. Koch, J.-F. Lamarque, D. Lee, B. Mendoza, T. Nakajima, A. Robock, G. Stephens, T. Takemura and H. Zhang, 2013: Anthropogenic and Natural Radiative Forcing. In: Climate Change 2013: The Physical Science Basis. Contribution of Working Group I to the Fifth Assessment Report of the Intergovernmental Panel on Climate Change [Stocker, T.F., D. Qin, G.-K. Plattner, M. Tignor, S.K.Allen, J. Boschung, A. Nauels, Y. Xia, V. Bex and P.M. Midgley(eds.)]. Cambridge University Press, Cambridge, United Kingdom and New York, NY, USA.
N2O2CO2 <- 265 # 100YR GWP, Myhre, G., D. Sindell, F.-M. Bréon, W. Collins, J. Fuglestvedt, J. Huang, D. Koch, J.-F. Lamarque, D. Lee, B. Mendoza, T. Nakajima, A. Robock, G. Stephens, T. Takemura and H. Zhang, 2013: Anthropogenic and Natural Radiative Forcing. In: Climate Change 2013: The Physical Science Basis. Contribution of Working Group I to the Fifth Assessment Report of the Intergovernmental Panel on Climate Change [Stocker, T.F., D. Qin, G.-K. Plattner, M. Tignor, S.K.Allen, J. Boschung, A. Nauels, Y. Xia, V. Bex and P.M. Midgley(eds.)]. Cambridge University Press, Cambridge, United Kingdom and New York, NY, USA.

IMAGERegion = as.data.frame(rbind(c(1,"CAN"),c(2,"USA"),c(3,"MEX"),c(4,"RCAM"),c(5,"BRA"),c(6,"RSAM"),c(7,"NAF"),c(8,"WAF"),c(9,"EAF"),c(10,"SAF"),c(11,"WEU"),c(12,"CEU"),c(13,"TUR"),c(14,"UKR"),c(15,"STAN"),
                                  c(16,"RUS"),c(17,"ME"),c(18,"INDIA"),c(19,"KOR"),c(20,"CHN"),c(21,"SEAS"),c(22,"INDO"),c(23,"JAP"),c(24,"OCE"),c(25,"RSAS"),c(26,"RSAF"),c(27,"World")))

# ---- INPUTS: IMAGE ----
# set directory path 
#setwd("~/disks/y/ontwapps/Timer/Users/Vassilis/Projects - documents/SDG/Edward/R-Scripts/")
setwd("~/disks/y/ontwapps/Timer/Users/Vassilis/Papers/6. Synthesis/For Submission/Data Processing - R")

# Read Data File
SSP1=read.xlsx("data/SSP1.xls", sheetIndex=1)
SSP2=read.xlsx("data/SSP2.xls", sheetIndex=1)
SSP3=read.xlsx("data/SSP3.xls", sheetIndex=1)
SSP1.19=read.xlsx("data/SSP1_20.xls", sheetIndex=1)
SSP2.19=read.xlsx("data/SSP2_20.xls", sheetIndex=1)
SSP1.26=read.xlsx("data/SSP1_450.xls", sheetIndex=1)
SSP2.26=read.xlsx("data/SSP2_450.xls", sheetIndex=1)
SSP3.34=read.xlsx("data/SSP3_550.xls", sheetIndex=1)

SSP=rbind(SSP1,SSP2,SSP3,
           SSP1.19,SSP2.19,
           SSP1.26,SSP2.26,SSP3.34)

SSP[] <- lapply(SSP, as.character)
SSP=melt(SSP, id.vars=c("Model","Scenario","Region","Variable","Unit"), na.rm=TRUE)
colnames(SSP)[6] <-"Year"
SSP$Year = as.numeric(substr(SSP$Year, start=2, stop=5))
SSP$value = as.numeric(substr(SSP$value, start=1, stop=5))
SSP$Model <- NULL

# ---- INPUTS: TIMER ----
TechBioLandAll=read.csv("data/TechBioLandAll.csv", sep=";", dec=".", stringsAsFactors = FALSE, colClasses = "character")
TechBioLandAll=melt(TechBioLandAll, id.vars=c("Year","LandType"), na.rm=TRUE)
colnames(TechBioLandAll)[2] <- "Variable"
colnames(TechBioLandAll)[3] <- "Scenario"
TechBioLandAll$Unit <- "MHa"
TechBioLandAll$VARID <- "Land"
TechBioLandAll$REGION <- "World"
TechBioLandAll$VARID2=paste(TechBioLandAll$Scenario, TechBioLandAll$Year)
TechBioLandAll$Year = as.numeric(substr(TechBioLandAll$Year, start=1, stop=4))
TechBioLandAll$value = as.numeric(substr(TechBioLandAll$value, start=1, stop=10))

TechBioLandAllR=read.csv("data/TechBioLandAllR.csv", sep=",", dec=".", stringsAsFactors = FALSE, colClasses = "character")
TechBioLandAllR=melt(TechBioLandAllR, id.vars=c("Year","Region","Cat"), na.rm=TRUE)
colnames(TechBioLandAllR)[4] <- "SCENARIO"
colnames(TechBioLandAllR)[3] <- "LandType"
TechBioLandAllR$Year = as.numeric(substr(TechBioLandAllR$Year, start=1, stop=4))
TechBioLandAllR$value = as.numeric(substr(TechBioLandAllR$value, start=1, stop=15))
TechBioLandAllR$LandType[TechBioLandAllR$LandType==1] <- "Abandoned"
TechBioLandAllR$LandType[TechBioLandAllR$LandType==2] <- "Rest"

BioSupplyAll=read.csv("data/BioSupply.csv", sep=",", dec=".", stringsAsFactors = FALSE, colClasses = "character")
BioSupplyAll=subset(BioSupplyAll, select=c(Year,CostPot,Cat,SSP1,SSP2,SSP3))
BioSupplyAll=melt(BioSupplyAll, id.vars=c("Year","CostPot","Cat"), na.rm=TRUE)
BioSupplyAll=spread(BioSupplyAll,CostPot,value)
colnames(BioSupplyAll)[3] <- "SCENARIO"
colnames(BioSupplyAll)[4] <- "Potential"
colnames(BioSupplyAll)[5] <- "Cost"
BioSupplyAll$Year = as.numeric(substr(BioSupplyAll$Year, start=1, stop=4))
BioSupplyAll$Cat = as.numeric(substr(BioSupplyAll$Cat, start=1, stop=4))
BioSupplyAll$Cost = as.numeric(substr(BioSupplyAll$Cost, start=1, stop=4))
BioSupplyAll$LandCat[BioSupplyAll$Cat<=260] <- "Abandoned"
BioSupplyAll$LandCat[BioSupplyAll$Cat>260&BioSupplyAll$Cat<=520] <- "Rest"
BioSupplyAll$LandCat[BioSupplyAll$Cat>520] <- "Residues"

BioSupplyTotAll=read.csv("data/BioSupplyTotAll.csv", sep=",", dec=".", stringsAsFactors = FALSE, colClasses = "character")
BioSupplyTotAll=melt(BioSupplyTotAll, id.vars=c("Year","Cat"), na.rm=TRUE)
colnames(BioSupplyTotAll)[3] <- "SCENARIO"
colnames(BioSupplyTotAll)[2] <- "LandType"
BioSupplyTotAll$Year = as.numeric(substr(BioSupplyTotAll$Year, start=1, stop=4))
BioSupplyTotAll$value = as.numeric(substr(BioSupplyTotAll$value, start=1, stop=15))

BioSupplyTotAllR=read.csv("data/BioSupplyTotAllR.csv", sep=",", dec=".", stringsAsFactors = FALSE, colClasses = "character")
BioSupplyTotAllR=melt(BioSupplyTotAllR, id.vars=c("Year","Region","Cat"), na.rm=TRUE)
colnames(BioSupplyTotAllR)[4] <- "SCENARIO"
colnames(BioSupplyTotAllR)[3] <- "LandType"
BioSupplyTotAllR$Year = as.numeric(substr(BioSupplyTotAllR$Year, start=1, stop=4))
BioSupplyTotAllR$value = as.numeric(substr(BioSupplyTotAllR$value, start=1, stop=15))
BioSupplyTotAllR$LandType[BioSupplyTotAllR$LandType==1] <- "Abandoned"
BioSupplyTotAllR$LandType[BioSupplyTotAllR$LandType==2] <- "Rest"
BioSupplyTotAllR$LandType[BioSupplyTotAllR$LandType==3] <- "Residues"

FeedNonEnBioAll = read.csv("data/FeedNonEnBioAll.csv", sep=",", dec=".", stringsAsFactors = FALSE, colClasses = "character")
FeedNonEnBioAll = melt(FeedNonEnBioAll, id.vars=c("Year","Scenario"), na.rm=TRUE)
FeedNonEnBioAll$value = as.numeric(substr(FeedNonEnBioAll$value, start=1, stop=10))
FeedNonEnBioAll = FeedNonEnBioAll %>% mutate(value_cor=value/1000)
FeedNonEnBioAll$Unit <- "EJ/yr"
FeedNonEnBioAll$VARID <- "SecE"
FeedNonEnBioAll$REGION <- "World"
FeedNonEnBioAll$Year = as.numeric(substr(FeedNonEnBioAll$Year, start=1, stop=4))
FeedNonEnBioAll$value <- NULL
colnames(FeedNonEnBioAll)[4] <- "value"
FeedNonEnBioAll$variable2[FeedNonEnBioAll$variable=="Bioenergy"] <- "SecENonEnBio"
FeedNonEnBioAll$variable2[FeedNonEnBioAll$variable=="Total"] <- "SecENonEnTotal"
FeedNonEnBioAll$variable <- NULL
colnames(FeedNonEnBioAll)[7] <- "Variable"
FeedNonEnBioAll$VARID2=paste(FeedNonEnBioAll$Scenario, FeedNonEnBioAll$Year)

YieldAgrAll = read.csv("data/YieldAgrAll.csv", sep=",", dec=".", stringsAsFactors = FALSE, colClasses = "character")
YieldAgrAll = melt(YieldAgrAll, id.vars=c("Year","CROP"), na.rm=TRUE)
colnames(YieldAgrAll)[3] <- "SCENARIO"
YieldAgrAll$Year = as.numeric(substr(YieldAgrAll$Year, start=1, stop=4))
YieldAgrAll$value = as.numeric(substr(YieldAgrAll$value, start=1, stop=6))

YieldAgrAllR = read.csv("data/YieldAgrAllR.csv", sep=",", dec=".", stringsAsFactors = FALSE, colClasses = "character")
YieldAgrAllR = melt(YieldAgrAllR, id.vars=c("Year","REGION","CROP"), na.rm=TRUE)
colnames(YieldAgrAllR)[4] <- "SCENARIO"
YieldAgrAllR$Year = as.numeric(substr(YieldAgrAllR$Year, start=1, stop=4))
YieldAgrAllR$value = as.numeric(substr(YieldAgrAllR$value, start=1, stop=6))

YieldMargAll = read.csv("data/YieldMargAll.csv", sep=",", dec=".", stringsAsFactors = FALSE, colClasses = "character")
YieldMargAll = melt(YieldMargAll, id.vars=c("Year","LandType"), na.rm=TRUE)
colnames(YieldMargAll)[3] <- "SCENARIO"
YieldMargAll$Year = as.numeric(substr(YieldMargAll$Year, start=1, stop=4))
YieldMargAll$value = as.numeric(substr(YieldMargAll$value, start=1, stop=6))

#
# ---- PREPARE FILES ----
# Organise Data Files
SSP$Variable <-gsub("Emissions","Emis",SSP$Variable,fixed=F)
SSP$Variable <-gsub("Land Cover","Land",SSP$Variable,fixed=F)
SSP$Variable <-gsub("Primary Energy","Prim",SSP$Variable,fixed=F)
SSP$Variable <-gsub("Final Energy","FinE",SSP$Variable,fixed=F)
SSP$Variable <-gsub("Secondary Energy","SecE",SSP$Variable,fixed=F)
SSP$Variable <-gsub("Temperature","Temp",SSP$Variable,fixed=F)
SSP$Variable <-gsub("Native category","NatCat",SSP$Variable,fixed=F)
SSP$Variable <-gsub("Biomass","Bio",SSP$Variable,fixed=F)
SSP$Variable <-gsub("Fossil Fuels and Industry","FFI",SSP$Variable,fixed=F)
SSP$Variable <-gsub("Energy Demand","Dem",SSP$Variable,fixed=F)
SSP$Variable <-gsub("Energy Supply","Sup",SSP$Variable,fixed=F)
SSP$Variable <-gsub("Energy Crops","EneCrop",SSP$Variable,fixed=F)
SSP$Variable <-gsub("Non-Bio Renewables","Ren",SSP$Variable,fixed=F)
SSP$Variable <-gsub("Agricultural Production","AgriProd",SSP$Variable,fixed=F)
SSP$Variable <-gsub("Natural Gas","NatGas",SSP$Variable,fixed=F)
SSP$Variable <-gsub("Primary Level","Prim",SSP$Variable,fixed=F)
SSP$Variable <-gsub("Secondary Level","Sec",SSP$Variable,fixed=F)
SSP$Variable <-gsub("[[:punct:]]","",SSP$Variable,fixed=F)
SSP$Variable <-gsub("[[:space:]]","",SSP$Variable,fixed=F)

SSP$VARID = substr(SSP$Variable, start=1, stop=4)
SSP$VARID2 = paste(SSP$Scenario,SSP$Year)

# Regional Aggregation
SSP=spread(SSP,Region,value)
SSP = SSP %>% mutate(OECD90=CAN+JAP+OCE+TUR+USA+WEU)
SSP = SSP %>% mutate(REF=CEU+RUS+STAN+UKR)
SSP = SSP %>% mutate(ASIA=CHN+INDIA+INDO+KOR+RSAS+SEAS)
SSP = SSP %>% mutate(MAF=EAF+ME+NAF+RSAF+SAF+WAF)
SSP = SSP %>% mutate(LAM=BRA+MEX+RCAM+RSAM)
SSP = subset(SSP, select=c("Scenario","Variable","Unit","Year","VARID","VARID2","OECD90","REF","ASIA","MAF","LAM","World"))

# Correct Non-CO2 gasses
SSP = melt(SSP, id.vars=c("Scenario","Variable","Unit","Year","VARID","VARID2"), na.rm=FALSE )
colnames(SSP)[7] <- "REGION"

SSP.temp=subset(SSP, Variable=="EmisCH4"|Variable=="EmisN2O")
SSP.temp=spread(SSP.temp,Variable,value)
SSP.temp=SSP.temp %>% mutate(EmisCH4_cor=EmisCH4*CH42CO2)
SSP.temp=SSP.temp %>% mutate(EmisN2O_cor=(EmisN2O*N2O2CO2)/1000)
SSP.temp = melt(SSP.temp, id.vars=c("Scenario","Unit","Year","VARID","VARID2","REGION"), na.rm=FALSE )
colnames(SSP.temp)[7] <- "Variable"
SSP.temp = subset(SSP.temp, Variable=="EmisCH4_cor"|Variable=="EmisN2O_cor")
SSP.temp$Unit <- "CO2-eq/yr"
SSP.temp=na.omit(SSP.temp)

SSP.temp2=subset(SSP, !(Variable=="EmisCH4"|Variable=="EmisN2O"))
SSP.data = rbind(SSP.temp,SSP.temp2)
rm(SSP.temp,SSP.temp2)
# Add Extra TIMER data not in super-mapping
SSP.data = rbind(SSP.data,TechBioLandAll,FeedNonEnBioAll)

# Scenario Order
SSP.data$ScenOrder = factor(SSP.data$Scenario, levels=c("SSP1","SSP1_450","SSP1_20","SSP2","SSP2_450","SSP2_20","SSP3","SSP3_550"))

#
# ---- DFs FOR FIGURES ----
SSP.Land = subset(SSP.data, VARID=="Land")
SSP.Land = subset(SSP.Land, Variable=="LandOtherNaturalLand"|Variable=="LandCropland"|Variable=="LandCroplandEneCrop"|Variable=="LandForest"|Variable=="LandPasture"|Variable=="Abandoned"|Variable=="Rest")
SSP.Land = spread(SSP.Land, Variable, value) 
SSP.Land = SSP.Land %>% mutate(PotBioLand = (Abandoned + Rest)/1000000)
SSP.Land$Abandoned <- NULL
SSP.Land$Rest <- NULL
SSP.Land=melt(SSP.Land, id.vars=c("Scenario","Unit","Year","VARID","VARID2","REGION","ScenOrder"), na.rm=TRUE)
SSP.Land$Unit <-"Mha"
SSP.Land=na.omit(SSP.Land)
SSP.Land=spread(SSP.Land,variable,value)
SSP.Land = SSP.Land %>% mutate(CroplandFrac=LandCropland/(LandCropland+LandForest+LandOtherNaturalLand+LandPasture+PotBioLand))
SSP.Land = SSP.Land %>% mutate(ForestFrac=LandForest/(LandCropland+LandForest+LandOtherNaturalLand+LandPasture+PotBioLand))
SSP.Land = SSP.Land %>% mutate(OtherNatLandFrac=LandOtherNaturalLand/(LandCropland+LandForest+LandOtherNaturalLand+LandPasture+PotBioLand))
SSP.Land = SSP.Land %>% mutate(PastureFrac=LandPasture/(LandCropland+LandForest+LandOtherNaturalLand+LandPasture+PotBioLand))
SSP.Land = SSP.Land %>% mutate(PotBioLandFrac=PotBioLand/(LandCropland+LandForest+LandOtherNaturalLand+LandPasture+PotBioLand))
SSP.Land = SSP.Land %>% mutate(CheckFrac=CroplandFrac+ForestFrac+OtherNatLandFrac+PastureFrac+PotBioLandFrac)
SSP.Land=melt(SSP.Land, id.vars=c("Scenario","Unit","Year","VARID","VARID2","REGION","ScenOrder"), na.rm=TRUE)
SSP.Land$Year = as.character(SSP.Land$Year)

SSP.Land$VarOrder = factor(SSP.Land$variable, levels=c("PotBioLand",
                                                       "LandOtherNaturalLand",
                                                       "LandCroplandEneCrop",
                                                       "LandForest",
                                                       "LandPasture",
                                                       "LandCropland",
                                                       "PotBioLandFrac",
                                                       "OtherNatLandFrac",
                                                       "ForestFrac",
                                                       "PastureFrac",
                                                       "CroplandFrac",
                                                       "CheckFrac"))

SSP.Prim = subset(SSP.data, VARID=="Prim")
SSP.Prim$Year = as.character(SSP.Prim$Year)
SSP.Prim$VarOrder = factor(SSP.Prim$Variable, levels=c("PrimNuclear",
                                                       "PrimRen",
                                                       "PrimBioModern",
                                                       "PrimBioTraditional",
                                                       "PrimGas",
                                                       "PrimOil",
                                                       "PrimCoal"))


SSP.Emis = subset(SSP.data, VARID=="Emis")
SSP.Emis$Year = as.character(SSP.Emis$Year)
SSP.Emis$VarOrder = factor(SSP.Emis$Variable, levels=c("EmisN2O_cor",
                                                       "EmisCH4_cor",
                                                       "EmisCO2LandUse",
                                                       "EmisCO2FFIDem",
                                                       "EmisCO2FFISup"))
SSP.BioP = subset(SSP.Prim, Variable=="PrimBio"|Variable=="PrimBioEneCrop"|Variable=="PrimBioModern"|Variable=="PrimBioresidues"|Variable=="PrimBioTraditional"|
                   Variable=="PrimBioNatCat1"|Variable=="PrimBioNatCat2"|Variable=="PrimBioNatCat3"|Variable=="PrimBioNatCat4"|Variable=="PrimBioNatCat5"|Variable=="PrimBioNatCat6")

SSP.BioProd = subset(SSP.BioP, Variable=="PrimBioEneCrop"|Variable=="PrimBioNatCat6")
SSP.BioProd = spread(SSP.BioProd, Variable, value)
colnames(SSP.BioProd)[10] <- "PrimBioResidues"
SSP.BioProd = SSP.BioProd %>% mutate(PrimBioTotal = PrimBioEneCrop + PrimBioResidues)
SSP.BioProd = melt(SSP.BioProd, id.vars=c("Scenario","Unit","Year","VARID","VARID2","REGION","ScenOrder","VarOrder"))

SSP.BioS = subset(SSP.data, VARID=="SecE")
SSP.BioS$Year = as.character(SSP.BioS$Year)
SSP.BioS$VarOrder = factor(SSP.BioS$Variable, levels = c("SecENonEnTotal",
                                                         "SecENonEnBio",
                                                         "SecEElectricityBiowoCCS",
                                                         "SecEElectricityBiowCCS",
                                                         "SecEHydrogenBiowoCCS",
                                                         "SecEHydrogenBiowCCS",
                                                         "SecELiquidsBio",
                                                         "SecESolidsBio"))

SSP.LUCBio = subset(SSP.data, Variable=="EmisCO2LandUse"|Variable=="PrimBioEneCrop")

BioSupply.Order <- BioSupplyAll
BioSupply.Order <- BioSupply.Order[with(BioSupply.Order, order(Year, SCENARIO, LandCat, Cost)),]
BioSupply.Order$ID = paste(BioSupply.Order$Year,BioSupply.Order$SCENARIO,BioSupply.Order$LandCat)
BioSupply.Order$CumPot <- ave(BioSupply.Order$Potential, BioSupply.Order$ID, FUN=cumsum)
BioSupply.Order$ID <- NULL
BioSupply.Order$CumPot = as.numeric(substr(BioSupply.Order$CumPot, start=1, stop=15))
BioSupply.Order = BioSupply.Order %>% mutate(CumPotCor = CumPot/1000000000)

BioSupply.Total = BioSupplyAll
BioSupply.Total = spread(BioSupply.Total,LandCat,Potential)
BioSupply.Total$Abandoned[is.na(BioSupply.Total$Abandoned)] <- 0
BioSupply.Total$Residues[is.na(BioSupply.Total$Residues)] <- 0
BioSupply.Total$Rest[is.na(BioSupply.Total$Rest)] <- 0
BioSupply.Total$Abandoned = as.numeric(substr(BioSupply.Total$Abandoned, start=1, stop=15))
BioSupply.Total$Rest = as.numeric(substr(BioSupply.Total$Rest, start=1, stop=15))
BioSupply.Total$Residues = as.numeric(substr(BioSupply.Total$Residues, start=1, stop=15))
BioSupply.Total = BioSupply.Total %>% mutate(Total = (Abandoned + Residues + Rest)/1000000000)
BioSupply.Total = subset(BioSupply.Total, select=-c(Abandoned,Residues,Rest))
BioSupply.Total <- BioSupply.Total[with(BioSupply.Total, order(Year, SCENARIO, Cost)),]
BioSupply.Total$ID = paste(BioSupply.Total$Year,BioSupply.Total$SCENARIO)
BioSupply.Total$CumPot <- ave(BioSupply.Total$Total, BioSupply.Total$ID, FUN=cumsum)

BioSupplyEC = subset(BioSupplyTotAllR, LandType=="Abandoned"|LandType=="Rest")
BioSupplyEC = spread(BioSupplyEC,LandType,value)
BioSupplyEC = BioSupplyEC %>% mutate(TotalEC = Abandoned+Rest)
BioSupplyEC$Region2 = IMAGERegion[match(BioSupplyEC$Region,IMAGERegion$V1),"V2"]
BioSupplyEC = subset(BioSupplyEC, select = -c(Region,Abandoned,Rest))
BioSupplyEC = spread(BioSupplyEC,Region2,TotalEC)
BioSupplyEC = BioSupplyEC %>% mutate(OECD90=CAN+JAP+OCE+TUR+USA+WEU)
BioSupplyEC = BioSupplyEC %>% mutate(REF=CEU+RUS+STAN+UKR)
BioSupplyEC = BioSupplyEC %>% mutate(ASIA=CHN+INDIA+INDO+KOR+RSAS+SEAS)
# BioSupplyEC = BioSupplyEC %>% mutate(MAF=EAF+ME+NAF+RSAF+SAF+WAF)
BioSupplyEC = BioSupplyEC %>% mutate(MAF=ME+NAF+RSAF+SAF) # Exclude EAF+WAF which show strange behaviour due to agri expansion
BioSupplyEC = BioSupplyEC %>% mutate(LAM=BRA+MEX+RCAM+RSAM)
BioSupplyEC = subset(BioSupplyEC, select=c("SCENARIO","Year","OECD90","REF","ASIA","MAF","LAM","World"))
BioSupplyEC = melt(BioSupplyEC, id.vars=c("SCENARIO","Year"), na.rm=TRUE)
colnames(BioSupplyEC)[3:4] <- c("Region","ECPotential")
BioSupplyEC$ID = paste(BioSupplyEC$SCENARIO,BioSupplyEC$Year,BioSupplyEC$Region)

BioSupplyLand = TechBioLandAllR
BioSupplyLand = spread(BioSupplyLand,LandType,value)
BioSupplyLand = BioSupplyLand %>% mutate(TotalLand = Abandoned+Rest)
BioSupplyLand$Region2 = IMAGERegion[match(BioSupplyLand$Region,IMAGERegion$V1),"V2"]
BioSupplyLand = subset(BioSupplyLand, select = -c(Region,Abandoned,Rest))
BioSupplyLand = spread(BioSupplyLand,Region2,TotalLand)
BioSupplyLand = BioSupplyLand %>% mutate(OECD90=CAN+JAP+OCE+TUR+USA+WEU)
BioSupplyLand = BioSupplyLand %>% mutate(REF=CEU+RUS+STAN+UKR)
BioSupplyLand = BioSupplyLand %>% mutate(ASIA=CHN+INDIA+INDO+KOR+RSAS+SEAS)
# BioSupplyLand = BioSupplyLand %>% mutate(MAF=EAF+ME+NAF+RSAF+SAF+WAF)
BioSupplyLand = BioSupplyLand %>% mutate(MAF=ME+NAF+RSAF+SAF) # Exclude EAF+WAF which show strange behaviour
BioSupplyLand = BioSupplyLand %>% mutate(LAM=BRA+MEX+RCAM+RSAM)
BioSupplyLand = subset(BioSupplyLand, select=c("SCENARIO","Year","OECD90","REF","ASIA","MAF","LAM","World"))
BioSupplyLand = melt(BioSupplyLand, id.vars=c("SCENARIO","Year"), na.rm=TRUE)
colnames(BioSupplyLand)[3:4] <- c("Region","LandPotential")
BioSupplyLand$ID = paste(BioSupplyLand$SCENARIO,BioSupplyLand$Year,BioSupplyLand$Region)


SSP.Clim = subset(SSP.data, VARID=="Forc"|VARID=="Temp"|Variable=="PriceCarbon")

#
# ---- RESULTS FOR PAPER ----
# Land Use
SSP.Land1 = subset(SSP.Land, select=c(Scenario,Unit,Year,REGION,variable,value))

# Primary Energy
SSP.Prim1 = subset(SSP.Prim, select=c(Scenario,Unit,Year,REGION,Variable,value))
SSP.Prim1 = spread(SSP.Prim1,Variable,value)
SSP.Prim1 = SSP.Prim1 %>% mutate(PrimFossil = PrimCoal+PrimGas+PrimOil)
SSP.Prim1 = SSP.Prim1 %>% mutate(FossilFrac = PrimFossil/Prim)
SSP.Prim1 = SSP.Prim1 %>% mutate(BioFrac = PrimBioModern/Prim)
SSP.Prim1 = melt(SSP.Prim1, id.vars=c("Scenario","Unit","Year","REGION"), na.rm=FALSE)

# Climate
SSP.Clim1 = subset(SSP.Clim, select=c(Scenario,Unit,Year,REGION,Variable,value))
SSP.Clim1 = na.omit(SSP.Clim1)

# Secondary Energy
SSP.BioS1 = subset(SSP.BioS, select=c(Scenario,Unit,Year,REGION,Variable,value))

# Final Energy
SSP.FinE = subset(SSP.data, VARID=="FinE")
SSP.FinE = subset(SSP.FinE, select=c(Scenario,Unit,Year,REGION,Variable,value))

# Socioeconomics
SSP.Socio = subset(SSP.data, VARID=="Agri"|VARID=="GDPM"|VARID=="Popu"|VARID=="Pric")
SSP.Socio = subset(SSP.Socio, select=c(Scenario,Unit,Year,REGION,Variable,value))

# Yields
YieldAgr.Ligno = subset(YieldAgrAll, CROP=="NWOOD"&(Year=="2010"|Year=="2050"|Year=="2100"))
YieldMarg.sum =  subset(YieldMargAll, SCENARIO=="SSP1"&(Year=="2010"|Year=="2050"|Year=="2100"))

# Biomass Supply
BioSupplyPot = aggregate(BioSupply.Total$CumPot, by=list(BioSupply.Total$Year, BioSupply.Total$SCENARIO), FUN=max, na.rm=TRUE)
colnames(BioSupplyPot)[1:3] <- c("Year","SCENARIO","Potential")
BioSupplyPot = subset(BioSupplyPot, Year=="2010"|Year=="2050"|Year=="2100")

BioSupplyTotAll$Unit <- "GJ"

# Biomass production
SSP.BioProdFrac = subset(SSP.BioP, Variable=="PrimBioEneCrop"|Variable=="PrimBioModern"|Variable=="PrimBioNatCat6")
SSP.BioProdFrac = spread(SSP.BioProdFrac, REGION, value)
SSP.BioProdFrac$World <- NULL
SSP.BioProdFrac$VarOrder <- NULL
# colnames(SSP.BioProdFrac)[7] <- "Variable"
SSP.BioProdFrac = SSP.BioProdFrac %>% mutate (World = OECD90+REF+ASIA+MAF+LAM)
SSP.BioProdFrac = SSP.BioProdFrac %>% mutate (OECD90Frac = OECD90/World)
SSP.BioProdFrac = SSP.BioProdFrac %>% mutate (REFFrac = REF/World)
SSP.BioProdFrac = SSP.BioProdFrac %>% mutate (ASIAFrac = ASIA/World)
SSP.BioProdFrac = SSP.BioProdFrac %>% mutate (MAFFrac = MAF/World)
SSP.BioProdFrac = SSP.BioProdFrac %>% mutate (LAMFrac = LAM/World)
SSP.BioProdFrac = melt(SSP.BioProdFrac, id.vars=c("Scenario","Unit","Year","VARID","VARID2","ScenOrder","Variable"))
colnames(SSP.BioProdFrac)[8] <- "REGION"
SSP.BioProdFrac = subset(SSP.BioProdFrac, select=c(Scenario,Unit,Year,REGION,Variable,value))

# Emissions
SSP.Emis1 = subset(SSP.Emis, select=c(Scenario,Unit,Year,REGION,Variable,value)) 

# Aggregate Yields (calculated)
Yields.R = BioSupplyLand
Yields.R$ECPotential = BioSupplyEC[match(Yields.R$ID,BioSupplyEC$ID),"ECPotential"]
Yields.R$ID <- NULL
Yields.R = Yields.R %>% mutate(AgrYield = ECPotential / LandPotential)  # GJ/Ha
Yields.R$ScenOrder = factor(Yields.R$SCENARIO, levels=c("SSP1","SSP1_450","SSP1_20","SSP2","SSP2_450","SSP2_20","SSP3","SSP3_550"))

#
# ---- OUTPUT (xlsx)----
# write.xlsx(SSP.Land1, file="output/SSP_Results.xlsx", sheetName="Land Cover", row.names=TRUE, showNA = TRUE)
# write.xlsx(SSP.Prim1, file="output/SSP_Results.xlsx", sheetName="Primary Energy", append=TRUE, row.names=FALSE, showNA = TRUE)
# write.xlsx(SSP.BioS1, file="output/SSP_Results.xlsx", sheetName="Secondary Bioenergy", append=TRUE, row.names=FALSE, showNA = TRUE)
# write.xlsx(SSP.FinE, file="output/SSP_Results.xlsx", sheetName="Final Energy", append=TRUE, row.names=FALSE, showNA = TRUE)
# write.xlsx(BioSupplyTotAll, file="output/SSP_Results.xlsx", sheetName="Biomass Potential", append=TRUE, row.names=FALSE, showNA = TRUE)
# write.xlsx(SSP.BioProdFrac, file="output/SSP_Results.xlsx", sheetName="Production Regions", append=TRUE, row.names=FALSE, showNA = TRUE)
# write.xlsx(YieldAgr.Ligno, file="output/SSP_Results.xlsx", sheetName="Ligno. Yields", append=TRUE, row.names=FALSE, showNA = TRUE)
# write.xlsx(SSP.Emis1, file="output/SSP_Results.xlsx", sheetName="Emissions", append=TRUE, row.names=FALSE, showNA = TRUE)
# write.xlsx(SSP.Clim1, file="output/SSP_Results.xlsx", sheetName="Climate Effects", append=TRUE, row.names=FALSE, showNA = TRUE)
# write.xlsx(SSP.Socio, file="output/SSP_Results.xlsx", sheetName="Socio-Economics", append=TRUE, row.names=FALSE, showNA = TRUE)
#
rm(SSP.Land1,SSP.Prim1,SSP.BioS1,SSP.Emis1,SSP.Clim1)
#
# ---- LABELS ----
var_labels <- c("LandBuiltupArea"="Urban",
                "LandCropland"="Cropland (Food)",
                "LandCroplandEneCrop"="Cropland (Energy)",
                "LandForest"="Forest",
                "LandOtherNaturalLand"="Other Natural Land",
                "LandPasture"="Pasture",
                "CroplandFrac"="Cropland (Food)",
                "ForestFrac"="Forest",
                "OtherNatLandFrac"="Other Natural Land",
                "PastureFrac"="Pasture",
                "PotBioLandFrac"="Available for Biomass",
                "PrimBioEneCrop"="Energy Crops",
                "PrimBioModern"="Modern Biomass",
                "PrimBioNatCat1"="Woody",
                "PrimBioNatCat2"="Maize",
                "PrimBioNatCat3"="Sugarcane",
                "PrimBioNatCat4"="OilCrops",
                "PrimBioNatCat5"="Grassy",
                "PrimBioNatCat6"="Residues",
                "PrimBioResidues"="Residues",
                "PrimBioTraditional"="Traditional Biomass",
                "PrimCoal"="Coal",
                "PrimGas"="Gas",
                "PrimNuclear"="Nuclear",
                "PrimOil"="Oil",
                "PrimRen"="Renewables",
                "TempGlobalMean"="Temperature Change",
                "EmisCH4_Cor"="CH4",
                "EmisCO2FFIDem"="Energy Demand",
                "EmisCO2FFISup"="Energy Supply",
                "EmisCO2LandUse"="Land Use",
                "EmisN2O_Cor"="N2O")

scen_labels <-c("SSP1"="SSP1",
                "SSP2"="SSP2",
                "SSP3"="SSP3",
                "SSP1_20"="SSP1-1.9 W/m²",
                "SSP2_20"="SSP2-1.9 W/m²",
                "SSP1_450"="SSP1-2.6 W/m²",
                "SSP2_450"="SSP2-2.6 W/m²",
                "SSP3_550"="SSP3-3.4 W/m²")
reg_labels <- c("BRA" = "Brazil",
                "CAN" = "Canada",
                "CEU" = "Central Europe",
                "CHN" = "China",
                "EAF" = "E. Africa",
                "INDIA" = "India",
                "INDO" = "Indonesia",
                "JAP" = "Japan",
                "KOR" = "Korea",
                "ME" = "Middle East",
                "MEX" = "Mexico",
                "NAF" = "N. Africa",
                "OCE" = "Oceania",
                "RCAM" = "Rest C. America",
                "RSAF" = "Rest S. Africa",
                "RSAM" = "Rest S. America",
                "RSAS" = "Rest S. Asia",
                "RUS" = "Russia",
                "SAF" = "S. Africa",
                "SEAS" = "S.E. Asia",
                "STAN" = "Kazakhstan",
                "TUR" = "Turkey",
                "UKR" = "Ukraine",
                "USA" = "USA",
                "WAF" = "W. Africa",
                "WEU" = "W. Europe",
                "World" = "World",
                "EU" = "EU-28",
                "NL" = "The Netherlands",
                "OECD90"="OECD",
                "REF"="Reforming Economies",
                "ASIA"="Asia",
                "MAF"="M. East & Africa",
                "LAM"="Lat. America")
                
#
# ---- FIG: LAND BALANCE----
# FigLandFrac <- ggplot(data=subset(SSP.Land, (variable=="CroplandFrac"|variable=="ForestFrac"|variable=="OtherNatLandFrac"|variable=="PastureFrac"|variable=="PotBioLandFrac")&(Year=="2010"|Year=="2030"|Year=="2050"|Year=="2070"|Year=="2100")&(Scenario=="SSP1"|Scenario=="SSP2"|Scenario=="SSP3")&REGION=="World"),
                      # mapping=aes(x=Year, y=value, fill=VarOrder)) +
FigLandFrac <- ggplot(data=subset(SSP.Land, (variable=="CroplandFrac"|variable=="ForestFrac"|variable=="OtherNatLandFrac"|variable=="PastureFrac"|variable=="PotBioLandFrac")&(Year=="2010"|Year=="2030"|Year=="2050"|Year=="2070"|Year=="2100")&REGION=="World"),
                      mapping=aes(x=Year, y=value, fill=VarOrder)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab("Land Cover Fraction") +
  xlab("") +
  theme(legend.position="right", legend.text = element_text(size=6, face="plain"), legend.background = element_rect(fill=alpha('blue', 0))) +
  scale_fill_manual(values=c("lightcyan4","darkolivegreen1","forestgreen","bisque","darkblue"),
                    name="",
                    breaks=c("PotBioLandFrac","OtherNatLandFrac","ForestFrac","PastureFrac","CroplandFrac"),
                    labels=c("Potential Land for \nShort Rotation Crops","Other Natural Lands","Forest","Pasture","Cropland (food)")
  ) +
  facet_wrap(~ScenOrder, labeller=labeller(ScenOrder=scen_labels))
FigLandFrac

#
# ---- FIG: PRIMARY ENERGY----
FigPrim <- ggplot(data=subset(SSP.Prim, (Variable=="PrimBioModern"|Variable=="PrimBioTraditional"|Variable=="PrimCoal"|Variable=="PrimGas"|Variable=="PrimOil"|Variable=="PrimNuclear"|Variable=="PrimRen")&(Year=="2010"|Year=="2030"|Year=="2050"|Year=="2070"|Year=="2100")&REGION=="World"),mapping=aes(x=Year, y=value, fill=VarOrder)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(EJ[Prim],"/yr",""))) +
  xlab("") +
  theme(legend.position="right", legend.text = element_text(size=6, face="plain"), legend.background = element_rect(fill=alpha('blue', 0))) +
  scale_fill_manual(values=c("blueviolet","grey80","forestgreen","brown","plum2","dodgerblue","black"),
                    name="",
                    breaks=c("PrimNuclear","PrimRen","PrimBioModern","PrimBioTraditional","PrimGas","PrimOil","PrimCoal"),
                    labels=c("Nuclear","Other Renewables","Modern Biomass","Traditional Biomass","Natural Gas","Oil","Coal")) +
  facet_wrap(~ScenOrder, labeller=labeller(ScenOrder=scen_labels))
FigPrim

#
# ---- FIG: EMISSIONS----
FigEmis <- ggplot(data=subset(SSP.Emis, REGION=="World"&(Year=="2010"|Year=="2030"|Year=="2050"|Year=="2070"|Year=="2100")),mapping=aes(x=Year, y=value/1000, fill=VarOrder)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(GtCO[2],"-eq/yr",""))) +
  xlab("") +
  theme(legend.text = element_text(size=6, face="plain"),legend.text.align = 0) +
  scale_fill_manual(values=c("mediumpurple","lightgoldenrod","forestgreen","lightskyblue","saddlebrown"),
                    name="",
                    breaks=c("EmisN2O_cor","EmisCH4_cor","EmisCO2LandUse","EmisCO2FFIDem","EmisCO2FFISup"),
                    labels=c(expression(paste(N[2],"O","")),expression(CH[4]),expression(paste(CO[2]," ","(Land Use Change)","")),expression(paste(CO[2]," ","(Energy Demand)","")),expression(paste(CO[2]," ","(Energy Supply)","")))) +
  facet_wrap(~ScenOrder, labeller=labeller(ScenOrder=scen_labels))
FigEmis

#
# ---- FIG: PRIM BIO REGION ----

FigBioRegion <- ggplot(data=subset(SSP.BioP, !(REGION=="World")&(Year=="2010"|Year=="2030"|Year=="2050"|Year=="2070"|Year=="2100")&Variable=="PrimBioModern"),mapping=aes(x=Year, y=value, fill=REGION)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(EJ[Prim],"/yr",""))) +
  xlab("") +
  theme(legend.position="right", legend.text = element_text(size=6, face="plain"), legend.background = element_rect(fill=alpha('blue', 0))) +
  scale_fill_manual(values=c("navy","red","pink","brown","forestgreen"),
                    name="",
                    breaks=c("OECD90","REF","ASIA","MAF","LAM"),
                    labels=c("OECD","Reforming Economies","Asia","M. East & Africa","Lat. America")) +
  facet_wrap(~ScenOrder, labeller=labeller(ScenOrder=scen_labels))
FigBioRegion

FigBioRegionEC <- ggplot(data=subset(SSP.BioProd, !(REGION=="World")&(Year=="2010"|Year=="2030"|Year=="2050"|Year=="2070"|Year=="2100")&variable=="PrimBioEneCrop"),mapping=aes(x=Year, y=value, fill=REGION)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(EJ[Prim],"/yr",""))) +
  xlab("") +
  theme(legend.position="right", legend.text = element_text(size=6, face="plain"), legend.background = element_rect(fill=alpha('blue', 0))) +
  scale_fill_manual(values=c("navy","red","pink","brown","forestgreen"),
                    name="",
                    breaks=c("OECD90","REF","ASIA","MAF","LAM"),
                    labels=c("OECD","Reforming Economies","Asia","M. East & Africa","Lat. America")) +
  facet_wrap(~ScenOrder, labeller=labeller(ScenOrder=scen_labels))
FigBioRegionEC

FigBioRegionRes <- ggplot(data=subset(SSP.BioProd, !(REGION=="World")&(Year=="2010"|Year=="2030"|Year=="2050"|Year=="2070"|Year=="2100")&variable=="PrimBioResidues"),mapping=aes(x=Year, y=value, fill=REGION)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(EJ[Prim],"/yr",""))) +
  xlab("") +
  theme(legend.position="right", legend.text = element_text(size=6, face="plain"), legend.background = element_rect(fill=alpha('blue', 0))) +
  scale_fill_manual(values=c("navy","red","pink","brown","forestgreen"),
                    name="",
                    breaks=c("OECD90","REF","ASIA","MAF","LAM"),
                    labels=c("OECD","Reforming Economies","Asia","M. East & Africa","Lat. America")) +
  facet_wrap(~ScenOrder, labeller=labeller(ScenOrder=scen_labels))
FigBioRegionRes

#
# ---- FIG: PRIM BIO FEED ----
FigBioFeed <- ggplot(data=subset(SSP.BioP, REGION=="World"&(Year=="2010"|Year=="2030"|Year=="2050"|Year=="2070"|Year=="2100")&(Variable=="PrimBioNatCat1"|Variable=="PrimBioNatCat2"|Variable=="PrimBioNatCat3"|Variable=="PrimBioNatCat5"|Variable=="PrimBioNatCat6")),mapping=aes(x=Year, y=value, fill=Variable)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(EJ[Prim],"/yr",""))) +
  xlab("") +
  theme(legend.position="right", legend.text = element_text(size=6, face="plain"), legend.background = element_rect(fill=alpha('blue', 0))) +
  scale_fill_manual(values=c("saddlebrown","darkgoldenrod1","darkolivegreen1","darkgreen","cornflowerblue"),
                    name="",
                    breaks=c("PrimBioNatCat1","PrimBioNatCat2","PrimBioNatCat3","PrimBioNatCat5","PrimBioNatCat6"),
                    labels=c("Woody","Maize","Sugarcane","Grassy","Residues")) +
  facet_wrap(~ScenOrder, labeller=labeller(ScenOrder=scen_labels))
FigBioFeed

#
# ---- FIG: SEC BIO  ----
FigBioSec <- ggplot(data=subset(SSP.BioS, !(Variable=="SecENonEnTotal")&REGION=="World"&(Year=="2010"|Year=="2030"|Year=="2050"|Year=="2070"|Year=="2100")&value>0),mapping=aes(x=Year, y=value, fill=VarOrder)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(EJ[Sec],"/yr",""))) +
  xlab("") +
  theme(legend.position="right", legend.text = element_text(size=6, face="plain"), legend.background = element_rect(fill=alpha('blue', 0))) +
  scale_fill_manual(values=c("mediumpurple","grey50","grey80","darkorange4","darkorange","navy","maroon"),
                    name="",
                    breaks=c("SecENonEnBio","SecEElectricityBiowoCCS","SecEElectricityBiowCCS","SecEHydrogenBiowoCCS","SecEHydrogenBiowCCS","SecELiquidsBio","SecESolidsBio"),
                    labels=c("Chemicals","Electricity","Electricity w. CCS","Hydrogen","Hydrogen w. CCS","Liquids","Solids")) +
  facet_wrap(~ScenOrder, labeller=labeller(ScenOrder=scen_labels))
FigBioSec
#
# ---- FIG: LUC vs. BIO ----
SSP.LUCBio$Unit <- NULL
SSP.LUCBio$VARID <- NULL

SSP.LUCBio = spread(SSP.LUCBio, Variable, value)
FigLUCBio<-ggplot(data=subset(SSP.LUCBio, Year>2010&REGION=="World")) + 
  geom_line(aes(x=Year, y=PrimBioEneCrop*0.032, colour="PrimBioEneCrop"), size=0.5) +
  geom_line(aes(x=Year, y=EmisCO2LandUse/1000, colour="EmisCO2LandUse"), size=0.5) +
  scale_y_continuous(sec.axis = sec_axis(~./0.032, name = expression(paste(EJ[Prim],"/yr")))) +
  xlim(2010,2100) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=90, size=6, hjust=0.5), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste(CO[2],"-eq/yr",""))) +
  xlab("") +
  theme(legend.position=c(0.9,0.13), legend.text = element_text(size=6, face="plain"), legend.background = element_rect(fill=alpha('blue', 0))) +
  scale_colour_manual(values=c("grey65", "black"),
                      name ="",
                      breaks=c("EmisCO2LandUse","PrimBioEneCrop"),
                      labels=c("Land Use Change Emissions","Biomass (Energy Crops)")) +
  facet_wrap(~ScenOrder, labeller=labeller(ScenOrder = scen_labels))
FigLUCBio
#
# ---- FIG: BIOMASS SUPPLY CURVES ----
AnnotationIdent = max(subset(BioSupply.Order, Year=="2100"&SCENARIO=="SSP1"&LandCat=="Abandoned")$CumPotCor)
BioSupply.Order$Annotation <- ""
BioSupply.Order$Annotation[BioSupply.Order$Year=="2100"&BioSupply.Order$SCENARIO=="SSP1"&BioSupply.Order$LandCat=="Abandoned"] <- ">200"

FigBioSup<-ggplot(data=subset(BioSupply.Order, Year=="2100"|Year=="2050")) + 
  geom_line(aes(x=CumPotCor, y=Cost, colour=LandCat), size=0.3) +
  xlim(0,100) + 
  ylim(0,15) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("A: Primary Biomass Supply Curves for Different Biomass Resources and Baselines") + theme(plot.title = element_text(face="bold")) +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste("$"[2005],"/GJ",""))) +
  xlab(expression(paste(EJ[prim],"/Yr"))) +
  geom_text(aes(x=85,y=7,label=Annotation), size=2) +
  theme(legend.position="right", legend.text = element_text(size=6, face="plain"), legend.background = element_rect(fill=alpha('blue', 0))) +
  scale_colour_manual(values=c("grey50", "forestgreen","firebrick"),
                       name ="",
                       breaks=c("Abandoned","Residues","Rest"),
                       labels=c("Abandoned Lands","Residues","Other Natural Lands")) +
  facet_grid(Year~SCENARIO, scales="free_x", labeller=labeller(SCENARIO = scen_labels))
FigBioSup

FigBioSup2<-ggplot(data=subset(BioSupply.Total, Year=="2100"|Year=="2050")) + 
  geom_line(aes(x=CumPot, y=Cost, colour=SCENARIO), size=0.3) +
  xlim(0,300) + 
  ylim(0,15) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("B. Total Primary Biomass Supply Curves Across Baselines") + theme(plot.title = element_text(face="bold")) +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste("$"[2005],"/GJ",""))) +
  xlab(expression(paste(EJ[prim],"/Yr"))) +
  theme(legend.position="right", legend.text = element_text(size=6, face="plain"), legend.background = element_rect(fill=alpha('blue', 0))) +
  scale_colour_manual(values=c("forestgreen", "navy","firebrick"),
                      name ="",
                      breaks=c("SSP1","SSP2","SSP3"),
                      labels=c("SSP1","SSP2","SSP3")) +
  facet_grid(Year~., scales="free_x")
FigBioSup2

layout<-rbind(c(1,1,1,1),c(2,2,2,3)) 
FigBioSupFinal <- grid.arrange(FigBioSup,FigBioSup2, layout_matrix=layout, newpage=TRUE)




#
# ---- FIG: YIELDS ----
# Based on Potential Land / Potential EnergyCrops
# If I use Actual Land Use / Actual energy Crops, the curves look very strange

FigYields<-ggplot(data=subset(Yields.R, (Year=="2010"|Year=="2030"|Year=="2050"|Year=="2070"|Year=="2100")&!(Region=="World"))) +
  geom_line(aes(x=Year, y=AgrYield, colour=Region), size=0.5) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ylim(0,500) +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=66, size=6, hjust=1), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste("GJ"[Prim],"/Ha",""))) +
  theme(legend.position="right", legend.text = element_text(size=6, face="plain")) +
  scale_colour_manual(values=c("navy","red","pink","brown","forestgreen","black"),
                      name ="",
                      breaks=c("OECD90","REF","ASIA","MAF","LAM","World"),
                      labels=c("OECD","Reforming Economies","Asia","M. East & Africa","Lat. America", "World")) +
  facet_wrap(~ScenOrder, labeller=labeller(ScenOrder=scen_labels))
FigYields


#
# ---- FIG: CLIMATE ----
SSP.Clim2 = SSP.Clim
SSP.Clim2$ScenID2 = substr(SSP.Clim2$Scenario, start = 6, stop = 10)
SSP.Clim2$ScenID2[SSP.Clim2$Scenario=="SSP1"] <- "Baseline"
SSP.Clim2$ScenID2[SSP.Clim2$Scenario=="SSP2"] <- "Baseline"
SSP.Clim2$ScenID2[SSP.Clim2$Scenario=="SSP3"] <- "Baseline"
SSP.Clim2$ScenID = substr(SSP.Clim2$Scenario, start = 1, stop = 4)

FigForcing<-ggplot(data=subset(SSP.Clim2, Year>2010&REGION=="World"&Variable=="Forcing")) + 
  geom_line(aes(x=Year, y=value, colour=ScenOrder), size=0.5) +
  geom_point(aes(x=Year, y=value, shape=ScenID2)) +
  xlim(2010,2100) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("A: Radiative Forcing") + theme(plot.title = element_text(face="bold")) +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=90, size=6, hjust=0.5), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression("W/m"^2)) +
  xlab("") +
  theme(legend.position="none", legend.text = element_text(size=6, face="plain")) +
  scale_colour_manual(values=c("forestgreen","forestgreen","forestgreen","navy","navy","navy","firebrick","firebrick","firebrick"),
                      name ="",
                      breaks=c("SSP1","SSP1_20","SSP1_450","SSP2","SSP2_20","SSP2_450","SSP3","SSP3_450"),
                      labels=c("SSP1","","","SSP2","","","SSP3",""), guide=FALSE) +
  scale_shape_manual(values=c(1,2,3,4),
                     name ="Climate Target",
                     breaks=c("20","450","550","Baseline"),
                     labels=c("1.5C (RCP1.9)","2C (RCP2.6)","2C (RCP3.4)","Baseline")) +
  facet_grid(.~ScenID, scales="free_x")
FigForcing

FigTemp<-ggplot(data=subset(SSP.Clim2, Year>2010&REGION=="World"&Variable=="TempGlobalMean")) + 
  geom_line(aes(x=Year, y=value, colour=ScenOrder), size=0.5) +
  geom_point(aes(x=Year, y=value, shape=ScenID2)) +
  xlim(2010,2100) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("B: Global Mean Temperature Change") + theme(plot.title = element_text(face="bold")) +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=90, size=6, hjust=0.5), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(~degree~C)) +
  xlab("") +
  theme(legend.position="none", legend.text = element_text(size=6, face="plain")) +
  scale_colour_manual(values=c("forestgreen","forestgreen","forestgreen","navy","navy","navy","firebrick","firebrick","firebrick"),
                      name ="",
                      breaks=c("SSP1","SSP1_20","SSP1_450","SSP2","SSP2_20","SSP2_450","SSP3","SSP3_450"),
                      labels=c("SSP1","","","SSP2","","","SSP3",""), guide=FALSE) +
  scale_shape_manual(values=c(1,2,3,4),
                     name ="Climate Target",
                     breaks=c("20","450","550","Baseline"),
                     labels=c("1.5C (RCP1.9)","2C (RCP2.6)","2C (RCP3.4)","Baseline")) +
  facet_grid(.~ScenID, scales="free_x")
FigTemp

FigCtax<-ggplot(data=subset(SSP.Clim2, Year>2010&REGION=="World"&Variable=="PriceCarbon")) + 
  geom_line(aes(x=Year, y=value, colour=ScenOrder), size=0.5) +
  geom_point(aes(x=Year, y=value, shape=ScenID2)) +
  xlim(2010,2100) +
  geom_hline(yintercept=0,size = 0.1, colour='black') +
  ggtitle("C: Carbon Tax") + theme(plot.title = element_text(face="bold")) +
  theme_bw() +
  theme(text= element_text(size=6, face="plain"), axis.text.x = element_text(angle=90, size=6, hjust=0.5), axis.text.y = element_text(size=6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.2)) +
  ylab(expression(paste("$/",tCO[2],""))) +
  xlab("") +
  theme(legend.position="bottom", legend.text = element_text(size=6, face="plain")) +
  scale_colour_manual(values=c("forestgreen","forestgreen","forestgreen","navy","navy","navy","firebrick","firebrick","firebrick"),
                      name ="",
                      breaks=c("SSP1","SSP1_20","SSP1_450","SSP2","SSP2_20","SSP2_450","SSP3","SSP3_450"),
                      labels=c("SSP1","","","SSP2","","","SSP3",""), guide=FALSE) +
  scale_shape_manual(values=c(1,2,3,4),
                     name ="Climate Target",
                     breaks=c("20","450","550","Baseline"),
                     labels=c("1.5C (RCP1.9)","2C (RCP2.6)","2C (RCP3.4)","Baseline")) +
  facet_grid(.~ScenID, scales="free_x")
FigCtax

layout<-rbind(1,1,1,1,1,
              2,2,2,2,2,
              3,3,3,3,3,3) 
FigClim <- grid.arrange(FigForcing,FigTemp,FigCtax,layout_matrix=layout)


#
# ---- OUTPUT: FIGURES FOR DRAFT ----
# FIGURE 1 ONLY BASELINES
# png("output/Rev1/Fig1_old.png", width=6*ppi, height=2*ppi, res=ppi)
# print(plot(FigLandFrac))
# dev.off()
# FIGURE 1 ALL SCENARIOS
# png("output/Rev1/Fig1.png", width=6*ppi, height=4*ppi, res=ppi)
# print(plot(FigLandFrac))
# dev.off()
# #
# png("output/Rev1/Fig2.png", width=6*ppi, height=4*ppi, res=ppi)
# print(plot(FigPrim))
# dev.off()
# 
# png("output/Rev1/Fig3.png", width=6*ppi, height=4*ppi, res=ppi)
# print(plot(FigEmis))
# dev.off()
# 
# png("output/Rev1/Fig4.png", width=6*ppi, height=4*ppi, res=ppi)
# print(plot(FigBioSec))
# dev.off()
# #
# png("output/Rev1/Fig6.png", width=6*ppi, height=5*ppi, res=ppi, bg="white")
# print(plot(FigBioSupFinal))
# dev.off()
# #
# png("output/Rev1/Fig7.png", width=6*ppi, height=4*ppi, res=ppi)
# print(plot(FigBioFeed))
# dev.off()
# 
# png("output/Rev1/Fig8.png", width=6*ppi, height=4*ppi, res=ppi)
# print(plot(FigBioRegion))
# dev.off()
# #
# png("output/Rev1/Fig9.png", width=6*ppi, height=4*ppi, res=ppi)
# print(plot(FigLUCBio))
# dev.off()
# 
# png("output/Rev1/FigA1.png", width=6*ppi, height=7.5*ppi, res=ppi)
# print(plot(FigClim))
# dev.off()
# 
# png("output/Rev1/FigA2.png", width=6*ppi, height=4*ppi, res=ppi)
# print(plot(FigYields))
# dev.off()