# Calculating residuals between vegetation and rainfall time-series datasets and k-means clusters.
# Ellie Linden (ellielinden10@gmail.com)
# January 2022
 
library(tidyverse)
library(readxl)
library(foreign)
library(NbClust)

###########################################
### Set Input Workspace and Import Data ###
###########################################
inWS = "E:/K2C_LandscapeChange_Analysis_v2/Manuscript/Github/Data/"  

### Import Vegetation and Rainfall Data ###
K2C.85.93.TIMESAT.Output <- read.table(str_c(inWS, "Vegetation/TIMESAT_85to93.txt"), header = TRUE, fill = TRUE)
K2C.95.06.TIMESAT.Output <- read.table(str_c(inWS, "Vegetation/TIMESAT_95to06.txt"), header = TRUE, fill = TRUE)
years <- read.csv(str_c(inWS, "Rainfall/Rainfall_TIMESAT_Years.csv"), header = TRUE)
Rainfall <- read.dbf(str_c(inWS, "Rainfall/Rainfall.dbf"))

### Import Buffered Reserve Points ###
setwd(str_c(inWS, "ReserveLocations"))
row_column_files = list.files(pattern = "*points")
for (i in 1:length(row_column_files)) assign(row_column_files[i], read_excel(row_column_files[i]))

# ------------------------------------------------------------------------ #
# --------- Calculating residuals between vegetation and rainfall -------- #
# ------------------------------------------------------------------------ #

##########################
### Clean TIMESAT Data ###
##########################

### Create New Season Column ###
as.integer(K2C.85.93.TIMESAT.Output$Seas2 <- paste(K2C.85.93.TIMESAT.Output$Seas.))
as.integer(K2C.95.06.TIMESAT.Output$Seas2 <- paste(K2C.95.06.TIMESAT.Output$Seas.+10))

### Replacing 0's with NA in Dataframes ###
K2C.85.93.TIMESAT.Output[K2C.85.93.TIMESAT.Output == 0] <- NA
K2C.95.06.TIMESAT.Output[K2C.95.06.TIMESAT.Output == 0] <- NA

### Combining Tables ###
K2C.85.93.TIMESAT.Output$Seas2 <- as.factor(K2C.85.93.TIMESAT.Output$Seas2)
K2C.95.06.TIMESAT.Output$Seas2 <- as.factor(K2C.95.06.TIMESAT.Output$Seas2)
K2C <- na.omit(rbind(K2C.85.93.TIMESAT.Output, K2C.95.06.TIMESAT.Output, deparse.level = 1))

##########################################################################
### Create Unique ID Fields for Row/Column Combination in Reserve Data ###
##########################################################################
K2C$ID <- paste(K2C$Row, K2C$Col., sep=" ")
Balule_points.xls$ID <- paste(Balule_points.xls$X, Balule_points.xls$Y, sep = " ")
Blue_Canyon_points.xls$ID <- paste(Blue_Canyon_points.xls$X, Blue_Canyon_points.xls$Y, sep = " ")
Blyde_Olifants_Conservancy_points.xls$ID <- paste(Blyde_Olifants_Conservancy_points.xls$X, Blyde_Olifants_Conservancy_points.xls$Y, sep = " ")
Kapama_points.xls$ID <- paste(Kapama_points.xls$X, Kapama_points.xls$Y, sep = " ")
KempianaComplex_points.xls$ID <- paste(KempianaComplex_points.xls$X, KempianaComplex_points.xls$Y, sep = " ")
Klaserie_points.xls$ID <- paste(Klaserie_points.xls$X, Klaserie_points.xls$Y, sep = " ")
KNP_Central_clip_geology_points.xls$ID <- paste(KNP_Central_clip_geology_points.xls$X, KNP_Central_clip_geology_points.xls$Y, sep = " ")
Makalali_points.xls$ID <- paste(Makalali_points.xls$X, Makalali_points.xls$Y, sep = " ")
Mala_Mala_points.xls$ID <- paste(Mala_Mala_points.xls$X, Mala_Mala_points.xls$Y, sep = " ")
Manyeleti_points.xls$ID <- paste(Manyeleti_points.xls$X, Manyeleti_points.xls$Y, sep = " ")
Sabi_Sand_points.xls$ID <- paste(Sabi_Sand_points.xls$X, Sabi_Sand_points.xls$Y, sep = " ")
Selati_points.xls$ID <- paste(Selati_points.xls$X, Selati_points.xls$Y, sep = " ")
Thornybush_points.xls$ID <- paste(Thornybush_points.xls$X, Thornybush_points.xls$Y, sep = " ")
Timbavati_points.xls$ID <- paste(Timbavati_points.xls$X, Timbavati_points.xls$Y, sep = " ")
Umbabat_points.xls$ID <- paste(Umbabat_points.xls$X, Umbabat_points.xls$Y, sep = " ")

#####################################################
### Add Reserve/Rainfall Columns to K2C Dataframe ###
#####################################################

### Specify Reserve ###
K2C <- K2C %>% 
  mutate(Reserve = case_when(ID %in% Balule_points.xls$ID ~ "Balule", 
                             ID %in% Blue_Canyon_points.xls$ID ~ "BlueCanyon",
                             ID %in% Blyde_Olifants_Conservancy_points.xls$ID ~ "BlydeOlifantsConservancy",
                             ID %in% Klaserie_points.xls$ID ~ "Klaserie",
                             ID %in% Kapama_points.xls$ID ~ "Kapama",
                             ID %in% KempianaComplex_points.xls$ID ~ "Kempiana",
                             ID %in% KNP_Central_clip_geology_points.xls$ID ~ "KNPCentral",
                             ID %in% Makalali_points.xls$ID ~ "Makalali",
                             ID %in% Mala_Mala_points.xls$ID ~ "MalaMala",
                             ID %in% Manyeleti_points.xls$ID ~ "Manyeleti",
                             ID %in% Sabi_Sand_points.xls$ID ~ "SabiSand",
                             ID %in% Selati_points.xls$ID ~ "Selati",
                             ID %in% Thornybush_points.xls$ID ~ "Thornybush",
                             ID %in% Timbavati_points.xls$ID ~ "Timbavati",
                             ID %in% Umbabat_points.xls$ID ~ "Umbabat"))

### Filter Reserve used in Analysis (those covering at least 100 pixels) ###
K2C.filter <- K2C %>% 
  filter(Reserve %in% c("Balule", "BlueCanyon", "BlydeOlifantsConservancy", "Klaserie", 
                        "Kapama", "Kempiana", "KNPCentral", "Makalali", "MalaMala", "Manyeleti", 
                        "SabiSand", "Selati", "Thornybush", "Timbavati", "Umbabat"))

### Create Lists for Reserves that Remained Closed and Reserves that Removed Fences ###
Closed.Reserve.List <- c("Balule", "BlueCanyon", "BlydeOlifantsConservancy", "Kapama", 
                         "Makalali", "Selati", "Thornybush")
Open.Reserve.List <- c("Kempiana", "Klaserie", "MalaMala", "Manyeleti", 
                       "SabiSand", "Timbavati", "Umbabat")

### Create Lists for early/late years ###
early <- c("1", "2", "3", "4","5", "6", "7")
transition <- c("8","11", "12")
late <- c("13","14", "15", "16", "17", "18", "19", "20", "21")

### Create new columns that specifies for each individual year whether the reserve remained closed, removed its fence, or is KNP and specifies time period categroy ###
years$TIMESAT_year <- as.factor(years$TIMESAT_year)

K2C.filter <- K2C.filter %>%
  mutate(FenceAction = case_when(Reserve %in% Closed.Reserve.List ~ "RemainedClosed", 
                                 Reserve %in% Open.Reserve.List ~ "RemovedFence", 
                                 Reserve == "KNPCentral" ~ "KNP"), 
         Time = case_when(Seas2 %in% early ~ "Early", 
                          Seas2 %in% transition ~ "Transition", 
                          Seas2 %in% late ~ "Late")) %>% 
  left_join(., years, by=c("Seas2"="TIMESAT_year"))

### Convert variables to factors ###
K2C.filter$FenceAction <- as.factor(K2C.filter$FenceAction)
K2C.filter$Seas2 <- as.factor(K2C.filter$Seas2)
K2C.filter$Time <- as.factor(K2C.filter$Time)

### Remove last year in dataset, since TIMESAT didn't have a full last growing season ###
K2C.filter <- K2C.filter %>%
  filter(Seas2 != 21)

##################################
### Import/Clean Rainfall Data ###
##################################
Rainfall$ID <- paste(Rainfall$X, Rainfall$Y, sep=" ")

### Gather rainfall dataframe ###
Rainfall <- Rainfall %>%
  select(-pointid, -X, -Y) %>%
  gather(year, value, -ID) %>% 
  mutate(year = str_replace(year, "F", ""))

##################################################
### Join Rainfall/TIMESAT by pixel ID and Year ###
##################################################
Rainfall$year <- as.integer(Rainfall$year)
Rainfall <- Rainfall %>% 
  rename(Rainfall = value,
         Rainfall_year = year)

### Join Rainfall field to K2C dataframe and rename "Rainfall_year" field ###
K2C.filter <- K2C.filter %>% 
  left_join(Rainfall, by=c("ID","Rainfall_year")) %>% 
  rename(Year = Rainfall_year)

### Transform Rainfall using Log ###
K2C.filter <- K2C.filter %>%
  mutate(Rainfall_log = log(Rainfall))

##########################################################################################
### Calculate TIMESAT/Rainfall Residuals and Summarize grouped by FenceAction by Year  ###
##########################################################################################

### Create Variables of TIMESAT/Rainfall Residuals for all variables that have a linear relationship with Rainfall ###
K2C.filter <- K2C.filter %>% 
  mutate(Max_Res = resid(lm(Max.val. ~ Rainfall_log)),
         Amp_Res = resid(lm(Ampl. ~ Rainfall_log)))

### Calculate Means/Variances for the Max/Amp that will be used in cluster analysis ###
K2C.summary <- K2C.filter %>% 
  group_by(FenceAction, Year) %>% 
  mutate(MaxRes_Mean = mean(Max_Res),
         AmpRes_Mean = mean(Amp_Res),
         MaxRes_Var = var(Max_Res),
         AmpRes_Var = var(Amp_Res)) %>% 
  ungroup() %>% 
  select(Year, FenceAction, Time, MaxRes_Mean, AmpRes_Mean, MaxRes_Var, AmpRes_Var) %>% 
  distinct()

# --------------------------------------------------------- #
# ------------------ Calculating Clusters ----------------- #
# --------------------------------------------------------- #

###########
### Max ###
###########

# --------------- #
# - Max: Closed - #
# --------------- #

### Filter out Closed Reserves ### 
Max.Closed <- K2C.summary %>%
  filter(FenceAction == "RemainedClosed") %>%
  select(MaxRes_Mean, MaxRes_Var, Year, FenceAction, Time)

## Determine Optimal Number of Clusters (NbClust) and use output to Create Kmeans Object #
Max.Closed.nbclust <- NbClust(data = Max.Closed[1:2], distance = "euclidean", method = "kmeans", index = "all", max.nc = 5)
Max.Closed.kmeans <- kmeans(Max.Closed[1:2], centers = 4, nstart = 25)

# Add "Cluster" as a column
Max.Closed$Cluster <- as.factor(Max.Closed.kmeans$cluster)

# ------------- #
# - Max: Open - #
# ------------- #

### Filter out Open Reserves and Max Mean/Var Columns ###
Max.Open <- K2C.summary %>%
  filter(FenceAction == "RemovedFence") %>%
  select(MaxRes_Mean, MaxRes_Var, Year, FenceAction, Time)

## Determine Optimal Number of Clusters (NbClust) and use output to Create Kmeans Object ###
Max.Open.nbclust <- NbClust(data = Max.Open[1:2], distance = "euclidean", method = "kmeans", index = "all", max.nc = 5)
Max.Open.kmeans <- kmeans(Max.Open[1:2], centers = 5, nstart = 25)

# Add "Cluster" as a column
Max.Open$Cluster <- as.factor(Max.Open.kmeans$cluster)

# ------------ #
# - Max: KNP - #
# ------------ #

### Filter out KNP Reserves and Max Mean/Var Columns ###
Max.KNP <- K2C.summary %>%
  filter(FenceAction == "KNP") %>%
  select(MaxRes_Mean, MaxRes_Var, Year, FenceAction, Time)

## Determine Optimal Number of Clusters (NbClust) and use output to Create Kmeans Object ###
Max.KNP.nbclust <- NbClust(data = Max.KNP[1:2], distance = "euclidean", method = "kmeans", index = "all", max.nc = 5)
Max.KNP.kmeans <- kmeans(Max.KNP[1:2], centers = 5, nstart = 25)

# Add "Cluster" as a column
Max.KNP$Cluster <- as.factor(Max.KNP.kmeans$cluster)

###########
### Amp ###
###########

# --------------- #
# - Amp: Closed - #
# --------------- #

### Filter out Closed Reserves ### 
Amp.Closed <- K2C.summary %>%
  filter(FenceAction == "RemainedClosed") %>%
  select(AmpRes_Mean, AmpRes_Var, Year, FenceAction, Time)

## Determine Optimal Number of Clusters (NbClust) and use output to Create Kmeans Object #
Amp.Closed.nbclust <- NbClust(data = Amp.Closed[1:2], distance = "euclidean", method = "kmeans", index = "all", max.nc = 5)
Amp.Closed.kmeans <- kmeans(Amp.Closed[1:2], centers = 5, nstart = 25)

# Add "Cluster" as a column
Amp.Closed$Cluster <- as.factor(Amp.Closed.kmeans$cluster)

# ------------- #
# - Amp: Open - #
# ------------- #

### Filter out Open Reserves and Amp Mean/Var Columns ###
Amp.Open <- K2C.summary %>%
  filter(FenceAction == "RemovedFence") %>%
  select(AmpRes_Mean, AmpRes_Var, Year, FenceAction, Time)

## Determine Optimal Number of Clusters (NbClust) and use output to Create Kmeans Object ###
Amp.Open.nbclust <- NbClust(data = Amp.Open[1:2], distance = "euclidean", method = "kmeans", index = "all", max.nc = 5)
Amp.Open.kmeans <- kmeans(Amp.Open[1:2], centers = 5, nstart = 25)

# Add "Cluster" as a column
Amp.Open$Cluster <- as.factor(Amp.Open.kmeans$cluster)

# ------------ #
# - Amp: KNP - #
# ------------ #

### Filter out KNP Reserves and Amp Mean/Var Columns ###
Amp.KNP <- K2C.summary %>%
  filter(FenceAction == "KNP") %>%
  select(AmpRes_Mean, AmpRes_Var, Year, FenceAction, Time)

## Determine Optimal Number of Clusters (NbClust) and use output to Create Kmeans Object ###
Amp.KNP.nbclust <- NbClust(data = Amp.KNP[1:2], distance = "euclidean", method = "kmeans", index = "all", max.nc = 5)
Amp.KNP.kmeans <- kmeans(Amp.KNP[1:2], centers = 3, nstart = 25)

# Add "Cluster" as a column
Amp.KNP$Cluster <- as.factor(Amp.KNP.kmeans$cluster)

