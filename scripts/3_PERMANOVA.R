# Calculating distance metrics and running Permutational Multivariate Analysis of Variance (PERMANOVA).
# Ellie Linden (ellielinden10@gmail.com)
# March 2021

library(tidyverse)
library(vegan)
library(smacof)
library(RVAideMemoire) # pairwise.perm.manova

### Set output workspace ###
outWS.stats <- outWS <- "E:/K2C_LandscapeChange_Analysis_v2/Manuscript/Stats/"

### Re-order dataframe columns ###
K2C.summary <- K2C.summary %>% 
  select(Year, FenceAction, Time, 
         MaxRes_Mean, MaxRes_Var, 
         AmpRes_Mean, AmpRes_Var)

################################################################
### Run Adonis for Time Period/Fence Status Interaction Term ###
################################################################

# ------- #
# - Max - #
# ------- #
Max.vegdist.interaction <- vegdist(K2C.summary[4:5], method = 'euclidean')
Max.adonis.interaction <-adonis2(Max.vegdist.interaction ~ K2C.summary$Time*K2C.summary$FenceAction, data=K2C.summary, permutations = 999, method="euclidean")
write.table(Max.adonis.interaction, file = str_c(outWS.stats, "Max_adonis_interaction.csv"), sep = ",")

# ------- #
# - Amp - #
# ------- #
Amp.vegdist.interaction <- vegdist(K2C.summary[6:7], method = 'euclidean')
Amp.adonis.interaction <-adonis2(Amp.vegdist.interaction ~ K2C.summary$Time*K2C.summary$FenceAction, data=K2C.summary, permutations = 999, method="euclidean")
write.table(Amp.adonis.interaction, file = str_c(outWS.stats, "Amp_adonis_interaction.csv"), sep = ",")

#############################
### Pairwise Comparisions ###
#############################

# ----------- #
# --- Max --- #
# ----------- #

### Closed ###
Max.Closed.vegdist <- vegdist(Max.Closed[1:2], method = 'euclidean')
Max.Closed.adonis <-adonis2(Max.Closed.vegdist~Max.Closed$Time, data=Max.Closed, permutations = 999, method="euclidean")
Max.Closed.pairwise.manova <- pairwise.perm.manova(resp = Max.Closed.vegdist, Max.Closed$Time, p.method = "none", F = TRUE)

### Open ###
Max.Open.vegdist <- vegdist(Max.Open[1:2], method = 'euclidean')
Max.Open.adonis <-adonis2(Max.Open.vegdist ~ Max.Open$Time, data=Max.Open, permutations = 999, method="euclidean", strata="PLOT")
Max.Open.pairwise.manova <- pairwise.perm.manova(resp = Max.Open.vegdist, Max.Open$Time, p.method = "none", F = TRUE)

### KNP ###
Max.KNP.vegdist <- vegdist(Max.KNP[1:2], method = 'euclidean')
Max.KNP.adonis <-adonis2(Max.KNP.vegdist ~ Max.KNP$Time, data=Max.KNP, permutations = 999, method="euclidean", strata="PLOT")
Max.KNP.pairwise.manova <- pairwise.perm.manova(resp = Max.KNP.vegdist, Max.KNP$Time, p.method = "none", F = TRUE)

# ----------- #
# --- Amp --- #
# ----------- #

### Closed ###
Amp.Closed.vegdist <- vegdist(Amp.Closed[1:2], method = 'euclidean')
Amp.Closed.adonis <-adonis2(Amp.Closed.vegdist~Amp.Closed$Time, data=Amp.Closed, permutations = 999, method="euclidean", strata="PLOT")
Amp.Closed.pairwise.manova <- pairwise.perm.manova(resp = Amp.Closed.vegdist, Amp.Closed$Time, p.method = "none", F = TRUE)

### Open ###
Amp.Open.vegdist <- vegdist(Amp.Open[1:2], method = 'euclidean')
Amp.Open.adonis <-adonis2(Amp.Open.vegdist ~ Amp.Open$Time, data=Amp.Open, permutations = 999, method="euclidean", strata="PLOT")
Amp.Open.pairwise.manova <- pairwise.perm.manova(resp = Amp.Open.vegdist, Amp.Open$Time, p.method = "none", F = TRUE)

### KNP ###
Amp.KNP.vegdist <- vegdist(Amp.KNP[1:2], method = 'euclidean')
Amp.KNP.adonis <-adonis2(Amp.KNP.vegdist ~ Amp.KNP$Time, data=Amp.KNP, permutations = 999, method="euclidean", strata="PLOT")
Amp.KNP.pairwise.manova <- pairwise.perm.manova(resp = Amp.KNP.vegdist, Amp.KNP$Time, p.method = "none", F = TRUE)

##############################################
### Results: Not Pairwise (adonis2 output) ###
##############################################
write.table(Max.Closed.interaction, file = str_c(outWS.stats, "Max_adonis_interaction.csv"), sep = ",")
write.table(Max.Open.adonis, file = str_c(outWS.stats, "MaxOpen_adonis.csv"), sep = ",")
write.table(Max.KNP.adonis, file = str_c(outWS.stats, "MaxKNP_adonis.csv"), sep = ",")
write.table(Amp.Closed.adonis, file = str_c(outWS.stats, "AmpClosed_adonis.csv"), sep = ",")
write.table(Amp.Open.adonis, file = str_c(outWS.stats, "AmpOpen_adonis.csv"), sep = ",")
write.table(Amp.KNP.adonis, file = str_c(outWS.stats, "AmpKNP_adonis.csv"), sep = ",")

#########################
### Results: Pairwise ###
#########################
Max.Closed.results <- data.frame("Comparision" = c("Late vs. Transition", "Late vs. Early", "Transition vs. Early"), 
                                 "F"=c(Max.Closed.pairwise.manova$F.value[4], Max.Closed.pairwise.manova$F.value[1], Max.Closed.pairwise.manova$F.value[2]), 
                                 "p"=c(Max.Closed.pairwise.manova$p.value[4], Max.Closed.pairwise.manova$p.value[1], Max.Closed.pairwise.manova$p.value[2]))
Max.Open.results <- data.frame("Comparision" = c("Late vs. Transition", "Late vs. Early", "Transition vs. Early"), 
                               "F"=c(Max.Open.pairwise.manova$F.value[4], Max.Open.pairwise.manova$F.value[1], Max.Open.pairwise.manova$F.value[2]), 
                               "p"=c(Max.Open.pairwise.manova$p.value[4], Max.Open.pairwise.manova$p.value[1], Max.Open.pairwise.manova$p.value[2]))
Max.KNP.results <- data.frame("Comparision" = c("Late vs. Transition", "Late vs. Early", "Transition vs. Early"), 
                              "F"=c(Max.KNP.pairwise.manova$F.value[4], Max.KNP.pairwise.manova$F.value[1], Max.KNP.pairwise.manova$F.value[2]), 
                              "p"=c(Max.KNP.pairwise.manova$p.value[4], Max.KNP.pairwise.manova$p.value[1], Max.KNP.pairwise.manova$p.value[2]))
Amp.Closed.results <- data.frame("Comparision" = c("Late vs. Transition", "Late vs. Early", "Transition vs. Early"), 
                                 "F"=c(Amp.Closed.pairwise.manova$F.value[4], Amp.Closed.pairwise.manova$F.value[1], Amp.Closed.pairwise.manova$F.value[2]), 
                                 "p"=c(Amp.Closed.pairwise.manova$p.value[4], Amp.Closed.pairwise.manova$p.value[1], Amp.Closed.pairwise.manova$p.value[2]))
Amp.Open.results <- data.frame("Comparision" = c("Late vs. Transition", "Late vs. Early", "Transition vs. Early"), 
                               "F"=c(Amp.Open.pairwise.manova$F.value[4], Amp.Open.pairwise.manova$F.value[1], Amp.Open.pairwise.manova$F.value[2]), 
                               "p"=c(Amp.Open.pairwise.manova$p.value[4], Amp.Open.pairwise.manova$p.value[1], Amp.Open.pairwise.manova$p.value[2]))
Amp.KNP.results <- data.frame("Comparision" = c("Late vs. Transition", "Late vs. Early", "Transition vs. Early"), 
                              "F"=c(Amp.KNP.pairwise.manova$F.value[4], Amp.KNP.pairwise.manova$F.value[1], Amp.KNP.pairwise.manova$F.value[2]), 
                              "p"=c(Amp.KNP.pairwise.manova$p.value[4], Amp.KNP.pairwise.manova$p.value[1], Amp.KNP.pairwise.manova$p.value[2]))

### Export Results ###
write.table(Max.Closed.results, file = str_c(outWS.stats, "MaxClosed.csv"), sep = ",")
write.table(Max.Open.results, file = str_c(outWS.stats, "MaxOpen.csv"), sep = ",")
write.table(Max.KNP.results, file = str_c(outWS.stats, "MaxKNP.csv"), sep = ",")
write.table(Amp.Closed.results, file = str_c(outWS.stats, "AmpClosed.csv"), sep = ",")
write.table(Amp.Open.results, file = str_c(outWS.stats, "AmpOpen.csv"), sep = ",")
write.table(Amp.KNP.results, file = str_c(outWS.stats, "AmpKNP.csv"), sep = ",")
