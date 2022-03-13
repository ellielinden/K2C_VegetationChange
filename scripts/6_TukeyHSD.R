# Running post-hoc Tukey HSD.
# Ellie Linden (ellielinden10@gmail.com)
# March 2021

library(tidyverse)
library(broom) # tidying statistical outputs

### Set output workspace ###
outWS.stats <- outWS <- "E:/K2C_LandscapeChange_Analysis_v2/Manuscript/Stats/"

# ------- #
# - Max - #
# ------- #
Max_aov <- aov(MaxRes_Mean ~ FenceAction*Time, data = K2C.summary)
Max.tukey.all <- TukeyHSD(Max_aov)
Max.tukey.all.tidy <- tidy(Max.tukey.all)
write.table(Max.tukey.all.tidy, file = str_c(outWS.stats, "Max_tukey.csv"), sep = ",")

# ------- #
# - Amp - #
# ------- #
Amp_aov <- aov(AmpRes_Mean ~ FenceAction*Time, data = K2C.summary)
Amp.tukey.all <- TukeyHSD(Amp_aov)
Amp.tukey.all.tidy <- tidy(Amp.tukey.all)
write.table(Amp.tukey.all.tidy, file = str_c(outWS.stats, "Amp_tukey.csv"), sep = ",")