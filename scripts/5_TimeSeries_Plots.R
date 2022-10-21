# Plot vegetation-rainfall residuals over time (Early, Transition, Late), symbolized by Management Type (Closed, Open, KNP).
# Ellie Linden (ellielinden10@gmail.com)
# March 2021

library(plotrix) # std.error
library(ggpubr) # ggarrange
library(gridExtra) # grid.arrange
library(cowplot) # plot_grid

### Set Output Workspace for Plot Exports ###
outWS <- "E:/K2C_LandscapeChange_Analysis_v2/Manuscript/Plots_updatedLegends/"

###########################################
### Calculate AVerage/SE and Clean Data ###
###########################################

### Calculate Average and SE of the Means ###
K2C.summary.2 <- K2C.summary %>% 
  group_by(FenceAction, Time) %>% 
  mutate(MaxRes_Mean_2 = mean(MaxRes_Mean),
         AmpRes_Mean_2 = mean(AmpRes_Mean),
         MaxRes_SE = std.error(MaxRes_Mean),
         AmpRes_SE = std.error(AmpRes_Mean)) %>% 
  ungroup() %>% 
  dplyr::select(FenceAction, Time, MaxRes_Mean_2, AmpRes_Mean_2, MaxRes_SE, AmpRes_SE) %>% 
  distinct()

### Re-Order Data for Plots ###
K2C.summary.2 <- K2C.summary.2 %>% 
  mutate(FenceAction = fct_relevel(FenceAction, "KNP", "RemovedFence", "RemainedClosed"),
         Time = fct_relevel(Time, "Early", "Transition", "Late"))

##################
### Set Colors ###
##################
purples <- c("#68006C", "#CF5FD3",  "#A101A6")
teals <- c("#015965", "#5FC0CE", "#03899C")
golds <- c("#A66A00", "#FFCC73", "#FFA200")
reserve.colors <- c("#03899C", "#A101A6", "#FFA200")

################################
### Plot Points with SE Bars ###
################################

# ------- #
# - Max - #
# ------- #
Max.points <- ggplot(K2C.summary.2, aes(x=Time, y=MaxRes_Mean_2, group=FenceAction, color=FenceAction)) +
  geom_errorbar(aes(ymin=MaxRes_Mean_2-MaxRes_SE, ymax=MaxRes_Mean_2+MaxRes_SE), width=.4, size=1, position=position_dodge(0.1)) +
  geom_line(size=1, position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=3) +
  scale_color_manual(values=reserve.colors) +
  labs(x="Time Period", y="Max-Rainfall Residual") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none",
        text = element_text(size=20)) 

# ------- #
# - Amp - #
# ------- #
Amp.points <- ggplot(K2C.summary.2, aes(x=Time, y=AmpRes_Mean_2, group=FenceAction, color=FenceAction)) +
  geom_errorbar(aes(ymin=AmpRes_Mean_2-AmpRes_SE, ymax=AmpRes_Mean_2+AmpRes_SE), width=.4, size=1, position=position_dodge(0.1)) +
  geom_line(size=1, position=position_dodge(0.1)) +
  geom_point(position=position_dodge(0.1), size=3) +
  scale_color_manual(values=reserve.colors) +
  labs(x="Time Period", y="Amp-Rainfall Residual") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none",
        text = element_text(size=20)) 

#################################
### Combine Max and Amp Plots ###
#################################

# Combine plots to single figure
point.plots <- ggarrange(Max.points, Amp.points,
                        ncol = 2, nrow = 1,
                        labels = c("a","b"),
                        font.label = list(size=20),
                        label.x = 0.12)

# Export figure
point.plots.output.file <- str_c(outWS, "PointPlots_10000x5000_no_legend.jpeg")
jpeg(point.plots.output.file, width = 10000, height = 5000, units = "px", res = 600)
plot(point.plots)
dev.off()