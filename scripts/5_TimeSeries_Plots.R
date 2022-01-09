# Plot vegetation-rainfall residuals over time (Early, Transition, Late), symbolized by Management Type (Closed, Open, KNP).
# Ellie Linden (ellielinden10@gmail.com)
# January 2022

library(plotrix) # std.error
library(ggpubr) # ggarrange
library(gridExtra) # grid.arrange
library(cowplot) # plot_grid

### Set Output Workspace for Plot Exports ###
outWS <- "E:/K2C_LandscapeChange_Analysis_v2/Manuscript/Plots/"

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


# ----------------------------- #
# - Combine Max and Amp Plots - #
# ----------------------------- #
point.plots <- ggarrange(Max.points, Amp.points,
                        ncol = 2, nrow = 1,
                        labels = c("a","b"),
                        font.label = list(size=20),
                        label.x = 0.12)

############################
### Create shared legend ###
############################
legend <- ggplot() + 
  # Teal Box:
  geom_rect(aes(xmin = 2, xmax = 3, ymin = 1, ymax = 2), 
            fill = "#03899C", color = "black") + 
  # Purples Box:
  geom_rect(aes(xmin = 2, xmax = 3, ymin = 2, ymax = 3), 
            fill = "#A101A6", color = "black") +
  # Yellows Box:
  geom_rect(aes(xmin = 2, xmax = 3, ymin = 3, ymax = 4), 
            fill = "#FFA200", color = "black") + 
  # Texts:
  annotate("text", x = 1.5, y = 3.4, label = "Closed", size=5) +
  annotate("text", x = 1.5, y = 2.5, label = "Open", size=5) +
  annotate("text", x = 1.5, y = 1.5, label = "KNP", size=5) +
  theme_void() +
  scale_x_continuous(expand = c(0.5, 0.5)) +
  scale_y_continuous(expand = c(.06, .06))

#################################
### Combine plots with legend ###
#################################

### Force legend plot to have "NULL" spaces on either side ###
legend.2 <- plot_grid(legend, NULL, NULL, NULL, NULL, nrow = 1)

### Combined mean-variance plots and legend
point.plot.with.legend <- plot_grid(point.plots, legend.2,  
                                   align = "v",
                                   nrow = 2,
                                   rel_heights = c(9, 1))

### Save plot ###
TimeSeries.output.file <- str_c(outWS, "TimeSeries_10000x5000.jpeg")
jpeg(TimeSeries.output.file, width = 10000, height = 5000, units = "px", res = 600)
plot(point.plot.with.legend)
dev.off()
