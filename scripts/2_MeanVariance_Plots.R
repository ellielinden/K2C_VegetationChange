# Creating mean-variance plots.
# Ellie Linden (ellielinden10@gmail.com)
# March 2021

library(tidyverse)
library(ggalt) # geom_encircle
library(ggrepel) # geom_text_repel
library(ggpubr) # ggarrange
library(gridExtra) # grid.arrange
library(cowplot) # plot_grid

### Set Output Workspace for Plot Exports ###
outWS <- "E:/K2C_LandscapeChange_Analysis_v2/Manuscript/Plots/"

#######################################
### Set Min/Max Axes and Intercepts ###
#######################################

# ------- #
# - Max - #
# ------- #
Max.xmin <- min(Max.Closed$MaxRes_Mean, Max.Open$MaxRes_Mean, Max.KNP$MaxRes_Mean) - 0.02 # adding space so the cluster outlines don't get cut off
Max.xmax <- max(Max.Closed$MaxRes_Mean, Max.Open$MaxRes_Mean, Max.KNP$MaxRes_Mean) + 0.02 # adding space so the cluster outlines don't get cut off
Max.ymin <- min(Max.Closed$MaxRes_Var, Max.Open$MaxRes_Var, Max.KNP$MaxRes_Var)
Max.ymax <- max(Max.Closed$MaxRes_Var, Max.Open$MaxRes_Var, Max.KNP$MaxRes_Var)
Max.xintercept <- mean(cbind(Max.Closed$MaxRes_Mean,Max.Open$MaxRes_Mean, Max.KNP$MaxRes_Mean))
Max.yintercept <- mean(cbind(Max.Closed$MaxRes_Var,Max.Open$MaxRes_Var, Max.KNP$MaxRes_Var))

# ------- #
# - Amp - #
# ------- #
Amp.xmin <- min(Amp.Closed$AmpRes_Mean, Amp.Open$AmpRes_Mean, Amp.KNP$AmpRes_Mean) - 0.02 # adding space so the cluster outlines don't get cut off
Amp.xmax <- max(Amp.Closed$AmpRes_Mean, Amp.Open$AmpRes_Mean, Amp.KNP$AmpRes_Mean) + 0.02 # adding space so the cluster outlines don't get cut off
Amp.ymin <- min(Amp.Closed$AmpRes_Var, Amp.Open$AmpRes_Var, Amp.KNP$AmpRes_Var)
Amp.ymax <- max(Amp.Closed$AmpRes_Var, Amp.Open$AmpRes_Var, Amp.KNP$AmpRes_Var)
Amp.xintercept <- mean(cbind(Amp.Closed$AmpRes_Mean, Amp.Open$AmpRes_Mean, Amp.KNP$AmpRes_Mean))
Amp.yintercept <- mean(cbind(Amp.Closed$AmpRes_Var, Amp.Open$AmpRes_Var, Amp.KNP$AmpRes_Var))

#############################
### List/Clean Dataframes ###
#############################

### Create List of Original Dataframes ###
original.df.list <- list("Max_C" = Max.Closed, "Max_O" = Max.Open, "Max_K" = Max.KNP, 
                "Amp_C" = Amp.Closed, "Amp_O" = Amp.Open, "Amp_K" = Amp.KNP)

### Loop through Original Dataframes to Rename Columns (so they're all consistent) ###
df.list = list()

for (i in names(original.df.list)) {
  print(i)
  df <- original.df.list[[i]]
  metric <- str_split_fixed(i, pattern = "_", n=2)[1]
  original_mean_variable <- str_c(metric, "Res_Mean")
  original_var_variable <- str_c(metric, "Res_Var")
  df <- df %>%
    rename(Mean = original_mean_variable,
           Var = original_var_variable) %>%
    arrange(Mean)
  df.list[[i]] = df
}

##################
### Set Colors ###
##################
purples <- c("#68006C", "#CF5FD3",  "#A101A6")
teals <- c("#015965", "#5FC0CE", "#03899C")
golds <- c("#A66A00", "#FFCC73", "#FFA200")
golds2 <- c("#8c2d04", "#fe9929", "#cc4c02") # darker golds for text

####################################################################
### Loop Through Dataframes to Create/Export Mean-Variance Plots ###
####################################################################
MeanVariance_PlotList = list()

for (i in names(df.list)) {
  print(i)
  df <- df.list[[i]]
  centroids <- aggregate(cbind(df$Mean, df$Var)~df$Cluster, df, mean)
  centroids <- centroids %>%
    rename(Cluster = `df$Cluster`,
           Mean = "V1",
           Var = "V2")
  Mean.Mean <- mean(df$Mean)
  Var.Mean <- mean(df$Var)
  
  # Set axis limts based on metric
  metric <- str_split_fixed((i[1]), pattern = "_", n=2)[[1]]
  if (metric=="Max") {
    xmin = Max.xmin
    xmax = Max.xmax
    ymin = Max.ymin
    ymax = Max.ymax
    xintercept = Max.xintercept
    yintercept = Max.yintercept
  } else {
    xmin = Amp.xmin
    xmax = Amp.xmax
    ymin = Amp.ymin
    ymax = Amp.ymax
    xintercept = Amp.xintercept
    yintercept = Amp.yintercept
    
  }
  
  # Set colors based on management type
  management <- str_split_fixed((i[1]), pattern = "_", n=2)[[2]]
  if (management=="C") {
    colors = golds
    text.colors = golds2
  } else if (management=="O") {
    colors=purples
    text.colors =purples
  } else {
    colors=teals
    text.colors =teals
  }
  
  # Create plots
  p <- ggplot(data = df) +
    geom_hline(yintercept = yintercept, color="black", size=0.5) +
    geom_vline(aes(xintercept = xintercept), color="black", size=0.5) +
    geom_encircle(mapping = aes(x = Mean, y = Var, group = Cluster), 
                  size=2, 
                  s_shape = 1, 
                  expand = 0,
                  alpha = 100,
                  color = "black",
                  # fill="NA" , # blank is "NA"
                  fill="grey85",
                  show.legend = FALSE) +
    geom_path(aes(x = Mean,
                  y = Var,
                  color = Time),
              size=1) +
    geom_point(data=centroids,
               mapping = aes(Mean, Var),
               color="black",
               shape=4,
               size=1.5,
               stroke = 1.5) +
    geom_point(mapping = aes(x = Mean,
                             y = Var,
                             fill = Time),
               color="black",
               shape = 21, # needed for point fill colors
               size=4) +
    geom_text_repel(aes(x = Mean,
                  y = Var,
                  label = Year,
                  color = Time,
                  max.overlaps  = Inf),
                  size = 5,
                  min.segment.length = Inf) +
    labs(x = "Mean", y = "Variance", color="Growing Season") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position = "none",
          text = element_text(size=20)) +
    ylim(ymin, ymax) + xlim(xmin, xmax) +
    scale_color_manual(values=text.colors) + # sets colors for text
    scale_fill_manual(values=colors) # sets colors for point fill
  
  # Export Individual Plots
  outname <- str_c(outWS, i, ".jpeg")
  jpeg(outname, width = 4000, height = 2000, units = "px", res = 300)
  plot(p)
  dev.off()
  MeanVariance_PlotList[[i]] = p
  
}

##############################################################
### Extract individual plots to arrange in a single figure ###
##############################################################
p.Max.C <- MeanVariance_PlotList[["Max_C"]]
p.Max.O <- MeanVariance_PlotList[["Max_O"]]
p.Max.K <- MeanVariance_PlotList[["Max_K"]]
p.Amp.C <- MeanVariance_PlotList[["Amp_C"]]
p.Amp.O <- MeanVariance_PlotList[["Amp_O"]]
p.Amp.K <- MeanVariance_PlotList[["Amp_K"]]

mean.variance.plots <- ggarrange(p.Max.C, p.Amp.C, p.Max.O, p.Amp.O, p.Max.K, p.Amp.K,
                                 ncol = 2, nrow = 3,
                                 labels = c("a", "d", "b", "e", "c", "f"),
                                 font.label = list(size=20),
                                 label.x = 0.12)

############################
### Create shared legend ###
############################
legend <- ggplot() + 
  # Teal Boxes:
  geom_rect(aes(xmin = 1, xmax = 2, ymin = 1, ymax = 2), 
            fill = "#015965", color = "black") + 
  geom_rect(aes(xmin = 2, xmax = 3, ymin = 1, ymax = 2), 
            fill = "#03899C", color = "black") +
  geom_rect(aes(xmin = 3, xmax = 4, ymin = 1, ymax = 2), 
            fill = "#5FC0CE", color = "black") +
  # Purples Boxes:
  geom_rect(aes(xmin = 1, xmax = 2, ymin = 2, ymax = 3), 
            fill = "#68006C", color = "black") +
  geom_rect(aes(xmin = 2, xmax = 3, ymin = 2, ymax = 3), 
            fill = "#A101A6", color = "black") +
  geom_rect(aes(xmin = 3, xmax = 4, ymin = 2, ymax = 3), 
            fill = "#CF5FD3", color = "black") +
  # Yellows Boxes:
  geom_rect(aes(xmin = 1, xmax = 2, ymin = 3, ymax = 4), 
            fill = "#A66A00", color = "black") +
  geom_rect(aes(xmin = 2, xmax = 3, ymin = 3, ymax = 4), 
            fill = "#FFA200", color = "black") +
  geom_rect(aes(xmin = 3, xmax = 4, ymin = 3, ymax = 4), 
            fill = "#FFCC73", color = "black") +
  # Texts:
  annotate("text", x = 1.5, y = 4.4, label = "Early", size=5) +
  annotate("text", x = 2.5, y = 4.4, label = "Transition", size=5) +
  annotate("text", x = 3.5, y = 4.4, label = "Late", size=5) +
  annotate("text", x = 0.6, y = 3.4, label = "Closed", size=5) +
  annotate("text", x = 0.6, y = 2.5, label = "Open", size=5) +
  annotate("text", x = 0.6, y = 1.5, label = "KNP", size=5) +
  theme_void() +
  scale_x_continuous(expand = c(.09, .09)) +
  scale_y_continuous(expand = c(.06, .06))

#################################
### Combine plots with legend ###
#################################

### Force legend plot to have "NULL" spaces on either side ###
legend.2 <- plot_grid(legend, NULL, NULL, NULL, NULL, nrow = 1)

### Combined mean-variance plots and legend
mean.variance.plot.with.legend <- plot_grid(mean.variance.plots, NULL, legend.2, # add "NULL" to create a little space between plot and legend
                                            align = "v",
                                            nrow = 3,
                                            rel_heights = c(9, 0.2, 1))

### Save plot ###
MeanVariancePlot.output.file <- str_c(outWS, "MeanVariancePlot_10000x10000.jpeg")
jpeg(MeanVariancePlot.output.file, width = 10000, height = 10000, units = "px", res = 600)
plot(mean.variance.plot.with.legend)
dev.off()