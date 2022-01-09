# Visualize clusters across coordinate space using Nonmetric Multidimensional Scaling (NMDS) plots.
# Ellie Linden (ellielinden10@gmail.com)
# January 2022

library(tidyverse)
library(vegan)
library(smacof) # mds
library(ggpubr) # ggarrange
library(gridExtra) # grid.arrange
library(cowplot) # plot_grid

### Set Output Workspace for Plot Exports ###
outWS <- "E:/K2C_LandscapeChange_Analysis_v2/Manuscript/Plots/"

##########################################
### Nonmetric Multidimensional Scaling ###
##########################################

# ------- #
# - Max - #
# ------- #
Max.Closed.vegdist <- vegdist(Max.Closed[1:2], method = 'euclidean')
Max.Closed.mds <- mds(Max.Closed.vegdist)
Max.Closed.scores <- as.data.frame(Max.Closed.mds$conf) # extract scores from mds output
Max.Closed.scores$Time <- Max.Closed$Time

Max.Open.vegdist <- vegdist(Max.Open[1:2], method = 'euclidean')
Max.Open.mds <- mds(Max.Open.vegdist)
Max.Open.scores <- as.data.frame(Max.Open.mds$conf) # extract scores from mds output
Max.Open.scores$Time <- Max.Open$Time

Max.KNP.vegdist <- vegdist(Max.KNP[1:2], method = 'euclidean')
Max.KNP.mds <- mds(Max.KNP.vegdist)
Max.KNP.scores <- as.data.frame(Max.KNP.mds$conf) # extract scores from mds output
Max.KNP.scores$Time <- Max.KNP$Time

# ------- #
# - Amp - #
# ------- #
Amp.Closed.vegdist <- vegdist(Amp.Closed[1:2], method = 'euclidean')
Amp.Closed.mds <- mds(Amp.Closed.vegdist)
Amp.Closed.scores <- as.data.frame(Amp.Closed.mds$conf) # extract scores from mds output
Amp.Closed.scores$Time <- Amp.Closed$Time

Amp.Open.vegdist <- vegdist(Amp.Open[1:2], method = 'euclidean')
Amp.Open.mds <- mds(Amp.Open.vegdist)
Amp.Open.scores <- as.data.frame(Amp.Open.mds$conf) # extract scores from mds output
Amp.Open.scores$Time <- Amp.Open$Time

Amp.KNP.vegdist <- vegdist(Amp.KNP[1:2], method = 'euclidean')
Amp.KNP.mds <- mds(Amp.KNP.vegdist)
Amp.KNP.scores <- as.data.frame(Amp.KNP.mds$conf) # extract scores from mds output
Amp.KNP.scores$Time <- Amp.KNP$Time

########################
### Set Min/Max Axes ###
########################

# ------- #
# - Max - #
# ------- #
Max.xmin.within.management <- min(Max.Closed.scores$D1, Max.Open.scores$D1, Max.KNP.scores$D1) - 0.2
Max.xmax.within.management <- max(Max.Closed.scores$D1, Max.Open.scores$D1, Max.KNP.scores$D1) + 0.2
Max.ymin.within.management <- min(Max.Closed.scores$D2, Max.Open.scores$D2, Max.KNP.scores$D2) - 0.01
Max.ymax.within.management <- max(Max.Closed.scores$D2, Max.Open.scores$D2, Max.KNP.scores$D2) + 0.01

# ------- #
# - Amp - #
# ------- #
Amp.xmin.within.management <- min(Amp.Closed.scores$D1, Amp.Open.scores$D1, Amp.KNP.scores$D1)- 0.2
Amp.xmax.within.management <- max(Amp.Closed.scores$D1, Amp.Open.scores$D1, Amp.KNP.scores$D1)+ 0.2
Amp.ymin.within.management <- min(Amp.Closed.scores$D2, Amp.Open.scores$D2, Amp.KNP.scores$D2)- 0.01
Amp.ymax.within.management <- max(Amp.Closed.scores$D2, Amp.Open.scores$D2, Amp.KNP.scores$D2)+ 0.01

#############################
### List/Clean Dataframes ###
#############################

# Create list of original dataframes
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

##################################################
### Loop through dataframes to plot NMDS plots ###
##################################################
NMDS_PlotList = list()

for (i in names(df.list)) {
  print(i)
  df <- df.list[[i]]
  # Calculate distance and create mds 
  vegdist <- vegdist(df[1:2], method = 'euclidean')
  mds <- mds(vegdist)
  scores <- as.data.frame(mds$conf) # extract scores from mds output
  scores$Time <- df$Time
  # Create hulls
  Time.Early <- scores[scores$Time == "Early", ][chull(scores[scores$Time == "Early", c("D1", "D2")]), ]  # hull values for grp A
  Time.Transition <- scores[scores$Time == "Transition", ][chull(scores[scores$Time == "Transition", c("D1", "D2")]), ]  # hull values for grp A
  Time.Late <- scores[scores$Time == "Late", ][chull(scores[scores$Time == "Late", c("D1", "D2")]), ]  # hull values for grp A
  hull.data <- rbind(Time.Early, Time.Transition, Time.Late)  #combine grp.a and grp.b
  # Create centroids
  Centroids <- aggregate(cbind(scores$D1, scores$D2)~scores$Time, scores, mean)
  names(Centroids)[1] <- "TimePeriod"
  names(Centroids)[2] <- "D1"
  names(Centroids)[3] <- "D2"
  # Set axis limts based on metric
  metric <- str_split_fixed((i[1]), pattern = "_", n=2)[[1]]
  if (metric=="Max") {
    xmin = Max.xmin.within.management
    xmax = Max.xmax.within.management
    ymin = Max.ymin.within.management
    ymax = Max.ymax.within.management
  } else {
    xmin = Amp.xmin.within.management
    xmax = Amp.xmax.within.management
    ymin = Amp.ymin.within.management
    ymax = Amp.ymax.within.management
  }
  # Set colors based on management type
  management <- str_split_fixed((i[1]), pattern = "_", n=2)[[2]]
  if (management=="C") {
    colors = golds
  } else if (management=="O") {
    colors=purples
  } else {
    colors=teals
  }
  
  p <- ggplot() + 
    geom_polygon(data=hull.data,aes(x=D1,y=D2,fill=Time,group=Time),alpha=0.6) +
    geom_point(data=scores,aes(x=D1,y=D2,colour=Time),size=3) + 
    geom_point(data = Centroids, aes(x=D1,y=D2,colour=TimePeriod), size=5, pch=4, stroke=2) +
    theme_classic() +
    theme(panel.background = element_rect(fill = "white"),
          legend.position = "none",
          text = element_text(size=20)) +
    xlab("NMDS1") + ylab("NMDS2") + 
    ylim(ymin, ymax) + xlim(xmin, xmax) + 
    scale_color_manual(values=colors) + # point colors
    scale_fill_manual(values=colors) # polygon colors
  outname <- str_c(outWS, i, ".jpeg")
  jpeg(outname, width = 4000, height = 2000, units = "px", res = 300)
  plot(p)
  dev.off()
  NMDS_PlotList[[i]] = p
  
}

#############################################################
### Extract Max and Amp plots to arrange in a single plot ###
#############################################################
nmds.Max.C <- NMDS_PlotList[["Max_C"]]
nmds.Max.O <- NMDS_PlotList[["Max_O"]]
nmds.Max.K <- NMDS_PlotList[["Max_K"]]
nmds.Amp.C <- NMDS_PlotList[["Amp_C"]]
nmds.Amp.O <- NMDS_PlotList[["Amp_O"]]
nmds.Amp.K <- NMDS_PlotList[["Amp_K"]]

nmds.plots <- ggarrange(nmds.Max.C, nmds.Amp.C, nmds.Max.O, nmds.Amp.O, nmds.Max.K, nmds.Amp.K,
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
nmds.plot.with.legend <- plot_grid(nmds.plots, legend.2,  
                                   align = "v",
                                   nrow = 2,
                                   rel_heights = c(9, 1))

### Save plot ###
NMDS.output.file <- str_c(outWS, "NMDS_10000x10000.jpeg")
jpeg(NMDS.output.file, width = 10000, height = 10000, units = "px", res = 600)
plot(nmds.plot.with.legend)
dev.off()
