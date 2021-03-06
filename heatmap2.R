library(ggplot2)
library(reshape2)
library(ggdendro)
library(grid)
library(gridExtra)
library(cowplot)
library(dplyr)
library(tidyr)
library(tibble)
library(scales)
library(plotly)


#################################### 
############### POSTER #############
#################################### 

# ldata <- read.table("/Users/dimitras/Documents/dimitra/Workspace/Luda_plots/data/FC_Untx_plasma.txt", sep="\t", header = TRUE)
ldata <- read.table("/Users/dimitras/Documents/dimitra/Workspace/Luda_plots/data/FC_PBS_veh_ctrl_plasma.txt", sep="\t", header = TRUE)

ldata.for_hclust = ldata %>% 
  column_to_rownames("measure") %>% 
  # select(Serum_serum:LPS.Zymosan_Plasma.24h) %>%
  select(LPS_Plasma.4h:LPS.Zymosan_Plasma.24h) %>%
  as.matrix

dd.col <- as.dendrogram(hclust(dist(1 - cor(ldata.for_hclust))))
# dd.col <- as.dendrogram(hclust(dist(ldata.for_hclust)))
col.ord <- order.dendrogram(dd.col)

dd.row <- as.dendrogram(hclust(dist(1 - cor(t(ldata.for_hclust)))))
# dd.row <- as.dendrogram(hclust(dist(t(ldata.for_hclust))))
row.ord <- order.dendrogram(dd.row)

xx_names <- attr(ldata.for_hclust, "dimnames")
df <- as.data.frame(ldata.for_hclust)
colnames(df) <- xx_names[[2]]
df$measure <- xx_names[[1]]
df$measure <- with(df, factor(measure, levels=measure, ordered=TRUE))

mdf <- melt(df, id.vars="measure")

#for cor clust
ddata_y <- dendro_data(dd.row)
ddata_x <- dendro_data(dd.col)
# # for dist clust
# ddata_x <- dendro_data(dd.row)
# ddata_y <- dendro_data(dd.col)

### Set up a blank theme
theme_none <- theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.title.x = element_text(colour=NA),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.line = element_blank()
  #axis.ticks.length = element_blank()
)

### Create plot components ###    
# Heatmap
# ldata.hclustered = hclust(as.dist((1 - cor(t(ldata.for_hclust)))))

factors=as.character(ddata_y$labels$label[row.ord])

p1 <- mdf %>%
  ## gather(id,fc,Serum_serum:LPS.Zymosan_Plasma.24h) %>%
  ## gather(id,fc,LPS_Plasma.4h:LPS.Zymosan_Plasma.24h) %>%
  mutate(TP=gsub("^.*_(.*)","\\1",variable),
         # TP=ifelse(TP=="serum","Serum",TP), # change the value
         TP=ifelse(TP=="Plasma.4h","Plasma 4h",TP), # change the value
         TP=ifelse(TP=="Plasma.24h","Plasma 24h",TP), # change the value
         # TP=factor(TP,levels=c("Serum", "Plasma 4h", "Plasma 24h")),
         TP=factor(TP,levels=c("Plasma 4h", "Plasma 24h")),
         condition=gsub("^(.*)_(.*)","\\1",variable),
         # condition = factor(condition, levels=c("Serum", "PBS", "LPS", "Zymosan", "LPS.Zymosan")),
         condition = factor(condition, levels=c("LPS", "Zymosan", "LPS.Zymosan")),
         measure = factor(measure, levels=factors),
         logfc=log(value),
         logfc=replace(logfc, logfc==-Inf, NA),
         # logfc=ifelse(logfc>2.5,2.5,logfc),
         # logfc=ifelse(logfc< -2.5,-2.5,logfc)
         logfc=ifelse(logfc>2,2,logfc),
         logfc=ifelse(logfc< -2,-2,logfc)
  ) %>% 
  ggplot(aes(x = condition, y = measure, group=TP, fill=logfc)) +
  geom_tile() +
  # scale_fill_gradient2(low = "blue", high = "red", midpoint = 0, na.value = "grey80", values= rescale(-0.5108:7.1682)) +
  # scale_fill_gradientn(colours = c("royalblue4", "grey90", "firebrick4"), na.value = "white", limits=c(-3,3), labels=c("<-2","0",">2"), breaks = c(-2,0,2), values=rescale(c(-3,0,3), from=c(-3,3))) +
  scale_fill_gradientn(colours = c("royalblue4", "grey90", "firebrick4"), na.value = "white", limits=c(-2,2), labels=c("<-2","0",">2"), breaks = c(-1.6,0,1.5), values=rescale(c(-2,0,2), from=c(-2,2))) +
  facet_grid(.~TP, scales = "free_x", space = "free_x") +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4, size = 30, face="bold"),
        axis.text.y = element_text(size = 30, face="bold"),
        strip.text = element_text(size = 30, face="bold"),
        strip.background = element_rect(fill = "dodgerblue3"),
        legend.title = element_text(size = 30, face="bold"),
        legend.text = element_text(size = 30, face="bold"),
        panel.grid = element_blank(),
        legend.position = c(.95, .05),
        legend.background = element_rect(fill="snow"))

# Dendrogram 1
p2 <- ggplot(segment(ddata_x)) + 
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend), size=2) + 
  labs(x = "", y = "") + 
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank())
  

# Dendrogram 2
p3 <- ggplot(segment(ddata_y)) + 
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend), size=2) + 
  coord_flip() + 
  labs(x = "", y = "") + 
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank())

grid.newpage()

# combined_plot = ggdraw() +
#   draw_plot(p1, 0, 0, .8, .91) +
#   draw_plot(p2, .13, .9, .66, .09) +
#   draw_plot(p3, .77, .09, .2, .8)

combined_plot = ggdraw() +
  draw_plot(p1, 0, 0, .85, .95) +
  draw_plot(p2, .2, .94, .6, .05) +
  draw_plot(p3, .83, .1, .15, .83)

# ggsave("/Users/dimitras/Documents/dimitra/Workspace/Luda_plots/plot_results/tiff/FC_Untx_plasma_w_dendro.tiff", combined_plot, height = 60, width = 50, units ="cm")
# ggsave("/Users/dimitras/Documents/dimitra/Workspace/Luda_plots/plot_results/jpeg/FC_Untx_plasma_w_dendro.jpg", combined_plot, height = 60, width = 50, units ="cm")
# ggsave("/Users/dimitras/Documents/dimitra/Workspace/Luda_plots/plot_results/eps/FC_Untx_plasma_w_dendro.eps", combined_plot, height = 60, width = 50, units ="cm")

ggsave("/Users/dimitras/Documents/dimitra/Workspace/Luda_plots/plot_results/tiff/FC_PBS_veh_ctrl_plasma_w_dendro.tiff", combined_plot, height = 60, width = 35, units ="cm")
ggsave("/Users/dimitras/Documents/dimitra/Workspace/Luda_plots/plot_results/jpeg/FC_PBS_veh_ctrl_plasma_w_dendro.jpg", combined_plot, height = 60, width = 35, units ="cm")
ggsave("/Users/dimitras/Documents/dimitra/Workspace/Luda_plots/plot_results/eps/FC_PBS_veh_ctrl_plasma_w_dendro.eps", combined_plot, height = 60, width = 35, units ="cm")

# print(p1, vp=viewport(0.8, 0.8, x=0.4, y=0.4))
# print(p2, vp=viewport(0.52, 0.2, x=0.45, y=0.9)) # angle=180
# print(p3, vp=viewport(0.2, 0.8, x=0.9, y=0.4))




#################################### 
############### PAPER ##############
#################################### 

# First, replaced all non-significant FC values in the data tables (grey ones) with 1.0.

################ FC_PBS_veh_ctrl_plasma.txt #################### 

ldata <- read.table("/Users/dimitras/Documents/dimitra/Workspace/Luda_heatmaps4poster/data/paper/FC_PBS_veh_ctrl_plasma.txt", sep="\t", header = TRUE)

ldata.for_hclust = ldata %>% 
  column_to_rownames("measure") %>% 
  select(LPS_Plasma.4h:LPS.Zymosan_Plasma.24h) %>%
  as.matrix

ldata.for_hclust[1,1]= 1.00000001 # change first value of first row to be it close to 1, because the values of the whole row are 1s, which makes the sd 0, thus the dist creates NAs and the hclust crashes.

dd.col <- as.dendrogram(hclust(dist(1 - cor(ldata.for_hclust))))
col.ord <- order.dendrogram(dd.col)

dd.row <- as.dendrogram(hclust(dist(1 - cor(t(ldata.for_hclust)))))
row.ord <- order.dendrogram(dd.row)

xx_names <- attr(ldata.for_hclust, "dimnames")
df <- as.data.frame(ldata.for_hclust)
colnames(df) <- xx_names[[2]]
df$measure <- xx_names[[1]]
df$measure <- with(df, factor(measure, levels=measure, ordered=TRUE))

mdf <- melt(df, id.vars="measure")

#for cor clust
ddata_y <- dendro_data(dd.row)
ddata_x <- dendro_data(dd.col)

### Set up a blank theme
theme_none <- theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.title.x = element_text(colour=NA),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.line = element_blank()
)

### Create plot components ###    
factors=as.character(ddata_y$labels$label[row.ord])

p1 <- mdf %>%
  mutate(TP=gsub("^.*_(.*)","\\1",variable),
         TP=ifelse(TP=="Plasma.4h","Plasma 4h",TP),
         TP=ifelse(TP=="Plasma.24h","Plasma 24h",TP),
         TP=factor(TP,levels=c("Plasma 4h", "Plasma 24h")),
         condition=gsub("^(.*)_(.*)","\\1",variable),
         condition=ifelse(condition=="Zymosan","Zym",condition),
         condition=ifelse(condition=="LPS.Zymosan","LPS+Zym",condition),
         condition = factor(condition, levels=c("LPS", "Zym", "LPS+Zym")),
         measure = factor(measure, levels=factors),
         logfc=log(value),
         logfc=replace(logfc, logfc==-Inf, NA)
         # logfc=ifelse(logfc>2,2,logfc),
         # logfc=ifelse(logfc< -2,-2,logfc)
  ) %>% 
  ggplot(aes(x = condition, y = measure, group=TP, fill=logfc)) +
  geom_tile(aes(width=0.99, height=0.99)) +
  # geom_text(aes(label=round(logfc,1)), size=10) +
  # scale_fill_gradientn(colours = c("royalblue4", "grey90", "firebrick4"), na.value = "white", limits=c(-2,2), labels=c("<-2","0",">2"), breaks = c(-1.6,0,1.5), values=rescale(c(-2,0,2), from=c(-2,2))) +
  scale_fill_gradientn(colours = c("royalblue4", "royalblue","royalblue3", "royalblue2", "royalblue1", "grey90", "firebrick1", "firebrick2", "firebrick3", "firebrick", "firebrick4"), na.value = "white", limits=c(-5,5), labels=c("-4","-2", "0","2","4"), breaks = c(-4,-2,0,2,4)) +
  # scale_fill_gradientn(colours = c("royalblue4", "royalblue","royalblue3", "royalblue2", "royalblue1", "grey90", "firebrick1", "firebrick2", "firebrick3", "firebrick", "firebrick4"), na.value = "white", limits=c(-5,5), labels=c("-4","-2", "0","2","4"), breaks = c(-4,-2,0,2,4), values=rescale(c(-5,-4,-3,-2,-1,0,1,2,3,4,5), from=c(-5,5))) + # rescale doesn't change sth in this case
  facet_grid(.~TP, scales = "free_x", space = "free_x") +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 30, face="bold"),
        axis.text.y = element_text(size = 30, face="bold"),
        strip.text = element_text(size = 45, face="bold", color = "white"),
        strip.background = element_rect(fill = "dodgerblue4"),
        legend.title.align = 0.5,
        legend.title = element_text(size = 35, face="bold"),
        legend.text = element_text(size = 30, face="bold"),
        legend.key.height = unit(3,"line"),
        legend.key.width = unit(2,"line"),
        legend.position = c(.94, .1),
        legend.background = element_rect(fill="mintcream"),
        panel.grid = element_blank())

# Dendrogram 1
p2 <- ggplot(segment(ddata_x)) + 
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend), size=2) + 
  labs(x = "", y = "") + 
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank())


# Dendrogram 2
p3 <- ggplot(segment(ddata_y)) + 
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend), size=2) + 
  coord_flip() + 
  labs(x = "", y = "") + 
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank())

grid.newpage()

combined_plot = ggdraw() +
  draw_plot(p1, 0, 0, .85, .95) +
  draw_plot(p2, .2, .94, .6, .05) +
  draw_plot(p3, .83, 0, .15, .92)

ggsave("/Users/dimitras/Documents/dimitra/Workspace/Luda_heatmaps4poster/plot_results/pdf/FC_PBS_veh_ctrl_plasma_w_dendro-.pdf", combined_plot, height = 60, width = 40, units ="cm")




##################### FC_Untx_plasma.txt #######################

ldata <- read.table("/Users/dimitras/Documents/dimitra/Workspace/Luda_heatmaps4poster/data/paper/FC_Untx_plasma.txt", sep="\t", header = TRUE)

ldata.for_hclust = ldata %>% 
  column_to_rownames("measure") %>% 
  select(Serum_serum:LPS.Zymosan_Plasma.24h) %>%
  as.matrix

dd.col <- as.dendrogram(hclust(dist(1 - cor(ldata.for_hclust))))
col.ord <- order.dendrogram(dd.col)

dd.row <- as.dendrogram(hclust(dist(1 - cor(t(ldata.for_hclust)))))
row.ord <- order.dendrogram(dd.row)

xx_names <- attr(ldata.for_hclust, "dimnames")
df <- as.data.frame(ldata.for_hclust)
colnames(df) <- xx_names[[2]]
df$measure <- xx_names[[1]]
df$measure <- with(df, factor(measure, levels=measure, ordered=TRUE))

mdf <- melt(df, id.vars="measure")

#for cor clust
ddata_y <- dendro_data(dd.row)
ddata_x <- dendro_data(dd.col)

### Set up a blank theme
theme_none <- theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.title.x = element_text(colour=NA),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.line = element_blank()
)

### Create plot components ###    
factors=as.character(ddata_y$labels$label[row.ord])

  p1 <- mdf %>%
    mutate(TP=gsub("^.*_(.*)","\\1",variable),
           TP=ifelse(TP=="serum","Serum",TP),
           TP=ifelse(TP=="Plasma.4h","Plasma 4h",TP),
           TP=ifelse(TP=="Plasma.24h","Plasma 24h",TP),
           TP=factor(TP,levels=c("Serum", "Plasma 4h", "Plasma 24h")),
           condition=gsub("^(.*)_(.*)","\\1",variable),
           condition=ifelse(condition=="Zymosan","Zym",condition),
           condition=ifelse(condition=="LPS.Zymosan","LPS+Zym",condition),
           condition = factor(condition, levels=c("Serum", "PBS", "LPS", "Zym", "LPS+Zym")),
           measure = factor(measure, levels=factors),
           logfc=log(value),
           logfc=replace(logfc, logfc==-Inf, NA)
           # logfc=ifelse(logfc>2.5,2.5,logfc),
           # logfc=ifelse(logfc< -2.5,-2.5,logfc)
    ) %>% 
    ggplot(aes(x = condition, y = measure, group=TP, fill=logfc)) +
    geom_tile(aes(width=0.99, height=0.99)) +
    # geom_text(aes(label=round(logfc,1)), size=10) +
    # scale_fill_gradientn(colours = c("royalblue4", "grey90", "firebrick4"), na.value = "white", limits=c(-3,3), labels=c("<-2","0",">2"), breaks = c(-2,0,2), values=rescale(c(-3,0,3), from=c(-3,3))) +
    scale_fill_gradientn(colours = c("royalblue4", "royalblue","royalblue3", "royalblue2", "royalblue1", "grey90", "firebrick1", "firebrick2", "firebrick3", "firebrick", "firebrick4"), na.value = "white", limits=c(-7.5,7.5), labels=c("-5","-2","0","2","5"), breaks = c(-5,-2,0,2,5), values=rescale(c(-7.5,-5,-3,-2,-1,0,1,2,3,5,7.5), from=c(-7.5,7.5))) +
    facet_grid(.~TP, scales = "free_x", space = "free_x") +
    theme(axis.title.x = element_blank(),axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 30, face="bold"),
          axis.text.y = element_text(size = 30, face="bold"),
          strip.text = element_text(size = 45, face="bold", color = "white"),
          strip.background = element_rect(fill = "dodgerblue4"),
          legend.title.align = 0.5,
          legend.title = element_text(size = 35, face="bold"),
          legend.text = element_text(size = 30, face="bold"),
          legend.key.height = unit(3,"line"),
          legend.key.width = unit(3,"line"),
          legend.position = c(.95, .1),
          legend.background = element_rect(fill="mintcream"),
          panel.grid = element_blank())
  
  # Dendrogram 1
  p2 <- ggplot(segment(ddata_x)) + 
    geom_segment(aes(x=x, y=y, xend=xend, yend=yend), size=2) + 
    labs(x = "", y = "") + 
    theme_minimal() +
    theme(axis.ticks = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_blank())
  
  
  # Dendrogram 2
  p3 <- ggplot(segment(ddata_y)) + 
    geom_segment(aes(x=x, y=y, xend=xend, yend=yend), size=2) + 
    coord_flip() + 
    labs(x = "", y = "") + 
    theme_minimal() +
    theme(axis.ticks = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_blank())
  
  grid.newpage()
  
  combined_plot = ggdraw() +
    draw_plot(p1, 0, 0, .8, .91) +
    draw_plot(p2, .09, .9, .7, .09) +
    draw_plot(p3, .78, 0, .2, .88)
  
  ggsave("/Users/dimitras/Documents/dimitra/Workspace/Luda_heatmaps4poster/plot_results/pdf/FC_Untx_plasma_w_dendro.pdf", combined_plot, height = 60, width = 70, units ="cm")

  
  

  ##################### FC_Untx_plasma.txt --SERUM ONLY-- #######################
  
  ldata <- read.table("/Users/dimitras/Documents/dimitra/Workspace/Luda_heatmaps4poster/data/paper/FC_Untx_plasma-Serum.txt", sep="\t", header = TRUE)
  
  ldata.for_hclust = ldata %>% 
    column_to_rownames("measure") %>% 
    as.matrix
  
  dd.row <- as.dendrogram(hclust(dist(1 - cor(t(ldata.for_hclust)))))
  row.ord <- order.dendrogram(dd.row)
  
  xx_names <- attr(ldata.for_hclust, "dimnames")
  df <- as.data.frame(ldata.for_hclust)
  colnames(df) <- xx_names[[2]]
  df$measure <- xx_names[[1]]
  df$measure <- with(df, factor(measure, levels=measure, ordered=TRUE))
  
  mdf <- melt(df, id.vars="measure")
  
  #for cor clust
  ddata_y <- dendro_data(dd.row)
  
  ### Set up a blank theme
  theme_none <- theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_text(colour=NA),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.line = element_blank()
  )
  
  ### Create plot components ###    
  factors=as.character(ddata_y$labels$label[row.ord])
  
  p1 <- mdf %>%
    mutate(condition=gsub("^(Serum)_([[:digit:]]+)","\\1",variable),
           condition = factor(condition, levels=c("Serum")),
           measure = factor(measure, levels=factors),
           logfc=log(value),
           logfc=replace(logfc, logfc==-Inf, NA)
    ) %>% 
    ggplot(aes(x = condition, y = measure, fill=logfc)) +
    geom_tile() +
    scale_fill_gradientn(colours = c("royalblue4", "royalblue","royalblue3", "royalblue2", "royalblue1", "grey90", "firebrick1", "firebrick2", "firebrick3", "firebrick", "firebrick4"), na.value = "white", limits=c(-7.5,7.5), labels=c("-6","-4","-2","0","2","4","6"), breaks = c(-6,-4,-2,0,2,4,6), values=rescale(c(-7.5,-5,-3,-2,-1,0,1,2,3,5,7.5), from=c(-7.5,7.5))) +
    theme(axis.title.x = element_blank(),axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 30, face="bold"),
          axis.text.y = element_text(size = 30, face="bold"),
          strip.text = element_text(size = 45, face="bold", color = "white"),
          strip.background = element_rect(fill = "dodgerblue4"),
          legend.title.align = 0.5,
          legend.title = element_text(size = 35, face="bold"),
          legend.text = element_text(size = 30, face="bold"),
          legend.key.height = unit(3,"line"),
          legend.key.width = unit(3,"line"),
          legend.position = c(.95, .1),
          legend.background = element_rect(fill="mintcream"),
          panel.grid = element_blank())
  
  # Dendrogram
  p3 <- ggplot(segment(ddata_y)) + 
    geom_segment(aes(x=x, y=y, xend=xend, yend=yend), size=2) + 
    coord_flip() + 
    labs(x = "", y = "") + 
    theme_minimal() +
    theme(axis.ticks = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_blank())
  
  grid.newpage()
  
  combined_plot = ggdraw() +
    draw_plot(p1, 0, 0, .15, 1) +
    draw_plot(p3, .11, 0, .85, 1)
  
  ggsave("/Users/dimitras/Documents/dimitra/Workspace/Luda_heatmaps4poster/plot_results/pdf/FC_Untx_plasma-Serum_w_dendro.pdf", combined_plot, height = 50, width = 80, units ="cm")
  
  
  
#########################################

#https://cran.r-project.org/web/packages/ggdendro/vignettes/ggdendro.html