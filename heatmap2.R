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

#https://cran.r-project.org/web/packages/ggdendro/vignettes/ggdendro.html