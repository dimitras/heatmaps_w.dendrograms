library(dplyr)
library(tidyr)
library(tibble)
library(broom)
library(rgl)
library(pca3d)
options(rgl.printRglwidget = TRUE)

#3D PCA - combination of score and loading plot
serum.vs.untxplasma.data = read.table("~/Documents/dimitra/Workspace/Luda_heatmaps4poster/data/PCA/serum.vs.untx.plasma.txt", header = TRUE, check.names=FALSE)

serum.vs.untxplasma.data2 = serum.vs.untxplasma.data[,-1]
pca <- prcomp(serum.vs.untxplasma.data2[,-1], scale.=TRUE)
gr = factor(serum.vs.untxplasma.data2[,1])

# open3d()
#open Xquartz
par3d(family ="sans", cex=1.2)#, planes3d(0, alpha = 0.2))
pca3d(pca, 
      group=gr, 
      components = 1:3,
      show.ellipses=TRUE, 
      ellipse.ci=.9,
      show.plane=TRUE,
      fancy = FALSE, 
      show.labels = rownames(pca$rotation[,0]), 
      show.group.labels = FALSE, 
      show.shapes = TRUE,
      radius = 3,
      #biplot = FALSE, biplot.vars = 17,
      legend="right",
      show.axes = TRUE, 
      axes.color = c("#000000"),
      show.axe.titles = TRUE,
      show.scale = FALSE,
      palette = c("#932891", "#019192")
      )

snapshotPCA3d(file="~/Documents/dimitra/Workspace/Luda_heatmaps4poster/plot_results/serum.vs.untx.plasma.pca3d.jpg")
rgl.postscript("~/Documents/dimitra/Workspace/Luda_heatmaps4poster/plot_results/serum.vs.untx.plasma.pca3d.pdf","pdf")


#3d PCA plot for treatment PBS, LPS, Zym, LPS+Zym for 4h
all.plasma.data = read.table("~/Documents/dimitra/Workspace/Luda_heatmaps4poster/data/PCA/all.plasma_by.treat.tp.txt", header = TRUE, check.names=FALSE )

treat.data = all.plasma.data %>%
  subset(TP==4) %>%
  select(Treatment,`5S.HETE`:`15.oxo.ETE`)

pca.treat <- prcomp(treat.data[,-1], scale.=TRUE)
treat.group = factor(treat.data[,1])

par3d(family ="sans", cex=0.8)
pca3d(pca.treat, 
      group=treat.group, 
      components = 1:3,
      show.ellipses=TRUE,
      ellipse.ci=.7,
      show.plane=TRUE,
      fancy = FALSE, 
      # show.labels = rownames(pca.treat$rotation[,0]),
      show.group.labels = FALSE, 
      show.shapes = TRUE,
      radius = 3,
      legend="right",
      show.axes = TRUE, 
      axes.color = c("#000000"),
      show.axe.titles = TRUE,
      show.scale = FALSE
)
snapshotPCA3d(file="~/Documents/dimitra/Workspace/Luda_heatmaps4poster/plot_results/pca3d.loadingplot.by.treatment.4h_v2.jpg")


#3d PCA plot for treatment PBS, LPS, Zym, LPS+Zym for 24h
treat.data = all.plasma.data %>%
  subset(TP==24) %>%
  select(Treatment,`5S.HETE`:`15.oxo.ETE`)

pca.treat <- prcomp(treat.data[,-1], scale.=TRUE)
treat.group = factor(treat.data[,1])

par3d(family ="sans", cex=0.8)
pca3d(pca.treat, 
      group=treat.group, 
      components = 1:3,
      show.ellipses=TRUE,
      ellipse.ci=.7,
      show.plane=TRUE,
      fancy = FALSE, 
      show.labels = rownames(pca.treat$rotation[,0]),
      show.group.labels = FALSE, 
      show.shapes = TRUE,
      radius = 3,
      legend="right",
      show.axes = TRUE, 
      axes.color = c("#000000"),
      show.axe.titles = TRUE,
      show.scale = FALSE
)
snapshotPCA3d(file="~/Documents/dimitra/Workspace/Luda_heatmaps4poster/plot_results/pca3d.loadingplot.by.treatment.24h_v3.jpg")


####3d PCA plot for TP 0,4,24h, compare kinetics for PBS - NOT USED
tp.data = all.plasma.data %>%
  subset(Treatment=="PBS") %>%
  select(TP:`15.oxo.ETE`)

pca.tp <- prcomp(tp.data[,-1], scale.=TRUE)
tp.group = factor(tp.data[,1])

par3d(family ="sans", cex=1.2)
pca3d(pca.tp, 
      group=tp.group, 
      components = 1:3,
      # show.ellipses=TRUE,
      # ellipse.ci=.7,
      show.plane=TRUE,
      fancy = FALSE, 
      show.labels = rownames(pca.tp$rotation[,0]),
      show.group.labels = FALSE, 
      show.shapes = TRUE,
      radius = 3,
      legend="right",
      show.axes = TRUE, 
      axes.color = c("#000000"),
      show.axe.titles = TRUE,
      show.scale = FALSE
)
snapshotPCA3d(file="~/Documents/dimitra/Workspace/Luda_heatmaps4poster/plot_results/pca3d.loadingplot.by.tp.forPBS_names.jpg")



all.plasma.data = read.table("~/Documents/dimitra/Workspace/Luda_heatmaps4poster/data/PCA/all.plasma_by.treat.txt", header = TRUE, check.names=FALSE )

#3d PCA plot for TP 0,4,24h, compare kinetics for LPS
tp.data = all.plasma.data %>%
  subset(Treatment=="PBS.4hr" | Treatment=="LPS.4hr" | Treatment=="PBS.24hr" | Treatment=="LPS.24hr") %>%
  select(Treatment:`15.oxo.ETE`)

pca.tp <- prcomp(tp.data[,-1], scale.=TRUE)
tp.group = factor(tp.data[,1])

par3d(family ="sans", cex=0.8)
pca3d(pca.tp, 
      group=tp.group,
      components = 1:3,
      # show.ellipses=TRUE,
      # ellipse.ci=.7,
      show.plane=TRUE,
      fancy = FALSE, 
      show.labels = rownames(pca.tp$rotation[,0]),
      show.group.labels = FALSE, 
      show.shapes = TRUE,
      radius = 3,
      legend="right",
      show.axes = TRUE, 
      axes.color = c("#000000"),
      show.axe.titles = TRUE,
      show.scale = FALSE,
      palette = c("firebrick3","brown1", "dodgerblue3", "lightskyblue")
)
snapshotPCA3d(file="~/Documents/dimitra/Workspace/Luda_heatmaps4poster/plot_results/pca3d.loadingplot.by.tp.forLPS.PBS_names.jpg")


#3d PCA plot for TP 0,4,24h, compare kinetics for Zym
tp.data = all.plasma.data %>%
  subset(Treatment=="PBS.4hr" | Treatment=="Zym.4hr" | Treatment=="PBS.24hr" | Treatment=="Zym.24hr") %>%
  select(Treatment:`15.oxo.ETE`)

pca.tp <- prcomp(tp.data[,-1], scale.=TRUE)
tp.group = factor(tp.data[,1])

par3d(family ="sans", cex=0.8)
pca3d(pca.tp, 
      group=tp.group, 
      components = 1:3,
      # show.ellipses=TRUE,
      # ellipse.ci=.7,
      show.plane=TRUE,
      fancy = FALSE, 
      show.labels = rownames(pca.tp$rotation[,0]),
      show.group.labels = FALSE, 
      show.shapes = TRUE,
      radius = 3,
      legend="right",
      show.axes = TRUE, 
      axes.color = c("#000000"),
      show.axe.titles = TRUE,
      show.scale = FALSE,
      palette = c("dodgerblue3", "lightskyblue", "seagreen4","springgreen3")
)
snapshotPCA3d(file="~/Documents/dimitra/Workspace/Luda_heatmaps4poster/plot_results/pca3d.loadingplot.by.tp.forZym.PBS_names.jpg")


#3d PCA plot for TP 0,4,24h, compare kinetics for LPS+Zym
tp.data = all.plasma.data %>%
  subset(Treatment=="PBS.4hr" | Treatment=="LPS+Zym.4hr" | Treatment=="PBS.24hr" | Treatment=="LPS+Zym.24hr") %>%
  select(Treatment:`15.oxo.ETE`)

pca.tp <- prcomp(tp.data[,-1], scale.=TRUE)
tp.group = factor(tp.data[,1])

par3d(family ="sans", cex=0.8)
pca3d(pca.tp, 
      group=tp.group, 
      components = 1:3,
      # show.ellipses=TRUE,
      # ellipse.ci=.7,
      show.plane=TRUE,
      fancy = FALSE, 
      show.labels = rownames(pca.tp$rotation[,0]),
      show.group.labels = FALSE, 
      show.shapes = TRUE,
      radius = 3,
      legend="right",
      show.axes = TRUE, 
      axes.color = c("#000000"),
      show.axe.titles = TRUE,
      show.scale = FALSE,
      palette = c("sienna3","sienna1", "dodgerblue3", "lightskyblue")
)
snapshotPCA3d(file="~/Documents/dimitra/Workspace/Luda_heatmaps4poster/plot_results/pca3d.loadingplot.by.tp.forLPSZym.PBS_names.jpg")



#And more plots...

#Score plot - Ellipses per group
pca3d(pca, 
      group=gr, 
      components = 1:3,
      show.ellipses=TRUE, 
      ellipse.ci=.75,
      show.plane=TRUE,
      # bg = "black",
      fancy = FALSE, 
      show.group.labels = TRUE, 
      legend="right",
      show.axes = TRUE, 
      axes.color = c("#000000"),
      show.axe.titles = TRUE,
      show.scale = FALSE,
      palette = c("#932891", "#019192")
)
snapshotPCA3d(file="~/Documents/dimitra/Workspace/Luda_heatmaps4poster/plot_results/serum.vs.untx.plasma.pca3d_scoreplot.jpg")
rgl.postscript("~/Documents/dimitra/Workspace/Luda_heatmaps4poster/plot_results/serum.vs.untx.plasma.pca3d_scoreplot.pdf","pdf")


#Loading plot
pca3d(pca, 
      group=gr, 
      components = 1:3,
      show.ellipses=FALSE, 
      show.plane=TRUE,
      fancy = FALSE, 
      show.labels = rownames(pca$rotation[,0]),
      biplot = TRUE, biplot.vars = 3,
      show.group.labels = FALSE,
      legend="right",
      show.axes = TRUE, 
      axes.color = c("#000000"),
      show.axe.titles = TRUE,
      show.scale = FALSE,
      palette = c("#932891", "#019192")
)
snapshotPCA3d(file="~/Documents/dimitra/Workspace/Luda_heatmaps4poster/plot_results/serum.vs.untx.plasma.pca3d_loadingplot.jpg")
rgl.postscript("~/Documents/dimitra/Workspace/Luda_heatmaps4poster/plot_results/serum.vs.untx.plasma.pca3d_loadingplot.pdf","pdf")


#Loading plot with selected variables
pca3d(pca, 
      group=gr, 
      components = 1:3,
      show.ellipses=FALSE, 
      show.plane=TRUE,
      fancy = FALSE, 
      # show.labels = rownames(pca$rotation[,0]),
      biplot = TRUE, biplot.vars = 3,
      show.group.labels = FALSE,
      legend="right",
      show.axes = TRUE, 
      axes.color = c("#000000"),
      show.axe.titles = TRUE,
      show.scale = FALSE,
      palette = c("#932891", "#019192")
)
snapshotPCA3d(file="~/Documents/dimitra/Workspace/Luda_heatmaps4poster/plot_results/serum.vs.untx.plasma.pca3d_loadingplot_w.selected.vars.jpg")
rgl.postscript("~/Documents/dimitra/Workspace/Luda_heatmaps4poster/plot_results/serum.vs.untx.plasma.pca3d_loadingplot_w.selected.vars.pdf","pdf")


#Loading plot with cluster centroids
pca3d(pca, 
      group=gr, 
      components = 1:3,
      show.ellipses=FALSE, 
      show.plane=FALSE,
      fancy = FALSE, 
      # show.labels = rownames(pca$rotation[,0]),
      show.centroids = TRUE,
      show.group.labels = FALSE,
      legend="right",
      show.axes = TRUE, 
      axes.color = c("#000000"),
      show.axe.titles = TRUE,
      show.scale = FALSE,
      palette = c("#932891", "#019192")
)
snapshotPCA3d(file="~/Documents/dimitra/Workspace/Luda_heatmaps4poster/plot_results/serum.vs.untx.plasma.pca3d_loadingplot_w.cluster.centroids.jpg")
rgl.postscript("~/Documents/dimitra/Workspace/Luda_heatmaps4poster/plot_results/serum.vs.untx.plasma.pca3d_loadingplot_w.cluster.centroids.pdf","pdf")










#2D
pca2d(pca, group=gr, show.ellipses=TRUE, show.group.labels = TRUE, legend="topleft", show.plane=TRUE)

pca2d(pca, group=gr, components = 1:2, legend="topleft", fancy = TRUE, show.plane=TRUE, biplot = FALSE, biplot.vars = 17)

pca2d(as.matrix(serum.vs.untxplasma.data2[,-1]), group=gr, show.ellipses=TRUE, show.group.labels = TRUE, legend="topleft", 
      show.plane=TRUE)



# open3d() 
# plot3d(pca$sdev[1:3])
# rgl.postscript("~/Documents/dimitra/Workspace/Luda_heatmaps4poster/plot_results/serum.vs.untx.plasma.plot3d.pdf","pdf") 
# png("~/Documents/dimitra/Workspace/Luda_heatmaps4poster/plot_results/serum.vs.untx.plasma.plot3d.png")
# dev.off()












# PCA with ggplot
autoplot(pam(serum.vs.untxplasma.data2[,-1],2), frame = TRUE, frame.type = 'norm')

# serum.vs.untxplasma.data2 = serum.vs.untxplasma.data[,-1]
# pca <- prcomp(serum.vs.untxplasma.data2[,-1], scale.=TRUE)
# analytes = pca$rotation[,0]
# autoplot(pca, data = serum.vs.untxplasma.data2, colour = pca$rotation[,0], label = TRUE, label.size = 7, na.rm = TRUE)


#try to transpose
library(data.table)
dt = serum.vs.untxplasma.data %>%
  gather(Analyte,Volume,X5S.HETE:X15.oxo.ETE) %>%
  select(Analyte,Volume, Material)
autoplot(kmeans(dt, 2), data = dt, label = TRUE, label.size = 5, na.rm = TRUE)










#tryouts

dt = serum.vs.untxplasma.data %>%
  gather(Analyte,Volume,X5S.HETE:X15.oxo.ETE) %>%
  select(Analyte,Volume, Material)
  autoplot(prcomp(~ Volume + Material + Analyte, scale = TRUE))

dt %>%
  ggplot(aes(x=Volume)) +
  # autoplot(prcomp(dt), colour = 'Material', shape = 'Material', frame = TRUE, na.rm = TRUE)
  autoplot(pam(serum.vs.untxplasma.data2[,-1],2), frame = TRUE, frame.type = 'norm', colour = gr, na.rm = TRUE)
  # theme_bw(base_size = 40) + 
  # theme(legend.position = c(0.93,0.95), legend.title = element_text(size = 32),
  #       legend.text = element_text(size = 30), legend.key.height = unit(1,"cm")) +
  # labs(y="Number of genes", x="Log Expression ratio (irradiated/control)")

ggsave("~/Dropbox/workspace/radiation/results/expression_ratios_histograms/all_sense_expression_ratios_comparison_1000bins_wlog_3x3.tiff", height = 80, width = 80, units ="cm")
