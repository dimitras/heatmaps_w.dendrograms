library(mixOmics)
library(dplyr)
library(pca3d)

setwd("~/Box Sync/Luda plots/dev/")
samples = read.table("data/PCA/all.plasma_by.treat.tp.txt", header = TRUE, check.names=FALSE )

mypalette = c("#F09F11","#356584","#1D5C47","#FEF643")
pch.shapes = c(16,17,15,18)

#per treatment at 4h
X = samples %>%
  subset(TP==4) %>%
  select(`5S.HETE`:`15.oxo.ETE`)

Y = samples %>%
  subset(TP==4) %>%
  select(Treatment)
Y = as.factor(Y[,])

subjects = samples %>%
  subset(TP==4) %>%
  select(Subject)
subjects = as.factor(subjects[,])

plsda.samples <- plsda(X, Y, ncomp = 2)

jpeg("plot_results/plsda/plsda.4h.jpg")
plotIndiv(plsda.samples, ind.names = subjects, ellipse = TRUE, legend = TRUE, col = mypalette, 
          title = "PLS-DA: All treatments at 4h")
dev.off()


#per treatment at 24h
X = samples %>%
  subset(TP==24) %>%
  select(`5S.HETE`:`15.oxo.ETE`)

Y = samples %>%
  subset(TP==24) %>%
  select(Treatment)
Y = as.factor(Y[,])

subjects = samples %>%
  subset(TP==24) %>%
  select(Subject)
subjects = as.factor(subjects[,])

plsda.samples <- plsda(X, Y, ncomp = 2)

jpeg("plot_results/plsda/plsda.24h.jpg")
plotIndiv(plsda.samples, ind.names = subjects, ellipse = TRUE, legend = TRUE, col = mypalette,
          title = "PLS-DA: All treatments at 24h")
dev.off()



# 3D - per treatment at 4h
X = samples %>%
  subset(TP==4) %>%
  select(`5S.HETE`:`15.oxo.ETE`)

Y = samples %>%
  subset(TP==4) %>%
  select(Treatment)
Y = as.factor(Y[,])

subjects = samples %>%
  subset(TP==4) %>%
  select(Subject)
subjects = as.factor(subjects[,])

plsda.samples <- plsda(X, Y, ncomp = 3)

plotIndiv(plsda.samples, ind.names = subjects, ellipse = TRUE, legend = TRUE, col = mypalette, 
          title = "PLS-DA: All treatments at 4h", style = '3d', axes.box = 'bbox')
snapshotPCA3d(file="plot_results/plsda/plsda3d.4h.jpg")


# 3D - per treatment at 24h
X = samples %>%
  subset(TP==24) %>%
  select(`5S.HETE`:`15.oxo.ETE`)

Y = samples %>%
  subset(TP==24) %>%
  select(Treatment)
Y = as.factor(Y[,])

subjects = samples %>%
  subset(TP==24) %>%
  select(Subject)
subjects = as.factor(subjects[,])

plsda.samples <- plsda(X, Y, ncomp = 3)

plotIndiv(plsda.samples, ind.names = subjects, ellipse = TRUE, legend = TRUE, col = mypalette, 
          title = "PLS-DA: All treatments at 24h", style = '3d', axes.box = 'bbox')
snapshotPCA3d(file="plot_results/plsda/plsda3d.24h.jpg")








#for TP 0,4,24h, compare kinetics for LPS
mypalette = c("firebrick3","brown1", "dodgerblue3", "lightskyblue")
pch.shapes = c(16,17,15,18)

samples.mod = samples %>%
  subset(Treatment=="PBS"|Treatment=="LPS") %>%
  mutate(Condition=paste(Treatment, TP, sep="."))

X = select(samples.mod, `5S.HETE`:`15.oxo.ETE`)

Y = select(samples.mod, Condition)
Y = as.factor(Y[,])

subjects = samples.mod %>%
  select(Subject)
subjects = as.factor(subjects[,])

plsda.samples <- plsda(X, Y, ncomp = 2)
jpeg("plot_results/plsda/plsda.LPS.jpg")
plotIndiv(plsda.samples, ind.names = subjects, ellipse = TRUE, legend = TRUE, col = mypalette, 
          title = "PLS-DA: LPS vs PBS")
dev.off()
#OR
plsda.samples <- plsda(X, Y, ncomp = 3)
plotIndiv(plsda.samples, ind.names = subjects, ellipse = TRUE, legend = TRUE, col = mypalette, 
          title = "PLS-DA: LPS vs PBS", style = '3d', axes.box = 'bbox')
snapshotPCA3d(file="plot_results/plsda/plsda3d.LPS.jpg")



#for TP 0,4,24h, compare kinetics for Zym
mypalette = c("dodgerblue3", "lightskyblue", "seagreen4","springgreen3")
pch.shapes = c(16,17,15,18)

samples.mod = samples %>%
  subset(Treatment=="PBS"|Treatment=="Zym") %>%
  mutate(Condition=paste(Treatment, TP, sep="."))

X = select(samples.mod, `5S.HETE`:`15.oxo.ETE`)

Y = select(samples.mod, Condition)
Y = as.factor(Y[,])

subjects = samples.mod %>%
  select(Subject)
subjects = as.factor(subjects[,])

plsda.samples <- plsda(X, Y, ncomp = 2)
jpeg("plot_results/plsda/plsda.Zym.jpg")
plotIndiv(plsda.samples, ind.names = subjects, ellipse = TRUE, legend = TRUE, col = mypalette, 
          title = "PLS-DA: Zym vs PBS")
dev.off()
#OR
plsda.samples <- plsda(X, Y, ncomp = 3)
plotIndiv(plsda.samples, ind.names = subjects, ellipse = TRUE, legend = TRUE, col = mypalette, 
          title = "PLS-DA: Zym vs PBS", style = '3d', axes.box = 'bbox')
snapshotPCA3d(file="plot_results/plsda/plsda3d.Zym.jpg")



#for TP 0,4,24h, compare kinetics for LPS+Zym
mypalette = c("sienna3","sienna1", "dodgerblue3", "lightskyblue")
pch.shapes = c(16,17,15,18)

samples.mod = samples %>%
  subset(Treatment=="PBS"|Treatment=="LPS+Zym") %>%
  mutate(Condition=paste(Treatment, TP, sep="."))

X = select(samples.mod, `5S.HETE`:`15.oxo.ETE`)

Y = select(samples.mod, Condition)
Y = as.factor(Y[,])

subjects = samples.mod %>%
  select(Subject)
subjects = as.factor(subjects[,])

plsda.samples <- plsda(X, Y, ncomp = 2)
jpeg("plot_results/plsda/plsda.LPSZym.jpg")
plotIndiv(plsda.samples, ind.names = subjects, ellipse = TRUE, legend = TRUE, col = mypalette, 
          title = "PLS-DA: LPS+Zym vs PBS")
dev.off()
#OR
plsda.samples <- plsda(X, Y, ncomp = 3)
plotIndiv(plsda.samples, ind.names = subjects, ellipse = TRUE, legend = TRUE, col = mypalette, 
          title = "PLS-DA: LPS+Zym vs PBS", style = '3d', axes.box = 'bbox')
snapshotPCA3d(file="plot_results/plsda/plsda3d.LPSZym.jpg")
