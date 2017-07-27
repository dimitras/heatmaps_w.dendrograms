# library(dplyr)
library(tidyr)
library(PMCMR)
library(FSA)

setwd("~/Box Sync/Luda plots/dev/")
samples = read.table("data/PCA/all.plasma_by.treat.tp.txt", header = TRUE, check.names=FALSE )

samples_reformed = samples %>%
  gather(Analyte,Quantity,`5S.HETE`:`15.oxo.ETE`) #%>%
  # mutate(Group=paste(Subject,Treatment,TP,Analyte, sep="."))

aov_res = aov(Quantity~(Treatment*TP*Analyte)+Error(Subject/Analyte)+(Treatment*TP), samples_reformed)
summary(aov_res)
# ggplot(samples_reformed, aes(x=Analyte, y=Quantity)) +
#   geom_boxplot()
# ggplot(samples_reformed, aes(x=Treatment, y=Quantity)) +  
#   geom_boxplot()

tuk = TukeyHSD(aov_res, Treatment, ordered=TRUE)
#doesn't work for repeated measures

#posthoc
ph = posthoc.kruskal.dunn.test(x=samples_reformed$Quantity, g=samples_reformed$Treatment, p.adjust.method="bonferroni")
#         LPS     LPS+Zym PBS    
# LPS+Zym 0.01112 -       -      
# PBS     1.00000 0.10223 -      
# Zym     2e-06   1.00000 0.00028

ph = posthoc.kruskal.dunn.test(x=samples_reformed$Quantity, g=samples_reformed$TP, p.adjust.method="bonferroni")
#     4     
# 24 <2e-16

#or
aov_res2 = aov(Quantity~(Treatment*TP*Analyte), samples_reformed)
summary(aov_res2)
