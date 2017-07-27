library(ade4)
library(dplyr)

## imac
# setwd("~/Documents/dimitra/Workspace/Luda_heatmaps4poster/")
# samples = read.table("data/PCA/all.data.for.amova_samples.txt", header = TRUE, check.names=FALSE )
# groups = read.table("data/PCA/all.data.for.amova_structure.txt", header = TRUE, check.names=FALSE )
# row.names(samples) = samples[,1]
# samples_reformed = samples %>%
#   select(-Subject)
# amovaresults = amova(samples_reformed, distances=NULL, groups)

## pennbox
setwd("~/Box Sync/Luda plots/dev/data/PCA/")
samples = read.table("all.data.for.amova.txt", header = TRUE, check.names=FALSE )
groups = read.table("all.data.for.amova_structure.txt", header = TRUE, check.names=FALSE )
row.names(samples) = samples[,1]
samples_reformed = samples %>%
  select(`5SHETE_PBS4h`:`15oxoETE_LPS.Zym4h`) # all treatments for 4h
# select(`5SHETE_PBS24h`:`15oxoETE_LPS.Zym24h`) # all treatments for 24h
# select(`5SHETE_PBS4h`:`15oxoETE_PBS4h`,`5SHETE_PBS24h`:`15oxoETE_PBS24h`,`5SHETE_LPS4h`:`15oxoETE_LPS4h`, `5SHETE_LPS24h`:`15oxoETE_LPS24h`) # all timepoints for PBS,LPS
# select(`5SHETE_PBS4h`:`15oxoETE_PBS4h`,`5SHETE_PBS24h`:`15oxoETE_PBS24h`,`5SHETE_Zym4h`:`15oxoETE_Zym4h`, `5SHETE_Zym24h`:`15oxoETE_Zym24h`) # all timepoints for PBS,Zym
# select(`5SHETE_PBS4h`:`15oxoETE_PBS4h`,`5SHETE_PBS24h`:`15oxoETE_PBS24h`,`5SHETE_LPS.Zym4h`:`15oxoETE_LPS.Zym4h`, `5SHETE_LPS.Zym24h`:`15oxoETE_LPS.Zym24h`) # all timepoints for PBS,LPS+Zym
groups_reformed = as.data.frame(groups[1:64,]) # all treatments for 4h
# groups_reformed = as.data.frame(groups[65:128,]) # all treatments for 24h
# groups_reformed = as.data.frame(groups[c(1:16,65:80,33:48,97:112),]) # all timepoints for PBS,LPS
# groups_reformed = as.data.frame(groups[c(1:16,65:80,17:32,81:96),]) # all timepoints for PBS,Zym
# groups_reformed = as.data.frame(groups[c(1:16,65:80,49:64,113:128),]) # all timepoints for PBS,LPS+Zym

amovaresults = amova(samples_reformed, distances=NULL, groups_reformed)

# randtest(amovaresults, nrepet = 99)



####################################################################################################################################################


### example found
# library("poppr")
# data("Aeut")
# strata(Aeut) <- data.frame(other(Aeut)$population_hierarchy)
# Aeut <- as.genclone(Aeut)
# Aeut
# table(strata(Aeut, ~Pop))
# table(strata(Aeut, ~Pop/Subpop, combine = FALSE))
# Aeutamova <- poppr.amova(Aeut, ~Pop/Subpop)
# Aeutamovacc <- poppr.amova(Aeut, ~Pop/Subpop, clonecorrect = TRUE)
# Aeutamova
# Aeutamovacc
# write.table(Aeutamova$componentsofcovariance, sep = ",", file = "~/Desktop/AeutiechesAMOVA.csv")
# set.seed(1999)
# Aeutsignif   <- randtest(Aeutamova, nrepet = 999)
# Aeutccsignif <- randtest(Aeutamovacc, nrepet = 999)
# plot(Aeutsignif)
# Aeutsignif
# plot(Aeutccsignif)
# Aeutccsignif






### try with genind object
# df2genind(alldata, sep=".")

# t.data = gsub("_", ".", alldata[-1,1])
# t.data = transpose(alldata)
# df2genind(t.data, sep=".")

# f.alldata = alldata %>%
#   subset(Treatment=="PBS4h" | Treatment=="PBS24h") %>%
#   select(Treatment:`15oxoETE`)

# strata(alldata) <- data.frame(alldata[,1])
# # as.genclone(alldata)
# table(strata(alldata, ~Subject/Treatment, combine = FALSE))
