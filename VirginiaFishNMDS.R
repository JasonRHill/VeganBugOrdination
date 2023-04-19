###Playng around with ref fishdata
###JRH Feb 21, 2019

#R version 3.4.3 (2017-11-30) -- "Kite-Eating Tree"
#Copyright (C) 2017 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)

#Using Vegan Package for NMDS
#Loading required package: permute
#Loading required package: lattice
#This is vegan 2.4-6

library(vegan)

fish.species.all<-read.csv ('fish.species.all.csv', header=TRUE, sep=",")
fish.species.norare<-read.csv ('fish.species.norare.csv', header=TRUE, sep=",")
fish.enviro<-read.csv ('fish.enviro.csv', header=TRUE, sep=",")
bugs2<-read.csv('bugs.csv', header = TRUE, sep=",")

#NMDS Bug
NMDSbug <-metaMDS (bugs[,50:144],k=3) 

#NMDS

NMDS <-metaMDS (fish.species.all[,2:131],k=2)      
plot(NMDS)

NMDS2 <-metaMDS (fish.species.norare[,2:89],k=2)      
plot(NMDS2)

#Stressplot fo NMDS
stressplot(NMDS)
gof <- goodness(NMDS)
gof
plot(NMDS, display = "sites", type = "n")
points(NMDS, display = "sites", cex = 2*gof/mean(gof))


#plot (all) with species
plot(NMDS, type = "n")
points(NMDS, display = "sites", cex = 0.8, pch=21, col="red", bg="yellow")
text(NMDS, display = "spec", cex=0.7, col="blue")

#plot (all.no rare) with species
plot(NMDS2, type = "n")
points(NMDS2, display = "sites", cex = 0.8, pch=21, col="red", bg="yellow")
text(NMDS2, display = "spec", cex=0.7, col="blue")



#plot (all) with species by BioRegion
with(fish.enviro, levels(BioRegion))
scl <- 3 ## scaling = 3
colvec <- c("red2", "green4", "mediumblue")
plot(NMDS, type = "n", scaling = scl)
with(fish.enviro, points(NMDS, display = "sites", col = colvec[BioRegion],
                  scaling = scl, pch = 21, bg = colvec[BioRegion]))
with(fish.enviro, legend("topright", legend = levels(BioRegion), bty = "n",
                  col = colvec, pch = 21, pt.bg = colvec))

#plot (norare fish) with species by BioRegion
with(fish.enviro, levels(BioRegion))
scl <- 3 ## scaling = 3
colvec <- c("red2", "green4", "mediumblue")
plot(NMDS2, type = "n", scaling = scl)
with(fish.enviro, points(NMDS2, display = "sites", col = colvec[BioRegion],
                         scaling = scl, pch = 21, bg = colvec[BioRegion]))
with(fish.enviro, legend("topright", legend = levels(BioRegion), bty = "n",
                         col = colvec, pch = 21, pt.bg = colvec))


#plot (all) with species by Major Drainage
with(fish.enviro, levels(Drainage))
scl <- 2 ## scaling = 2
colvec <- c("red2", "green4")
plot(NMDS, type = "n", scaling = scl)
with(fish.enviro, points(NMDS, display = "sites", col = colvec[Drainage],
                         scaling = scl, pch = 21, bg = colvec[Drainage]))
with(fish.enviro, legend("topright", legend = levels(Drainage), bty = "n",
                         col = colvec, pch = 21, pt.bg = colvec))

#plot (norare fish) with species by Major Drainage
with(fish.enviro, levels(Drainage))
scl <- 2 ## scaling = 2
colvec <- c("red2", "green4")
plot(NMDS2, type = "n", scaling = scl)
with(fish.enviro, points(NMDS2, display = "sites", col = colvec[Drainage],
                         scaling = scl, pch = 21, bg = colvec[Drainage]))
with(fish.enviro, legend("topright", legend = levels(Drainage), bty = "n",
                         col = colvec, pch = 21, pt.bg = colvec))

#plot (all) with species by VDGIF Temp Class
with(fish.enviro, levels(TempClass))
scl <- 3 ## scaling = 3
colvec <- c("red2", "green4", "mediumblue")
plot(NMDS, type = "n", scaling = scl)
with(fish.enviro, points(NMDS, display = "sites", col = colvec[TempClass],
                         scaling = scl, pch = 21, bg = colvec[TempClass]))
with(fish.enviro, legend("topright", legend = levels(TempClass), bty = "n",
                         col = colvec, pch = 21, pt.bg = colvec))


#plot (norare) with species by VDGIF Temp Class
with(fish.enviro, levels(TempClass))
scl <- 3 ## scaling = 3
colvec <- c("red2", "green4", "mediumblue")
plot(NMDS2, type = "n", scaling = scl)
with(fish.enviro, points(NMDS2, display = "sites", col = colvec[TempClass],
                         scaling = scl, pch = 21, bg = colvec[TempClass]))
with(fish.enviro, legend("topright", legend = levels(TempClass), bty = "n",
                         col = colvec, pch = 21, pt.bg = colvec))


#plot (all) with species by Stream Size
with(fish.enviro, levels(FishStreamSizeCat))
scl <- 4 ## scaling = 4
colvec <- c("red2", "green4", "mediumblue", "yellow")
plot(NMDS, type = "n", scaling = scl)
with(fish.enviro, points(NMDS, display = "sites", col = colvec[FishStreamSizeCat],
                         scaling = scl, pch = 21, bg = colvec[FishStreamSizeCat]))
with(fish.enviro, legend("topright", legend = levels(FishStreamSizeCat), bty = "n",
                         col = colvec, pch = 21, pt.bg = colvec))


#plot (all) with species by Stream Size
with(fish.enviro, levels(FishStreamSizeCat))
scl <- 4 ## scaling = 4
colvec <- c("red2", "green4", "mediumblue", "yellow")
plot(NMDS2, type = "n", scaling = scl)
with(fish.enviro, points(NMDS2, display = "sites", col = colvec[FishStreamSizeCat],
                         scaling = scl, pch = 21, bg = colvec[FishStreamSizeCat]))
with(fish.enviro, legend("topright", legend = levels(FishStreamSizeCat), bty = "n",
                         col = colvec, pch = 21, pt.bg = colvec))



#plot (all) with species by Stream Size
##Should have done this first - basin most important
with(fish.enviro, levels(Basin))
scl <- 7 ## scaling = 7
colvec <- c("red2", "green4", "mediumblue", "yellow", "black", "purple", "orange")
plot(NMDS, type = "n", scaling = scl)
with(fish.enviro, points(NMDS, display = "sites", col = colvec[Basin],
                         scaling = scl, pch = 21, bg = colvec[Basin]))
with(fish.enviro, legend("topright", legend = levels(Basin), bty = "n",
                         col = colvec, pch = 21, pt.bg = colvec))


#plot (norare fish) with species by Stream Size
##Should have done this first - basin most important
with(fish.enviro, levels(Basin))
scl <- 7 ## scaling = 7
colvec <- c("red2", "green4", "mediumblue", "yellow", "black", "purple", "orange")
plot(NMDS2, type = "n", scaling = scl)
with(fish.enviro, points(NMDS2, display = "sites", col = colvec[Basin],
                         scaling = scl, pch = 21, bg = colvec[Basin]))
with(fish.enviro, legend("topright", legend = levels(Basin), bty = "n",
                         col = colvec, pch = 21, pt.bg = colvec))


#plot (all) with species by Stream Size
##Should have done this first - basin most important
with(fish.enviro, levels(SubBasin))
scl <- 12 ## scaling = 12
colvec <- c("red2", "green4", "mediumblue", "yellow", "black", 
            "purple", "orange", "brown", "pink", "darkcyan", "grey", "lightgreen")
plot(NMDS, type = "n", scaling = scl)
with(fish.enviro, points(NMDS, display = "sites", col = colvec[SubBasin],
                         scaling = scl, pch = 21, bg = colvec[SubBasin]))
with(fish.enviro, legend("topright", legend = levels(SubBasin), bty = "n",
                         col = colvec, pch = 21, pt.bg = colvec))


#Can I view by other catogory varible?
###Yes, I need to add levels
with(bugs, levels(WQTYPE))
scl <- 5 ## scaling = 5
colvec <- c("red2", "green4", "mediumblue", "black", "yellow")

plot(NMDS2, type = "n", scaling = scl)
with(bugs, points(NMDS2, display = "sites", col = colvec[WQTYPE],
                      scaling = scl, pch = 21, bg = colvec[WQTYPE]))

text(NMDS2, display = "species", scaling = scl, cex = 0.8, col = "darkcyan")

with(bugs, legend("topright", legend = levels(WQTYPE), bty = "n",
                      col = colvec, pch = 21, pt.bg = colvec))

#head(with(bugs, colvec[WQTYPE]))

attach(bugs)

#not sure what this be...but i like it!
plot(NMDS2, disp="sites", type="n")
ordihull(NMDS2, WQTYPE, col=1:4, lwd=3)
ordiellipse(NMDS2, WQTYPE, col=1:4, kind = "ehull", lwd=3)
ordiellipse(NMDS2, WQTYPE, col=1:4, draw="polygon")
ordispider(NMDS2, WQTYPE, col=1:4, label = TRUE)
points(NMDS2, disp="sites", pch=21, col="red", bg="yellow", cex=1.3)


###This is cool! 
ord <- metaMDS(dune)
data(dune)
data(dune.env)
attach(dune.env)
plot(ord, disp="sites", type="n")
ordihull(ord, Management, col=1:4, lwd=3)
ordiellipse(ord, Management, col=1:4, kind = "ehull", lwd=3)
ordiellipse(ord, Management, col=1:4, draw="polygon")
ordispider(ord, Management, col=1:4, label = TRUE)
points(ord, disp="sites", pch=21, col="red", bg="yellow", cex=1.3)

#envirofit function adds continous variable vectors to community
corrs <- envfit(NMDSbug, bugs[,20:37])
#to view
corrs

#Then can plot the correlations to NMDS
plot(NMDSbug, dis="site")
plot(corrs)
text(NMDSbug, display = "species", scaling = scl, cex = 0.8, col = "darkcyan")

