###playng around with data
###JRH
#
library(ecodist)






data(iris)
iris.d <- dist(iris[,1:4])
### nmds() is timeconsuming, so this was generated
### in advance and saved.
### set.seed(1234)
### iris.nmds <- nmds(iris.d, nits=20, mindim=1, maxdim=4)
### save(iris.nmds, file="ecodist/data/iris.nmds.rda")
data(iris.nmds)
# examine fit by number of dimensions
plot(iris.nmds)
# choose the best two-dimensional solution to work with
iris.nmin <- min(iris.nmds, dims=2)
# rotate the configuration to maximize variance
iris.rot <- princomp(iris.nmin)$scores
# rotation preserves distance apart in ordination space
cor(dist(iris.nmin), dist(iris.rot))
# fit the data to the ordination as vectors
### vf() is timeconsuming, so this was generated
### in advance and saved.
### set.seed(1234)
### iris.vf <- vf(iris.nmin, iris[,1:4], nperm=1000)
### save(iris.vf, file="ecodist/data/iris.vf.rda")
data(iris.vf)
# repeat for the rotated ordination
### vf() is timeconsuming, so this was generated
### in advance and saved.
### set.seed(1234)
### iris.vfrot <- vf(iris.rot, iris[,1:4], nperm=1000)
### save(iris.vfrot, file="ecodist/data/iris.vfrot.rda")
data(iris.vfrot)
par(mfrow=c(1,2))

plot(iris.nmin, col=as.numeric(iris$Species), pch=as.numeric(iris$Species), main="NMDS")
plot(iris.vf)
plot(iris.rot, col=as.numeric(iris$Species), pch=as.numeric(iris$Species),
     main="Rotated NMDS")
plot(iris.vfrot)
# generate new data points to add to the ordination
# this might be new samples, or a second dataset
iris.new <- structure(list(Sepal.Length = c(4.6, 4.9, 5.4, 5.2, 6, 6.5, 6,
                                            6.8, 7.3), Sepal.Width = c(3.2, 3.5, 3.6, 2.3, 2.8, 3, 2.7, 3.1,
                                                                       3.2), Petal.Length = c(1.2, 1.5, 1.5, 3.5, 4.1, 4.2, 4.8, 5,
                                                                                              5.7), Petal.Width = c(0.26, 0.26, 0.26, 1.2, 1.3, 1.4, 1.8, 2,
                                                                                                                    2), Species = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L), .Label = c("setosa",
                                                                                                                                                                                              "versicolor", "virginica"), class = "factor")), .Names = c("Sepal.Length",
                                                                                                                                                                                                                                                         "Sepal.Width", "Petal.Length", "Petal.Width", "Species"), class = "data.frame",
                      row.names = c(NA, -9L))
# provide a dist object containing original and new data
# provide a logical vector indicating which samples were used to
# construct the original configuration
iris.full <- rbind(iris, iris.new)
all.d <- dist(iris.full[,1:4])
is.orig <- c(rep(TRUE, nrow(iris)), rep(FALSE, nrow(iris.new)))
### addord() is timeconsuming, so this was generated
### in advance and saved.
### set.seed(1234)
### iris.fit <- addord(iris.nmin, iris.full[,1:4], all.d, is.orig, maxit=100)
### save(iris.fit, file="ecodist/data/iris.fit.rda")
data(iris.fit)
plot(iris.fit$conf, col=iris.full$Species, pch=c(18, 4)[is.orig + 1],
     xlab="NMDS 1", ylab="NMDS 2")
title("Demo: adding points to an ordination")
legend("bottomleft", c("Training set", "Added point"), pch=c(4, 18))
legend("topright", levels(iris$Species), fill=1:3)



#Ecodist is pretty cool but not sure how to fit my data into this format
library(vegan)
bugs<-read.csv ('bugs.csv', header=TRUE, sep=",")

fish<-read.csv ('fish.csv', header=TRUE, sep=",")

envinfo <- bugs$WQTYPE

#NMDS

#Screeplost some variation explained by axis

NMDS2 <-metaMDS (bugs[,50:144],k=3)      
plot(NMDS2)

#Stressplot fo NMDS
stressplot(NMDS2)
gof <- goodness(NMDS2)
gof
plot(NMDS2, display = "sites", type = "n")
points(NMDS2, display = "sites", cex = 2*gof/mean(gof))


corrs <- envfit(NMDS2, bugs[,20:37])
#to view
corrs
#need scatterplot
plot(NMDS2, display="sites")



plot(NMDS2, type = "n")
points(NMDS2, display = "sites", cex = 0.8, pch=21, col="red", bg="yellow")
text(NMDS2, display = "spec", cex=0.7, col="blue")




#envirofit function adds continous variable vectors to community
corrs <- envfit(NMDS2, bugs[,20:37])
#to view
corrs

#Then can plot the correlations to NMDS
plot(NMDS2, dis="site")
plot(corrs)


#Basic Plot - cool but I need to see stations by type
plot(NMDS2, display = "sites", type = "n")
points(NMDS2, cex = 1, pch=21, col="green", bg="blue")
text(NMDS2, display = "spec", cex=0.7, col="blue")


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

