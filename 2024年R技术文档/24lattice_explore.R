
### R code from vignette source 'grid.Rnw'

###################################################
### code chunk number 1: grid.Rnw:33-34
###################################################
library(grid)


###################################################
### code chunk number 2: grid.Rnw:43-44
###################################################
library(lattice)


###################################################
### code chunk number 3: trellisdata (eval = FALSE)
###################################################
## x <- rnorm(100)
## y <- rnorm(100)
## g <- sample(1:8, 100, replace = TRUE)


###################################################
### code chunk number 4: trellispanelplot (eval = FALSE)
###################################################
## xyplot(y ~ x | g, panel = function(x, y) {
##     panel.xyplot(x, y);
##     grid.lines(unit(c(0, 1), "npc"), unit(0, "native"),
##                gp = gpar(col = "grey"))
## })


###################################################
### code chunk number 5: trellispanel
###################################################
x <- rnorm(100)
y <- rnorm(100)
g <- sample(1:8, 100, replace = TRUE)
xyplot(y ~ x | g, panel = function(x, y) {
  panel.xyplot(x, y);
  grid.lines(unit(c(0, 1), "npc"), unit(0, "native"),
             gp = gpar(col = "grey"))
})


###################################################
### code chunk number 6: trellisstripplot (eval = FALSE)
###################################################
## xyplot(y ~ x | g, strip = function(which.given, which.panel, ...) {
##     grid.rect()
##     grid.text(paste("Variable ", which.given, ": Level ",
##                     which.panel[which.given], sep = ""),
##               unit(1, "mm"), .5, just = "left")
## })


###################################################
### code chunk number 7: trellisstrip
###################################################
x <- rnorm(100)
y <- rnorm(100)
g <- sample(1:8, 100, replace = TRUE)
xyplot(y ~ x | g, strip = function(which.given, which.panel, ...) {
  grid.rect()
  grid.text(paste("Variable ", which.given, ": Level ",
                  which.panel[which.given], sep = ""),
            unit(1, "mm"), .5, just = "left")
})


###################################################
### code chunk number 8: trellisgridplot (eval = FALSE)
###################################################
## someText <- paste("A panel of text", "produced using", "raw grid code",
##                   "that could be used", "to describe",
##                   "the plot", "to the right.", sep = "\n")
## latticePlot <- xyplot(y ~ x | g, layout = c(2, 4))
## grid.rect(gp = gpar(lty = "dashed"))
## pushViewport(viewport(layout = grid.layout(1, 2,
##                       widths = unit.c(unit(1, "strwidth", someText) +
##                       unit(2, "cm"),
##                       unit(1, "null")))))
## pushViewport(viewport(layout.pos.col = 1))
## grid.rect(gp = gpar(fill = "light grey"))
## grid.text(someText,
##           x = unit(1, "cm"), y = unit(1, "npc") - unit(1, "inches"),
##           just = c("left", "top"))
## popViewport()
## pushViewport(viewport(layout.pos.col = 2))
## print(latticePlot, newpage = FALSE)
## popViewport(2)


###################################################
### code chunk number 9: trellisgrid
###################################################
x <- rnorm(100)
y <- rnorm(100)
g <- sample(1:8, 100, replace = TRUE)
someText <- paste("A panel of text", "produced using", "raw grid code",
                  "that could be used", "to describe",
                  "the plot", "to the right.", sep = "\n")

latticePlot <- xyplot(y ~ x | g, layout = c(2, 4))
grid.rect(gp = gpar(lty = "dashed"))
pushViewport(viewport(layout = grid.layout(1, 2,
                                           widths = unit.c(unit(1, "strwidth", someText) +
                                                             unit(2, "cm"),
                                                           unit(1, "null")))))
pushViewport(viewport(layout.pos.col = 1))
grid.rect(gp = gpar(fill = "light grey"))
grid.text(someText,
          x = unit(1, "cm"), y = unit(1, "npc") - unit(1, "inches"),
          just = c("left", "top"))
popViewport()
pushViewport(viewport(layout.pos.col = 2))
print(latticePlot, newpage = FALSE)
popViewport(2)

##
##
##install.packages(c("latticeExtra", "copula", "ellipse", "gridBase",
"locfit", "logspline", "mapproj", "maps", "MEMSS",
"mlmRev", "RColorBrewer"))
#source("http://bioconductor.org/biocLite.R")
#biocLite(c("flowCore", "flowViz", "hexbin"))


require(stats)
histogram( ~ height | voice.part, data = singer, nint = 17,
           endpoints = c(59.5, 76.5), layout = c(2,4), aspect = 1,
           xlab = "Height (inches)")

histogram( ~ height | voice.part, data = singer,
           xlab = "Height (inches)", type = "density",
           panel = function(x, ...) {
             panel.histogram(x, ...)
             panel.mathdensity(dmath = dnorm, col = "black",
                               args = list(mean=mean(x),sd=sd(x)))
           } )

densityplot( ~ height | voice.part, data = singer, layout = c(2, 4),  
             xlab = "Height (inches)", bw = 5)



barchart:
  Bar plots.

bwplot:
  Box-and-whisker plots.

densityplot:
  Kernel density estimates.

dotplot:
  Cleveland dot plots.

histogram:
  Histograms.

qqmath:
  Theretical quantile plots.

stripplot:
  One-dimensional scatterplots.

Bivariate:
  
  qq:
  Quantile plots for comparing two distributions.

xyplot:
  Scatterplots and time-series plots (and potentially a lot more).

Trivariate:
  
  levelplot:
  Level plots (similar to image plots).

contourplot:
  Contour plots.

cloud:
  Three-dimensional scatter plots.

wireframe:
  Three-dimensional surface plots (similar to persp plots).

Hypervariate:
  
  splom:
  Scatterplot matrices.

parallel:
  Parallel coordinate plots.

Miscellaneous:
  
  rfs:
  Residual and fitted value plots (also see oneway).

tmd:
  Tukey Mean-Difference plots.

RShowDoc("NEWS", package = "lattice")


