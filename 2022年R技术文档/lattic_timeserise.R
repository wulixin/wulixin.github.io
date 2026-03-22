
library(lattice)
library(latticeExtra)

## a Gaussian-like filter (contrast with c = 1 or c = 2)
xyplot(sunspot.year) +
  layer(panel.tskernel(x, y, width = 20, c = 3, col = 1, lwd = 2))

## example from ?kernel:
## long and short moving averages, backwards in time
xyplot(EuStockMarkets[,1]) +
  layer(panel.tskernel(x, y, width = 100, col = 1, sides = 1)) +
  layer(panel.tskernel(x, y, width = 20, col = 2, sides = 1))

## per group, with a triangular filter
xyplot(EuStockMarkets, superpose = TRUE) +
  glayer(panel.tskernel(..., width = 100, c = 2),
         theme = simpleTheme(lwd = 2))
head(EuStockMarkets)
## plot the actual kernels used; note adjustment of width
width = 100
kdat <- lapply(1:4, function(c) {
  k <- kernel("daniell", rep(floor(0.5*width / sqrt(c)), c))
  ## demonstrate that the effective bandwidth stays the same:
  message("c = ", c, ": effective bandwidth = ", bandwidth.kernel(k))
  ## represent the kernel as a time series, for plotting
  ts(k[-k$m:k$m], start = -k$m)
})
names(kdat) <- paste("c =", 1:4)
xyplot(do.call(ts.union, kdat), type = "h",
       scales = list(y = list(relation = "same")))



xyplot(ts(c(1:10,10:1)))

### Figure 14.1 from Sarkar (2008)
xyplot(sunspot.year, aspect = "xy",
       strip = FALSE, strip.left = TRUE,
       cut = list(number = 4, overlap = 0.05))

### A multivariate example; first juxtaposed, then superposed
xyplot(EuStockMarkets, scales = list(y = "same"))
xyplot(EuStockMarkets, superpose = TRUE, aspect = "xy", lwd = 2,
       type = c("l","g"), ylim = c(0, max(EuStockMarkets)))

### Examples using screens (these two are identical)
xyplot(EuStockMarkets, screens = c(rep("Continental", 3), "UK"))
xyplot(EuStockMarkets, screens = list(FTSE = "UK", "Continental"))

### Automatic group styles
xyplot(EuStockMarkets, screens = list(FTSE = "UK", "Continental"),
       superpose = TRUE)

xyplot(EuStockMarkets, screens = list(FTSE = "UK", "Continental"),
       superpose = TRUE, xlim = extendrange(1996:1998),
       par.settings = standard.theme(color = FALSE))

### Specifying styles for series by name
xyplot(EuStockMarkets, screens = list(FTSE = "UK", "Continental"),
       col = list(DAX = "red", FTSE = "blue", "black"), auto.key = TRUE)

xyplot(EuStockMarkets, screens = list(FTSE = "UK", "Continental"),
       col = list(DAX = "red"), lty = list(SMI = 2), lwd = 1:2,
       auto.key = TRUE)

### Example with simpler data, few data points
set.seed(1)
z <- ts(cbind(a = 1:5, b = 11:15, c = 21:25) + rnorm(5))
xyplot(z, screens = 1)
xyplot(z, screens = list(a = "primary (a)", "other (b & c)"),
       type = list(a = c("p", "h"), b = c("p", "s"), "o"),
       pch = list(a = 2, c = 3), auto.key = list(type = "o"))






## generate a random time series object with 12 columns
set.seed(1)
dat <- ts(matrix(cumsum(rnorm(200 * 12)), ncol = 12))
colnames(dat) <- paste("series", LETTERS[1:12])

## show simple line plot first, for reference.
xyplot(dat, scales = list(y = "same"))

## these layers show scale and origin in each panel...
infolayers <-
  layer(panel.scaleArrow(x = 0.99, digits = 1, col = "grey",
                         srt = 90, cex = 0.7)) +
  layer(lim <- current.panel.limits(),
        panel.text(lim$x[1], lim$y[1], round(lim$y[1],1), font = 2,
                   cex = 0.7, adj = c(-0.5,-0.5), col = "#9FC8DC"))

## Case 1: each panel has a different origin and scale:
## ('origin' default is the first data value in each series).
horizonplot(dat, layout = c(1,12), colorkey = TRUE) +
  infolayers

## Case 2: fixed scale but different origin (baseline):
## (similar in concept to scales = "sliced")
horizonplot(dat, layout = c(1,12), horizonscale = 10, colorkey = TRUE) +
  infolayers

## Case 3: fixed scale and constant origin (all same scales):
horizonplot(dat, layout = c(1,12), origin = 0, horizonscale = 10, colorkey = TRUE) +
  infolayers

## same effect using ylim (but colorkey does not know limits):
horizonplot(dat, layout = c(1,12), ylim = c(0, 10), colorkey = TRUE) +
  infolayers

## same scales with full coverage of color scale:
horizonplot(dat, layout = c(1,12), origin = 0,
            scales = list(y = list(relation = "same")),
            colorkey = TRUE, colorkey.digits = 1) +
  infolayers


## use ylab rather than strip.left, for readability.
## also shade any times with missing data values.
horizonplot(dat, horizonscale = 10, colorkey = TRUE,
            layout = c(1,12), strip.left = FALSE,
            ylab = list(rev(colnames(dat)), rot = 0, cex = 0.7)) +
  layer_(panel.fill(col = "gray90"), panel.xblocks(..., col = "white"))


## illustration of the cut points used in the following plot
xyplot(EuStockMarkets, scales = list(y = "same"),
       panel = function(x, y, ...) {
         col <-
           c("#B41414","#E03231","#F7A99C","#9FC8DC","#468CC8","#0165B3")
         for (i in c(-3:-1, 2:0)) {
           if (i >= 0)
             yi <- pmax(4000, pmin(y, 4000 + 1000 * (i+1)))
           if (i < 0)
             yi <- pmin(4000, pmax(y, 4000 + 1000 * i))
           panel.xyarea(x, yi, origin = 4000,
                        col = col[i+4], border = NA)
         }
         panel.lines(x, y)
         panel.abline(h = 4000, lty = 2)
       })

## compare with previous plot
horizonplot(EuStockMarkets, colorkey = TRUE,
            origin = 4000, horizonscale = 1000) +
  infolayers

## a cut-and-stack plot; use constant y scales!
horizonplot(sunspots, cut = list(n = 23, overlap = 0),
            scales = list(draw = FALSE, y = list(relation = "same")),
            origin = 100, colorkey = TRUE,
            strip.left = FALSE, layout = c(1,23)) +
  layer(grid::grid.text(round(x[1]), x = 0, just = "left"))


## a Gaussian-like filter (contrast with c = 1 or c = 2)
xyplot(sunspot.year) +
  layer(panel.tskernel(x, y, width = 20, c = 3, col = 1, lwd = 2))

## example from ?kernel:
## long and short moving averages, backwards in time
xyplot(EuStockMarkets[,1]) +
  layer(panel.tskernel(x, y, width = 100, col = 1, sides = 1)) +
  layer(panel.tskernel(x, y, width = 20, col = 2, sides = 1))

## per group, with a triangular filter
xyplot(EuStockMarkets, superpose = TRUE) +
  glayer(panel.tskernel(..., width = 100, c = 2),
         theme = simpleTheme(lwd = 2))

## plot the actual kernels used; note adjustment of width
width = 100
kdat <- lapply(1:4, function(c) {
  k <- kernel("daniell", rep(floor(0.5*width / sqrt(c)), c))
  ## demonstrate that the effective bandwidth stays the same:
  message("c = ", c, ": effective bandwidth = ", bandwidth.kernel(k))
  ## represent the kernel as a time series, for plotting
  ts(k[-k$m:k$m], start = -k$m)
})
names(kdat) <- paste("c =", 1:4)
xyplot(do.call(ts.union, kdat), type = "h",
       scales = list(y = list(relation = "same")))

