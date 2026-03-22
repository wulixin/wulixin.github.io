


# Plot flights to frequent destinations from Callum Prentice's
# global flight data set,
# http://callumprentice.github.io/apps/flight_stream/index.html
data(flights)
head(flights)
# Approximate locations as factors
dest   <- factor(sprintf("%.2f:%.2f",flights[,3], flights[,4]))
# A table of destination frequencies
freq <- sort(table(dest), decreasing=TRUE)
# The most frequent destinations in these data, possibly hub airports?
frequent_destinations <- names(freq)[1:10]
# Subset the flight data by destination frequency
idx <- dest %in% frequent_destinations
frequent_flights <- flights[idx, ]
# Lat/long and counts of frequent flights
ll <- unique(frequent_flights[,3:4])
# Plot frequent destinations as bars, and the flights to and from
# them as arcs. Adjust arc width and color by frequency.
globejs(lat=ll[, 1], long=ll[, 2], arcs=frequent_flights,
        bodycolor="#aaaaff", arcsHeight=0.3, arcsLwd=2,
        arcsColor="#ffff00", arcsOpacity=0.15,
        atmosphere=TRUE, color="#00aaff", pointsize=0.5)

## Not run: 
# Plot populous world cities from the maps package.
library(threejs)
library(maps)
data(world.cities, package="maps")
head(world.cities)
cities <- world.cities[order(world.cities$pop, decreasing=TRUE)[1:1000],]
value  <- 100 * cities$pop / max(cities$pop)
col <- colorRampPalette(c("cyan", "lightgreen"))(10)[floor(10 * value/100) + 1]

globejs(lat=cities$lat, long=cities$long, value=value, color=col, atmosphere=TRUE)

# Plot the data on the moon:
moon <- system.file("images/moon.jpg", package="threejs")
globejs(img=moon, bodycolor="#555555", lightcolor="#aaaaaa",
        lat=cities$lat, long=cities$long,
        value=value, color=col)

# Using global plots from the maptools, rworldmap, or sp packages.

# Instead of using ready-made images of the earth, we can use
# many R spatial imaging packages to produce globe images
# dynamically. With a little extra effort you can build globes with total
# control over how they are plotted.

library(maptools)
library(threejs)
data(wrld_simpl)

bgcolor <- "#000025"
earth <- tempfile(fileext=".jpg")

# NOTE: Use antialiasing to smooth border boundary lines. But! Set the jpeg
# background color to the globe background color to avoid a visible aliasing
# effect at the the plot edges.

jpeg(earth, width=2048, height=1024, quality=100, bg=bgcolor, antialias="default")
par(mar = c(0,0,0,0), pin = c(4,2), pty = "m",  xaxs = "i",
    xaxt = "n",       xpd = FALSE,  yaxs = "i", bty = "n", yaxt = "n")
plot(wrld_simpl, col="black", bg=bgcolor, border="cyan", ann=FALSE,
     setParUsrBB=TRUE)
dev.off()
globejs(earth)





g <- make_full_graph(5) %du% make_full_graph(5) %du% make_full_graph(5)
g <- add_edges(g, c(1,6, 1,11, 6, 11))
fc <- cluster_fast_greedy(g)
membership(fc)
sizes(fc)



x <- rnorm(5)
y <- rnorm(5)
z <- rnorm(5)
scatterplot3js(prices$amount, prices$pct_chg, prices$vol, pch="@", color=rainbow(5)) %>%
  lines3d(c(1, 2), c(3, 4), lwd=2)

data(prices)

# Example 1 from the scatterplot3d package (cf.)
z <- seq(-10, 10, 0.1)
x <- cos(z)
y <- sin(z)
scatterplot3js(x, y, z, color=rainbow(length(z)))

# Same example with explicit axis labels
scatterplot3js(x, y, z, color=rainbow(length(z)), axisLabels=c("a", "b", "c"))

# Same example showing multiple point styles with pch
scatterplot3js(x, y, z, color=rainbow(length(z)),
               pch=sample(c(".", "o", letters), length(x), replace=TRUE))


## Not run: 
# Adding point labels to a scatterplot:
x <- rnorm(5)
y <- rnorm(5)
z <- rnorm(5)
scatterplot3js(x, y, z, pch="o") %>%
  points3d(x + 0.1, y + 0.1, z, color="red", pch=paste("point", 1:5))

# Adding point labels to a graph, obtaining the graph vertex coordinates
# with the `vertices()` function:
data(LeMis)
graphjs(LeMis) %>% 
  points3d(vertices(.), color="red", pch=V(LeMis)$label,labels=V(LeMis)$label)


