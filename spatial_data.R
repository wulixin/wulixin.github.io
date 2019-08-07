

suppressPackageStartupMessages(library(dplyr))
library(stars)
# Loading required package: abind
# Loading required package: sf
# Linking to GEOS 3.5.0, GDAL 2.2.2, PROJ 4.8.0
tif = system.file("tif/L7_ETMs.tif", package = "stars")
read_stars(tif) %>%
  slice(index = 1, along = "band") %>%
  plot()


prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
(prec = read_ncdf(prec_file, curvilinear = c("lon", "lat"), ignore_bounds = TRUE))


################
sf::read_sf(system.file("gpkg/nc.gpkg", package = "sf"), "nc.gpkg") %>%
  st_transform(st_crs(prec)) -> nc # transform from NAD27 to WGS84
nc_outline = st_union(st_geometry(nc))
plot_hook = function() plot(nc_outline, border = 'red', add = TRUE)
prec %>%
  slice(index = 1:12, along = "time") %>%
  plot(downsample = c(5, 5, 1), hook = plot_hook)



a = aggregate(prec, by = nc, FUN = max)
# although coordinates are longitude/latitude, st_intersects assumes that they are planar
# although coordinates are longitude/latitude, st_intersects assumes that they are planar
plot(a, max.plot = 23, border = 'grey', lwd = .5)




index_max = function(x) ifelse(all(is.na(x)), NA, which.max(x))
st_apply(a, "geometry", index_max) %>%
  mutate(when = st_get_dimension_values(a, "time")[.$index_max]) %>%
  select(when) %>%
  plot(key.pos = 1, main = "time of maximum precipitation")




library(sf)
library(leaflet)
library(mapview)
library(mapedit)
library(leafpm)
# make a contrived polygon with holes for testing
outer1 = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
outer2 = matrix(c(11,0,11,1,12,1,12,0,11,0),ncol=2, byrow=TRUE)
pts1 = list(outer1, hole1, hole2)
pts2 = list(outer2)
pl1 = st_sf(geom = st_sfc(st_polygon(pts1)))
pl2 = st_sf(geom = st_sfc(st_polygon(pts2)))
mpl = st_sf(geom = st_combine(rbind(pl1, pl2)), crs=4326)
tst = editFeatures(mpl, editor = "leafpm")
# look at our creation
mapview(tst)




library(sp)
grd = SpatialPoints(expand.grid(x=1:100, y=1:100))
gridded(grd) = TRUE
fullgrid(grd) = TRUE
pts = spsample(grd, 50, "random")
pts$z = rnorm(50)
library(gstat)
v = vgm(1, "Sph", 90)
out = krige(z~1, pts, grd, v, nmax = 20, nsim = 4)

## drawing 4 GLS realisations of beta...
## [using conditional Gaussian simulation]

out[[3]] = 0.5 * out[[3]] + 0.5 * rnorm(1e4)
out[[4]] = rnorm(1e4)
spplot(out, as.table = TRUE)



library(tmap)
budapest_df = data.frame(name = "Budapest", x = 19, y = 47.5)
class(budapest_df)
#> [1] "data.frame"
budapest_sf = sf::st_as_sf(budapest_df, coords = c("x", "y"))
class(budapest_sf)
#> [1] "sf"         "data.frame"
tmap_mode("view")
#> tmap mode set to interactive viewing
m = tm_shape(budapest_sf) + tm_dots() + tm_view(basemaps = "OpenStreetMap", 
                                                set.view = 9)
tmap_leaflet(m)




library(tmap)
tm_shape(nz) +
  tm_polygons("Median_income", palette = "RdYlBu")



