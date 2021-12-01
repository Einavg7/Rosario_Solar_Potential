# municipal buildings analysis
## 37 municipal buildings provided by the city

library(dplyr)
library(sf)
library(spatstat)
library(raster)
library(units)
setwd("...")

muni <- read_sf("rosario_municipalb.shp")
muni <- st_transform(muni, "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
ros_b <- read_sf("rosario_b1.shp")
ros_b <- st_transform(ros_b, "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

muni$elec_prod[muni$area <= 30] <- 0 
hist(muni$area)
muni$elec_prod_mwh <- muni$elec_prod/1000
muni$elec_consu_mwh <- muni$elec_consu/1000
muni$rene_elec_mwh <- muni$rene_elec/1000

hist(muni$elec_prod_mwh)
qqnorm(muni$elec_prod_mwh)
summary(muni)

sum(muni$elec_prod_mwh)
boxplot(muni$elec_prod_mwh, horizontal=TRUE, main="Rosario Municipal Buildings", xlab = "Renewable Electricity Production Potential (mWh)")
#boxplot(muni$elec_consu_mwh, horizontal=TRUE, main="Rosario Municipal Buildings", xlab = "Renewable Electricity Production Potential (mWh)")

x <- muni[,13:15]
x <- st_drop_geometry(x)
x <- na.omit(x)
colSums(x)
library(RColorBrewer)
coul <- brewer.pal(3, "Set2")
barplot(colSums(x), col=coul, ylab = "Electricity (mWh)", main = "Rosario Electricity Balance")

options(sf_max.plot=1)
plot(muni["elec_prod_mwh"], key.pos = 1, axes = TRUE, key.width = lcm(1.3), key.length = 1.0)
plot(st_geometry(ros_b), add=T)
x <- muni[,13:15]
summary(x)

osm <- read_sf("rosario_osm.shp")
osm <- st_transform(osm, "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
osm$elec_prod[osm$area <= 30] <- 0
osm_nz <- osm[!(osm$elec_prod==0),]
osm_nz$elec_prod_mwh <- osm_nz$elec_prod/1000
summary(osm_nz$elec_prod_mwh)
boxplot(osm_nz$elec_prod_mwh, horizontal=TRUE, main="Rosario OSM Buildings", xlab = "Renewable Electricity Production Potential (mWh)")
sum(osm_nz$elec_prod_mwh)
