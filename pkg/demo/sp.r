

#################   test SpatialPointsDataFrame
library(sp)
library(reshape)
library(ggplot2)
data(meuse)
meuse2 <- meuse
coordinates(meuse2) <- c("x","y")
class(meuse2)

test <- melt_points(meuse2, c("elev", "cadmium"))
head(test)

class(meuse)
ggplot() + geom_map(map=meuse2, aes(colour="cadmium"))


################  test SpatialLinesDataFrame

# build up SpatialLinesDataFrame
l1 = cbind(c(1,2,3),c(3,2,2))
l1a = cbind(l1[,1]+.05,l1[,2]+.05)
l2 = cbind(c(1,2,3),c(1,1.5,1))
Sl1 = Line(l1)
Sl1a = Line(l1a)
Sl2 = Line(l2)
S1 = Lines(list(Sl1, Sl1a), ID="a")
S2 = Lines(list(Sl2), ID="b")
S3 = Lines(list(Line(cbind(c(3,4,5), c(2,3,9)))), ID="c")
Sl = SpatialLines(list(S2,S1, S3))
summary(Sl)

df = data.frame(z = c(1,2,3), row.names=sapply(slot(Sl, "lines"), function(x) slot(x, "ID")))
Sldf = SpatialLinesDataFrame(Sl, data = df)
summary(Sldf)

ggplot() + geom_map(map=Sldf)
# coordinates
coordinates(Sldf)

library(plyr)
library(reshape)
library(maptools)
install.packages("rgeos")
library(rgeos)
library(ggplot2)
system.time(d <- melt_lines(Sldf, "z"))
(d)
d2 <- fortify(Sldf)
d2


###############    SpatialPolygonsDataFrame


grd <- GridTopology(c(1,1), c(1,1), c(10,10))
polys <- as.SpatialPolygons.GridTopology(grd)
centroids <- coordinates(polys)
x <- centroids[,1]
y <- centroids[,2]
z <- 1.4 + 0.1*x + 0.2*y + 0.002*x*x
#ex_1.7 <- SpatialPolygonsDataFrame(polys, data=data.frame(x=x, y=y, z=z, row.names=sapply(slot(polys, "polygons"), function(i) slot(i, "ID"))))
polyDat <- SpatialPolygonsDataFrame(polys, data=data.frame(x=x, y=y, z=z, row.names=row.names(polys)))
polyDat

gpclibPermit()
test <- melt_polygons(polyDat, "z")
con <- url("http://www.filefactory.com/file/c2a3543/n/DEU_adm3.RData")
print(load(con))
close(con)

fortify(polyDat)
library('maptools')
gpclibPermit()
system.time(g2 <- fortify(gadm))
system.time(gadm_long <- melt_polygons(gadm, vars="NAME_3"))
names(gadm_long)
ggplot(gadm_long) + geom_polygon(aes(x=lon, y=lat, group=element_id, fill=value), gadm_long) + coord_map()

svgmap(gadm, polygon=list(fill="ENGTYPE_3"))



############## SpatialPixelsDataFrame
library(sp)

plot(meuse)
ggplot() + geom_tile(aes(x=x, y=y, fill=dist), as.data.frame(meuse.grid))+ scale_fill_gradientn("My variable",
colours=terrain.colors(255))
names(meuse)

data(meuse.grid)
gridded(meuse.grid) <- ~x+y
ggplot(data=as.data.frame(meuse.grid), aes(x=x, y=y)) +
geom_tile(aes(fill=dist)) + scale_fill_gradientn("My variable",
colours=terrain.colors(255)) + coord_equal()

svgmap(meuse.grid, grid=list(fill=c("ffreq")))
svgmap(meuse, points=list(colour="om"))

names(meuse.grid)
