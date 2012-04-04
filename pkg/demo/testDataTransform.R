source("../R/osmarDataTransform.R")
source("../R/mapDataToGraphic.R")
load("../data/home.rda")
load("../data/muc.RData")

# for coords_map()
library(mapproj)
# for tiles
library(ggmap)


# dim(osmar_coords_relations(home))
home <- heidi
home <- muc

ding <- osmar_long(home, node.vars="shop", way.vars=c("highway", "building"))
test <- osmar_long(home, relation.vars="version")
summary(test)

summary(ding)

test <- subset(ding, subset=ding$geom=="point")
test2 <- subset(ding, subset=ding$geom=="line")

head(test)


############# Plots
library(ggplot2)


test3 <- test2
test3 <- cast(test3, ... ~ variable, value="value")
test3 <- test3[order(test3$element_id, test3$order),]
test <- cast(test, element_id + node_id + lat + lon + geom + order ~ variable, value="value")

centerbox
bbox <- center_bbox(11.503819,48.10425, 3000, 3000)
bbox <- home2b
#bbox <- center_bbox(11.549631,48.11481, 3000, 3000) # heidi
layer_empty() + layer_points(test) + layer_lines(test3)

geom <- unique(test3$geom)
map <- do.call(aes_string, list(x="lon", y="lat", colour=NULL, group="element_id", data="test3", size=NULL))
ggplot() + layer(data=test3, geom="path", mapping = map)
ggplot() + layer(data=test3, geom="line", mapping=aes(x="lon", y="lat", data=test3))

p <- ggplot() + #geom_point(aes(y=lat, x=lon),  test, color="grey") +
  xlim(bbox["left"], bbox["right"]) +
  ylim(bbox["bottom"], bbox["top"]) +
  scale_x_continuous(limits=c(bbox["left"], bbox["right"])) +
  scale_y_continuous(limits=c(bbox["bottom"], bbox["top"])) + 
  #stat_density2d(geom="polygon", aes(y=lat, x=lon, alpha= ..level..), test[!is.na(test$shop),]) +
  geom_polygon(aes(y=lat, x=lon, group=element_id), test3[test3$building == "yes",]) +
  geom_path(aes(y=lat, x=lon, group=element_id, color=highway), test3[!is.na(test3$highway),]) +  
  #geom_point(aes(x=lon, y=lat),  test[!is.na(test$shop),]) +
  coord_map()
p
p +   stat_density2d(geom="tile", aes(x=lon, y=lat, fill = ..density..), contour=FALSE, test[which(!is.na(test$shop)),])  +
scale_fill_gradient(limits=c(6e+2,5e+3)) +   geom_point(aes(x=lon, y=lat),  test[which(!is.na(test$shop)),]) 

p <- ggplot() + geom_point(aes(y=lat, x=lon),  test, color="grey") +
  xlim(bbox["left"], bbox["right"]) +
  ylim(bbox["bottom"], bbox["top"]) +
  coord_map()
p

ggplot() + 
  geom_point(aes(y=lat, x=lon, color=shop), test[!is.na(testv$shop),]) +
  geom_path(aes(y=lat, x=lon, group=element_id, color=highway), test3[!is.na(test3$highway),]) + 
  xlim(bbox["left"], bbox["right"]) +
  ylim(bbox["bottom"], bbox["top"]) +
  coord_map()

plot(home)
bbox(home)


system.time(m <- svgmap(home, polygons=list(alpha="version", fill="building")))


garnishAllGrobs <- function(elt) {
  if (inherits(elt, "grob")) {
    garnishGrob(elt,
                onmousemove=paste("showTooltip(evt, '",
                  gsub("\n", " ", elt$name), "')",
                  sep=""),
                onmouseout="hideTooltip()")
  } else {
    elt
  }
}

addTooltips <- function() {
  grid.DLapply(garnishAllGrobs)
  grid.script(filename="tooltip.js")
}

addTooltips()
gridToSVG("tooltips.svg")


library(gridSVG)

dev.off()

svgmap(home, points=list(colour="shop"), lines=list(colour="highway"))
