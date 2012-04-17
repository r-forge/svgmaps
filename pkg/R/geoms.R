### Redefine geoms ###################################################################
library(proto)
library(ggplot2)
library(grid)
library(gridSVG)


### Test geom_ipoint
test <- data.frame(x=c(1,9,9,2,3,2,3,9,9,8), y=10:1, group=rep(c("es funktioniert", "yeeehaaaa"), times=5))
p <-  ggplot(mapping = aes(x = x, y = y)) + geom_ipoint(aes(tooltip = group), test)
p

test <- nodL
svgmaps() + geom_point2(aes(x = lon, y = lat, colour = value, tooltip = as.character(value)), nodL)
grid.script(filename="../inst/javascript/tooltip2.js")
gridToSVG("test.svg")


ggplot() + geom_ipolygon(aes(x = x, y = y, group = group, tooltip = group, fill = group), test) + geom_ipoint(aes(x = x, y = y, colour = group, tooltip = group), test)
grid.ls()
grob_name <- grid.get('GRID.polyline', grep=TRUE)$name
grob_name
grid.garnish(grob_name, meta= grid.get(grob_name)$gp$meta, onmouseover=rep("showTooltip(evt)", times=150),group=FALSE)
grid.script(filename="../demo/tooltip2.js")
gridToSVG("test.svg")


ding <- geom_point(aes(x = dist, y = speed), cars)
ggplot() + ding
                  )



ggplot() + geom_ipath(aes(x = x, y = y, group = group, colour = group, tooltip = group), test)
