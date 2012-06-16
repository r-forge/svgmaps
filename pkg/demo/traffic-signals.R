
## @knitr build-traffic-signals
library("sp")
library("osmar")
library("svgmaps")
library("plyr")
data("signals", package = "svgmaps")
## to sp-object
class(signals) <- "data.frame"
coordinates(signals) <- c("lon", "lat")


##  Make a grid with counts of traffic signals
bb = bbox(signals)
size <- 0.01
gt = GridTopology(cellcentre.offset = c(bb[1,1],bb[2,1]), 
                  cellsize = c(size, size), 
                  cells.dim = round(c(diff(bb[1,]), diff(bb[2,]))/size) + 1)

sp = SpatialGrid(gt)
## build SpatialPixelsDataFrame
signals_grid = SpatialPixelsDataFrame(sp, data.frame(x = rep(NA, 37*20)))

## Fill with counts data
tab <- data.frame(index = 1:740, count = 0)
count = table(overlay(signals_grid, signals))
tab$count[tab$index %in% names(count)] <- count
tab$index <- NULL
signals_grid@data <- tab
signals_grid@grid.index <- 1:740
signals_grid<- signals_grid


## @knitr traffic-signals
library("svgmaps")
library("sp")
library("osmar")
data("signals", package = "svgmaps")
dim(signals)
svgmap(signals) + igeom_point(size = 0.7)


## @knitr traffic-signals-1
data("signals_grid", package = "svgmaps")
summary(signals_grid)
svgmap(signals_grid) + igeom_tile(aes(fill = count))


## @knitr traffic-signals-2
data("muc_border", package = "svgmaps")
data("isar", package = "svgmaps")

## boundary box of the grid
bb <- slot(signals_grid, "bbox")

p <- svgmap(signals_grid) + 
  ## tiles with signal density
  igeom_tile(aes(fill = count, tooltip = count), col = "gray8")  + 
  ## traffic signals
  igeom_point(data = signals, colour = "orange", size = 0.3, view = 1)   +
  ## munich's border
  igeom_path(data = muc_border, col = "grey29", size = 1, view = 1) + 
  ## the isar (river)
  igeom_path(data = isar, colour = "cyan", size = 0.9, view = 1, alpha = 0.5, tooltip = "I am the Isar", link = "http://en.wikipedia.org/wiki/Isar") + 
  ## further setting for the plot
  scale_fill_gradient(low = "gray8", high = "red") +
  xlim(bb[1, 1], bb[1, 2]) + 
  ylim(bb[2, 1], bb[2, 2]) +
  coord_map() + 
  theme_map() 

save_svgmap(p, file = "traffic-signals.svg")


