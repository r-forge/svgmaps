library(svgmaps)
data("muc")

library(reshape2)
library(grid)


## escape & for xml; has to be moved to  add-interactivity
muc$nodes$tags$v <- as.character(muc$nodes$tags$v)
muc$nodes$tags$v <- gsub("&", "&amp;", muc$nodes$tags$v)

## simple examples without interactivity
svgmap(muc) + igeom_point()
svgmap(muc) + igeom_path()
svgmap(muc) + igeom_polygon()


## drop all rows wich are not interesting
muc_sub <- as_svgmap(muc, c("shop", "name", "highway", "building", "website"))
muc_sub <- muc_sub[-which(is.na(muc_sub$name) & is.na(muc_sub$building)), ]

## example for tooltip
tool <- svgmap(muc, keys = c("name", "building")) +
  igeom_polygon(aes(fill = building, tooltip = name), alpa = 0.1) + iopts(hcolour = "yellow")
save_svgmap(tool, filename = "tooltip.svg")



# build a more complex plot
p2 <- svgmap(muc_sub) +
  igeom_polygon(aes(fill = building, tooltip = building), alpha = 0.5, muc_sub) +
  igeom_point(aes(colour = shop, tooltip = name, link = website, highlight = shop), size = 1.5) +
  igeom_path(aes(tooltip = highway), colour = "grey") 


## view plot in device
print(p2)

## view plot in browser
save_svgmap(p2, filename = "p2.svg")

