load("../data/muc.RData")


library(svgmaps)

library(reshape2)
library(grid)

## for later plotting add keys to attrs dataframe
muc <- add_keys(muc, c("shop", "name", "highway", "building"))

## escape & for xml; has to be moved to  add-interactivity
muc$nodes$attrs$name <- as.character(muc$nodes$attrs$name)
muc$nodes$attrs$name <- gsub("&", "&amp;", muc$nodes$attrs$name)

## examples without interactivity
svgmaps(muc) + igeom_point()
svgmaps(muc) + igeom_path()
svgmaps(muc) + igeom_polygon()


## drop all rows wich are not interesting
muc_sub <- as_svgmaps(muc)
muc_sub <- muc_sub[-which(is.na(muc_sub$name) & is.na(muc_sub$building)), ]

## example for tooltip
tool <- svgmaps(muc) + igeom_polygon(aes(fill = building, tooltip = name), alpa = 0.1)
view_svg(tool)

## example for links
p <- svgmaps(muc_sub) + igeom_point(aes(colour = shop), tooltip = "click on me",  link = "http://www.r-bloggers.com/")
view_svg(p)



# build a more complex plot
p <- svgmaps(muc_sub) +
  igeom_polygon(aes(fill = building, tooltip = building), alpha = 0.5, muc) +
  igeom_point(aes(colour = shop, tooltip = name), size = 1.5) +
  igeom_path(aes(tooltip = highway), colour = "grey") +
  opts(legend.position = "none") +
  xlim(11.573, 11.578) +
  ylim(48.136, 48.139)
       

## view plot in device
print(p2)

## view plot in browser
view_svg(p2)

