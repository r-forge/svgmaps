
## @knitr simple-examples
library("svgmaps")
data("muc", package = "svgmaps")
svgmap(muc) + igeom_point()
svgmap(muc) + igeom_path()
svgmap(muc) + igeom_polygon()


## @knitr static-examples
muc_sub <- as_svgmap(muc, c("shop", "name", "highway", "website", "building"))
muc_sub <- muc_sub[-which(is.na(muc_sub$name) & 
                          is.na(muc_sub$building)), ]
with(muc_sub, table(shop[geom == "point"], useNA = "always"))
muc_inner <- svgmap(muc_sub) + 
  igeom_path(alpha = 0.1) + 
  igeom_polygon(alpha = 0.1) + 
  xlim(11.573, 11.5773) + 
  ylim(48.136, 48.1385) 

shops <- muc_inner+ igeom_point(aes(colour = shop)) 
shops


## @knitr directlabels
library(directlabels)
direct.label(shops, extreme.grid)


## @knitr interactive-examples
muc_sub$tip <- paste(muc_sub$name, " (shop: ", muc_sub$shop, ")", sep = "")
ishops<- muc_inner +  
  igeom_point(aes(tooltip = tip, col = shop), data = muc_sub) 
save_svgmap(ishops, file = "muc_shops.svg")


## @knitr highlight-examples
ishops2 <- muc_inner + 
  igeom_point(aes(tooltip = tip, highlight = shop, link = website, colour = shop), data = muc_sub)
save_svgmap(ishops2, file = "muc_shops2.svg")


