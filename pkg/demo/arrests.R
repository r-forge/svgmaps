
## @knitr arrests-data
library("maps")
library("svgmaps")
##  USA at a state level (polygons)
states <- map("state", region = ".", exact = FALSE, plot = FALSE, fill = TRUE)
states <- as_svgmap(states)
states <- states[order(states$order),]

svgmap(states) + igeom_polygon(fill = "white", colour = "black")


## @knitr arrests-merge
## Bringing together geographical and statistical data
data("USArrests")
arrests <- USArrests
names(arrests) <- tolower(names(arrests))
arrests$region <- tolower(rownames(arrests))
choro <- merge(states, arrests, by.x = "region", by.y = "region")

## reorder 
choro <- choro[order(choro$order), ]

arrest_map <- svgmap(choro) +
  igeom_polygon(aes(fill = assault, 
                    tooltip = paste(assault, "in",  region), 
                    highlight = element_id)) +
  coord_map() +
  theme_map()
save_svgmap(arrest_map, file = "arrest_map.svg")


