
#'A special theme for maps
#'
#'This is a special theme for maps.
#'
#'This theme can be added to a svgmaps graphics with the '+' operator. 
#'It is very minimal. It supresses the axis and the ticks. The code is a contribution from 
#'Osmo Salomaa. 
#'@param base_size A number defining the size of the text
#'@param base_family The text family
#'@return An ggplot options object
#'@export
#'@examples 
#'data (muc)
#'svgmaps(muc) + igeom_polygon() + theme_map()


theme_map = function(base_size=11, base_family="")
{
  require(grid)
  .theme_text = function(family=base_family, size=0.8*base_size, ...)
    theme_text(family=base_family, size=size, ...)
  o = structure(list(axis.line=theme_blank(),
           axis.text.x=theme_blank(),
           axis.text.y=theme_blank(),
           axis.ticks=theme_blank(),
           axis.ticks.length=unit(0, "lines"),
           axis.ticks.margin=unit(0, "lines"),
           axis.title.x=theme_blank(),
           axis.title.y=theme_blank(),
           legend.background=theme_rect(fill=NA, colour=NA),
           legend.box=NULL,
           legend.direction="vertical",
           legend.justification = "center",
           legend.key.height=NULL,
           legend.key.size=unit(1.2, "lines"),
           legend.key.width=NULL,
           legend.key=theme_rect(colour="white"),
           legend.margin=unit(0.2, "cm"),
           legend.position="right",
           legend.text=.theme_text(),
           legend.text.align=NULL,
           legend.title=.theme_text(hjust=0, face="bold"),
           legend.title.align=NULL,
           panel.background=theme_blank(),
           panel.border=theme_blank(),
           panel.grid.major=theme_blank(),
           panel.grid.minor=theme_blank(),
           panel.margin=unit(0, "lines"),
           plot.background=theme_blank(),
           plot.margin=unit(c(1, 1, 0.5, 0.5), "lines"),
           plot.title=.theme_text(vjust=1),
           strip.background=theme_rect(fill="grey90", colour="black", size=0.3),
           strip.text.x=.theme_text(),
           strip.text.y=.theme_text(angle=-90)))
  
  return(structure(o, class="options"))
}