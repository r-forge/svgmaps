

## maybe add possibility to give a boundary-box
svgmap <- function () {
  ggplot(mapping = aes_string(x = "long", y = "lat")) + coord_map()
}
