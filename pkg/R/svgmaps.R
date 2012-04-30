

## maybe add possibility to give a boundary-box
svgmap <- function (data = NULL, ...) {
  if (!is.null(data)) {data <- as_svgmap(data, ...)}
  p <- ggplot(data, mapping = aes_string(x = "lon", y = "lat", group = "element_id", geom = "geom")) + coord_equal()
  class(p) <- c("svgmap", class(p))
  p
}





## like fortify, but specialized for svgmap
as_svgmap <- function(object, ...){
  UseMethod("as_svgmap")
}

as_svgmap.default <- function (object, ...) {
  stop(paste("as_svgmap does not support objects of class", class(object), "\n For supported classes type: showMethods(as_svgmap)"))
}

as_svgmap.NULL <- function (object, ...) {
  return(NULL)
}

as_svgmap.data.frame <- function (object, ...){
  if (!is_svgmap(object)) stop("The data.frame is not in the svgmap format")
  return(object)
}


## Checks if the dataframe is what i want
is_svgmap <- function (df) {
  namez <- c("lon", "lat", "element_id", "point_id", "geom", "order")
  if(class(df) == "data.frame" & all(namez %in% names(df))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

## TODO: suppress the GUI-Device
save_svgmap <- function (object, filename = "RPlot.svg") {
  gr <- ggplotGrob(object)
  
  js_dir <- system.file("javascript", package = "svgmaps")
  script <- file.path(js_dir, "tooltip.js")
  
  igr <- grid::addGrob(gr, gridSVG::scriptGrob(filename = script, inline = TRUE))
  script <- system.file(js_dir, "add-events.js")
  igr <- grid::addGrob(igr, gridSVG::scriptGrob(filename = script, inline = TRUE))
  ## Open a new SVG-Device
  svgdev <- gridSVG:::openSVGDev(filename, width=par("din")[1], height=par("din")[2])
  ## Translate grid object, write
  gridSVG:::gridToDev(igr, svgdev)
  ## Close Device
  gridSVG:::devClose(svgdev)
}

view_svgmap <- function (p) {
  tmpfile <- tempfile()
  save_svg(p, tmpfile)
  ## Open Browser
  browseURL(tmpfile)
}
