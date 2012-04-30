

## maybe add possibility to give a boundary-box
svgmaps <- function (data = NULL, ...) {
  if (!is.null(data)) {data <- as_svgmaps(data, ...)}
  p <- ggplot(data, mapping = aes_string(x = "lon", y = "lat", group = "element_id", geom = "geom")) + coord_equal()
  class(p) <- c("svgmaps", class(p))
  p
}





## like fortify, but specialized for svgmaps
as_svgmaps <- function(object, ...){
  UseMethod("as_svgmaps")
}

as_svgmaps.default <- function (object, ...) {
  stop(paste("as_svgmaps does not support objects of class", class(object), "\n For supported classes type: showMethods(as_svgmaps)"))
}

as_svgmaps.NULL <- function (object, ...) {
  return(NULL)
}

as_svgmaps.data.frame <- function (object, ...){
  if (!is_svgmaps(object)) stop("The data.frame is not in the svgmaps format")
  return(object)
}


## Checks if the dataframe is what i want
is_svgmaps <- function (df) {
  namez <- c("lon", "lat", "element_id", "point_id", "geom", "order")
  if(class(df) == "data.frame" & all(namez %in% names(df))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

## TODO: suppress the GUI-Device
save_svg <- function (object, filename = "RPlot.svg") {
  gr <- ggplotGrob(object)
  script <- system.file("./javascript/tooltip.js", package = "svgmaps")
  igr <- grid::addGrob(gr, gridSVG::scriptGrob(filename = script, inline = TRUE))
  script <- system.file("./javascript/add-events.js", package = "svgmaps")
  igr <- grid::addGrob(igr, gridSVG::scriptGrob(filename = script, inline = TRUE))
  ## Open a new SVG-Device
  svgdev <- gridSVG:::openSVGDev(filename, width=par("din")[1], height=par("din")[2])
  ## Translate grid object, write
  gridSVG:::gridToDev(igr, svgdev)
  ## Close Device
  gridSVG:::devClose(svgdev)
}

view_svg <- function (p) {
  tmpfile <- tempfile()
  save_svg(p, tmpfile)
  ## Open Browser
  browseURL(tmpfile)
}
