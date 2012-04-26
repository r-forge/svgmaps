######  Test sp data transformation ##########################################
library(testthat)
library(sp)
context("sp data transformation")
source("conditions.R")
namez <- c("element_id", "point_id", "lon", "lat", "variable", "value", "order", "geom")

test_that("Points works", {
  data(meuse)
  coordinates(meuse) <- c("x","y")
  vars <-  c("elev", "cadmium")
  points <- as_svgmaps(meuse, vars)
  
  expect_that(points, has_names(namez))
  expect_is(points, "data.frame")
  expect_that(nrow(points), equals(nrow(meuse)*length(vars)))
  expect_that(unique(as.character(points$variable)), equals(vars))
})

test_that("Lines work", {
  ## build up SpatialLinesDataFrame
  l1 = cbind(c(1,2,3),c(3,2,2))
  l1a = cbind(l1[,1]+.05,l1[,2]+.05)
  l2 = cbind(c(1,2,3),c(1,1.5,1))
  Sl1 = Line(l1)
  Sl1a = Line(l1a)
  Sl2 = Line(l2)
  S1 = Lines(list(Sl1, Sl1a), ID="a")
  S2 = Lines(list(Sl2), ID="b")
  S3 = Lines(list(Line(cbind(c(3,4,5), c(2,3,9)))), ID="c")
  Sl = SpatialLines(list(S2,S1, S3))
  df = data.frame(z = c(1,2,3), row.names=sapply(slot(Sl, "lines"), function(x) slot(x, "ID")))
  Sldf = SpatialLinesDataFrame(Sl, data = df)

  vars <- c("z")
  lines <- as_svgmaps(Sldf, vars)

  expect_that(lines, is_a("data.frame"))
  expect_that(lines, has_names(namez))
  expect_that(unique(as.character(lines$geom)), equals("path"))
  expect_that(nrow(lines), equals(nrow(lines)*length(vars)))
})


test_that("Polygons work",{
})

test_that("Pixels work",{
})

test_that("Grid work", {
})
