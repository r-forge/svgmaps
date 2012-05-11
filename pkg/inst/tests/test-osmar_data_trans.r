######################################################
# Tests wether functions work as expected
######################################################
library(testthat)

context("osmar data transformation")

load("../../data/muc.RData")
source("../../R/osmar-transform.R")
source("conditions.R")
require(reshape)


namez <- c("lat", "lon", "point_id", "element_id", "geom", "order")

test_that("does get_coords_nodes work?", {
  coords <- get_coords_nodes(muc)
  expect_is(coords, "data.frame")
  expect_that(coords, is_not_empty())
  expect_that(coords, has_names(namez))
  expect_that(unique(coords$geom), equals("point"))
  expect_that(unique(coords$order), equals(1))
  expect_that(nrow(coords), equals(length(unique(coords$point_id))))
  expect_that(coords$point_id, equals(coords$element_id))
  # missing: make empty subset of muc and try
})

test_that("does get_coords_ways work?", {
  coords <- get_coords_ways(muc)
  expect_is(coords, "data.frame")
  expect_that(coords, is_not_empty())
  expect_that(coords, has_names(namez))
  expect_that(unique(coords$geom), equals(c("path", "polygon")))
  # missing: make empty subset of muc and try
})

test_that("does get_coords_relations work?", {
})

test_that("does merge_coords_attrs work?", {
  
})
namez <- c("lat", "lon", "point_id", "element_id", "geom", "order", "variable", "value")


test_that("does svgmaps.osmar work?", {
})

test_that("does change_ways2polygons work?",{
  ways <- melt_ways(muc)
  waysP <- change_ways2polygons(ways)
  test_that(dim(waysP), equals(dim(ways)))
  test_that(unique(waysP$geom), equals(c("path", "polygon")))
})

test_that("add_keys", {
  library(reshape2)
  key <- "shop"
  osmar2 <- add_keys(muc, "shop")
  osmar3 <- add_keys(muc, c("shop", "version"))

})
