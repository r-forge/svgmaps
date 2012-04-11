######################################################
# Tests wether functions work as expected
######################################################
library(testthat)

context("osmar data transformation")

load("../data/home.rda")
source("../R/osmar-transform.R")
source("conditions.R")
require(reshape)


test_that("does melt_attrs work?",{

  # does attribute to tags work fine?
  expect_is(melt_attrs(home$lines$attrs, "shop"), "data.frame")
  expect_is(melt_attrs(home$nodes$attrs, "shop"), "data.frame")
  expect_is(melt_attrs(home$relations$attrs, "shop"), "data.frame")

  expect_is(melt_attrs(home$relations$attrs, "version"), "data.frame")
  expect_is(melt_attrs(home$relations$attrs, "version"), "data.frame")
  expect_is(melt_attrs(home$relations$attrs, "version"), "data.frame")


  expect_that(melt_attrs(home$nodes$attrs, "version"), is_not_empty())
  expect_that(melt_attrs(home$ways$attrs, "version"), is_not_empty())
  expect_that(melt_attrs(home$relations$attrs, "version"), is_not_empty())
  
  expect_that(melt_attrs(home$nodes$attrs, "version"), has_names(c("id", "variable", "value")))
  expect_that(melt_attrs(home$nodes$attrs, "version"), has_names(c("id", "variable", "value")))
  expect_that(melt_attrs(home$nodes$attrs, "version"), has_names(c("id", "variable", "value")))
  
  expect_that(melt_attrs(home$nodes$attrs, "xxxxxxx"), has_names(c("id", "variable", "value")))
  expect_that(melt_attrs(home$nodes$attrs, "xxxxxxx"), has_names(c("id", "variable", "value")))
  expect_that(melt_attrs(home$nodes$attrs, "xxxxxxx"), has_names(c("id", "variable", "value")))
  
  expect_that(melt_attrs(home$nodes$attrs, "xxxxxxx"), is_empty())
  expect_that(melt_attrs(home$nodes$attrs, "xxxxxxx"), is_empty())
  expect_that(melt_attrs(home$nodes$attrs, "xxxxxxx"), is_empty())
})






test_that("does merge_attrs_tags work?",{
  # yes: variable exists, no: doesn't exist 
  nodes_yes <- merge_attrs_tags(home$nodes, vars="uid")
  nodes_no <- merge_attrs_tags(home$nodes, vars="xxx")
  
  ways_yes <- merge_attrs_tags(home$ways, vars="version") 
  ways_no <- merge_attrs_tags(home$ways, vars="")
  
  relations_yes <- merge_attrs_tags(home$relations, vars="timestamp")
  relations_no <- merge_attrs_tags(home$relations, vars="katzenklo")
  
  
  expect_that(nodes_yes, is_not_empty())
  expect_that(nodes_no,is_not_empty())
  expect_that(ways_yes, is_not_empty())
  expect_that(ways_no, is_not_empty())
  expect_that(relations_yes, is_not_empty())
  expect_that(relations_no, is_not_empty())
  
  namez <- c("id", "variable", "value")
  expect_that(nodes_yes, has_names(namez))
  expect_that(nodes_no, has_names(namez))
  expect_that(ways_yes, has_names(namez))
  expect_that(ways_no, has_names(namez))
  expect_that(relations_yes, has_names(namez))
  expect_that(relations_no, has_names(namez))
})

test_that("does get_coords_nodes work?", {
  namez <- c("lat", "lon", "node_id", "element_id", "geom", "order")
  coords <- get_coords_nodes(home)
  expect_is(coords, "data.frame")
  expect_that(coords, is_not_empty())
  expect_that(coords, has_names(namez))
  expect_that(unique(coords$geom), equals("point"))
  expect_that(unique(coords$order), equals(1))
  expect_that(nrow(coords), equals(length(unique(coords$node_id))))
  expect_that(coords$node_id, equals(coords$element_id))
  # missing: make empty subset of home and try
})

test_that("does get_coords_ways work?", {
  namez <- c("lat", "lon", "node_id", "element_id", "geom", "order")
  coords <- get_coords_ways(home)
  expect_is(coords, "data.frame")
  expect_that(coords, is_not_empty())
  expect_that(coords, has_names(namez))
  expect_that(unique(coords$geom), equals(c("path", "polygon")))
  # missing: make empty subset of home and try
})

test_that("does get_coords_relations work?", {
})

test_that("does merge_coords_attrs work?", {
  namez <- c("element_id", "node_id", "variable", "value", "lon", "lat", "order")
  
})


test_that("does melt_nodes work?", {
  nodL <- melt_relations(home, "version")
  namez <- c("lon", "lat", "variable", "value", "order", "geom", "element_id", "node_id")
  expect_that(nodL, is_not_empty())
  expect_that(nodL, has_names(namez))
})


test_that("does fortify_osmar_work?", {
})


test_that("does melt_relations work?", {
  relL <- melt_relations(home, "version")
  namez <- c("lon", "lat", "variable", "value", "order", "geom", "element_id", "node_id")
  expect_that(relL, is_not_empty())
  expect_that(relL, has_names(namez))
})


test_that("does melt_ways work?", {
  waysL <- melt_ways(home, "version", "version")
  namez <- c("lon", "lat", "variable", "value", "order", "geom", "element_id", "node_id")
  expect_that(waysL, is_not_empty())
  expect_that(waysL, has_names(namez))
})

test_that("does change_ways2polygons work?",{
  ways <- melt_ways(home, "version","version")
  waysP <- change_ways2polygons(ways)
  test_that(dim(waysP), equals(dim(ways)))
  test_that(unique(waysP$geom), equals(c("path", "polygon")))
})
