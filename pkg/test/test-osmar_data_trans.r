######################################################
# Tests wether functions work as expected
######################################################
library(testthat)

context("osmar data transformation")

load("../data/home.rda")
source("../R/osmarDataTransform.R")


# some test functions 

# dataframe shall not be empty
is_not_empty <- function() {
  function(x) {
    expectation(nrow(x) > 0, "empty dataframe")
  }
}

# dataframe shall be empty
is_empty <- function(){
  function(x){
    expectation(nrow(x) == 0, "dataframe is not empty, but should be")
  }
}

# dataframe shall have namez
df_has_names <- function(namez){
  function(x){
    expectation(all(names(x) %in% namez) & length(namez) == length(names(x)), "Names of do not fit")
  }
}

test_that("does osmar_attrs_to_tags work?",{

  # does attribute to tags work fine?
  expect_is(osmar_attrs_to_tags(home$lines$attrs, "shop"), "data.frame")
  expect_is(osmar_attrs_to_tags(home$nodes$attrs, "shop"), "data.frame")
  expect_is(osmar_attrs_to_tags(home$relations$attrs, "shop"), "data.frame")

  expect_is(osmar_attrs_to_tags(home$relations$attrs, "version"), "data.frame")
  expect_is(osmar_attrs_to_tags(home$relations$attrs, "version"), "data.frame")
  expect_is(osmar_attrs_to_tags(home$relations$attrs, "version"), "data.frame")


  expect_that(osmar_attrs_to_tags(home$nodes$attrs, "version"), is_not_empty())
  expect_that(osmar_attrs_to_tags(home$ways$attrs, "version"), is_not_empty())
  expect_that(osmar_attrs_to_tags(home$relations$attrs, "version"), is_not_empty())
  
  expect_that(osmar_attrs_to_tags(home$nodes$attrs, "version"), df_has_names(c("id", "variable", "value")))
  expect_that(osmar_attrs_to_tags(home$nodes$attrs, "version"), df_has_names(c("id", "variable", "value")))
  expect_that(osmar_attrs_to_tags(home$nodes$attrs, "version"), df_has_names(c("id", "variable", "value")))
  
  expect_that(osmar_attrs_to_tags(home$nodes$attrs, "xxxxxxx"), df_has_names(c("id", "variable", "value")))
  expect_that(osmar_attrs_to_tags(home$nodes$attrs, "xxxxxxx"), df_has_names(c("id", "variable", "value")))
  expect_that(osmar_attrs_to_tags(home$nodes$attrs, "xxxxxxx"), df_has_names(c("id", "variable", "value")))
  
  expect_that(osmar_attrs_to_tags(home$nodes$attrs, "xxxxxxx"), is_empty())
  expect_that(osmar_attrs_to_tags(home$nodes$attrs, "xxxxxxx"), is_empty())
  expect_that(osmar_attrs_to_tags(home$nodes$attrs, "xxxxxxx"), is_empty())
})






test_that("does osmar_merge_attrs_and_tags work?",{
  # yes: variable exists, no: doesn't exist 
  nodes_yes <- osmar_merge_attrs_and_tags(home$nodes, vars="uid")
  nodes_no <- osmar_merge_attrs_and_tags(home$nodes, vars="xxx")
  
  ways_yes <- osmar_merge_attrs_and_tags(home$ways, vars="version") 
  ways_no <- osmar_merge_attrs_and_tags(home$ways, vars="")
  
  relations_yes <- osmar_merge_attrs_and_tags(home$relations, vars="timestamp")
  relations_no <- osmar_merge_attrs_and_tags(home$relations, vars="katzenklo")
  
  
  expect_that(nodes_yes, is_not_empty())
  expect_that(nodes_no,is_not_empty())
  expect_that(ways_yes, is_not_empty())
  expect_that(ways_no, is_not_empty())
  expect_that(relations_yes, is_not_empty())
  expect_that(relations_no, is_not_empty())
  
  namez <- c("id", "variable", "value")
  expect_that(nodes_yes, df_has_names(namez))
  expect_that(nodes_no, df_has_names(namez))
  expect_that(ways_yes, df_has_names(namez))
  expect_that(ways_no, df_has_names(namez))
  expect_that(relations_yes, df_has_names(namez))
  expect_that(relations_no, df_has_names(namez))
})

test_that("does osmar_coords_nodes work?", {
  namez <- c("lat", "lon", "node_id", "element_id", "geom", "order")
  coords <- osmar_coords_nodes(home)
  expect_is(coords, "data.frame")
  expect_that(coords, is_not_empty())
  expect_that(coords, df_has_names(namez))
  expect_that(unique(coords$geom), equals("point"))
  expect_that(unique(coords$order), equals(1))
  expect_that(nrow(coords), equals(length(unique(coords$node_id))))
  expect_that(coords$node_id, equals(coords$element_id))
  # missing: make empty subset of home and try
})

test_that("does osmar_coords_ways work?", {
  namez <- c("lat", "lon", "node_id", "element_id", "geom", "order")
  coords <- osmar_coords_ways(home)
  expect_is(coords, "data.frame")
  expect_that(coords, is_not_empty())
  expect_that(coords, df_has_names(namez))
  expect_that(unique(coords$geom), equals("path"))
  # missing: make empty subset of home and try
})

test_that("does osmar_coords_relations work?", {
})

test_that("does osmar_merge_coords_attrs work?", {
  namez <- c("element_id", "node_id", "variable", "value", "lon", "lat", "order")
  
})


test_that("does osmar_nodes_long work?", {
  nodL <- osmar_relations_long(home, "version")
  namez <- c("lon", "lat", "variable", "value", "order", "geom", "element_id", "node_id")
  expect_that(nodL, is_not_empty())
  expect_that(nodL, df_has_names(namez))
})


test_that("does osmar_long_work?", {
})


test_that("does osmar_relations_long work?", {
  relL <- osmar_relations_long(home, "version")
  namez <- c("lon", "lat", "variable", "value", "order", "geom", "element_id", "node_id")
  expect_that(relL, is_not_empty())
  expect_that(relL, df_has_names(namez))
})


test_that("does osmar_ways_long work?", {
  waysL <- osmar_ways_long(home, "version")
  namez <- c("lon", "lat", "variable", "value", "order", "geom", "element_id", "node_id")
  expect_that(waysL, is_not_empty())
  expect_that(waysL, df_has_names(namez))
})

test_that("does osmar_ways2Polygons work?",{
  ways <- osmar_ways_long(home, "version")
  waysP <- osmar_ways2Polygons(ways)
  test_that(dim(waysP), equals(dim(ways)))
  test_that(unique(waysP$geom), equals(c("line", "polygon")))
})
