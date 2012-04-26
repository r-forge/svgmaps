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
has_names <- function(namez){
  function(x){
    expectation(all(names(x) %in% namez) & length(namez) == length(names(x)), "Names of do not fit")
  }
}
