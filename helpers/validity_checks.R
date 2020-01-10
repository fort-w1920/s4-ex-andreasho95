setValidity("animal", function(object) {
  invalids <- character(0)
  invalid_name <- is.na(object@name) || length(object@name) > 1 || nchar(object@name) == 0
  invalid_female <- is.na(object@female) || length(object@female) > 1
  invalid_weigth <- is.na(object@weight) || length(object@weight) > 1
  
  if (invalid_name) {
    invalids <- "@name must be a character of length 1"
  }
  
  if (invalid_female) {
    invalids <- c(invalids, "@female must be a logical of length 1")
  }
  
  if (invalid_weigth) {
    invalids <- c(invalids, "@weight must be a numeric of length 1")
  }
  
  # Early return in case of invalids
  if (length(invalids) > 0) {
    return(invalids)
  } 
  
  TRUE
})


setValidity("prey", function(object) {
  invalids <- character(0)
  invalid_hide <- is.na(object@hide) || length(object@hide) > 1 || object@hide < 0  || object@hide > 1
  if (invalid_hide) {
    invalids <- "@hide must be a numeric of length 1 in [0,1]"
  }
  # Early return in case of invalids
  if (length(invalids) > 0) {
    return(invalids)
  } 
  
  TRUE
})
