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
    invalids <- "@hide must be a numeric of length 1 in [0, 1]"
  }
  # Early return in case of invalids
  if (length(invalids) > 0) {
    return(invalids)
  } 
  TRUE
})

setValidity("predator", function(object) {
  invalids <- character(0)
  invalid_seek <- is.na(object@seek) || length(object@seek) > 1 || object@seek < 0  || object@seek > 1
  if (invalid_seek) {
    invalids <- "@seek must be a numeric of length 1 in [0, 1]"
  }
  # Early return in case of invalids
  if (length(invalids) > 0) {
    return(invalids)
  } 
  TRUE
})

setValidity("mouse", function(object) {
  invalids <- character(0)
  invalid_weight <- object@weight  < 0.5 || object@weight > 1
  invalid_hide <- object@hide < 0.6 || object@hide > 1
  if (invalid_weight) {
    invalids <- "@weight must be a numeric of length 1 in [0.5, 1]"
  }
  if (invalid_hide) {
    invalids <- c(invalids, "@hide must be a numeric of length 1 in [0.6, 1]")
  }
  # Early return in case of invalids
  if (length(invalids) > 0) {
    return(invalids)
  } 
  TRUE
})

setValidity("rabbit", function(object) {
  invalids <- character(0)
  invalid_weight <- object@weight  < 1 || object@weight > 5
  invalid_hide <- object@hide < 0.3 || object@hide > 0.8
  if (invalid_weight) {
    invalids <- "@weight must be a numeric of length 1 in [1, 5]"
  }
  if (invalid_hide) {
    invalids <- c(invalids, "@hide must be a numeric of length 1 in [0.3, 0.8]")
  }
  # Early return in case of invalids
  if (length(invalids) > 0) {
    return(invalids)
  } 
  TRUE
})

setValidity("deer", function(object) {
  invalids <- character(0)
  invalid_weight <- object@weight  < 15 || object@weight > 30
  invalid_hide <- object@hide < 0.2 || object@hide > 0.7
  if (invalid_weight) {
    invalids <- "@weight must be a numeric of length 1 in [15, 30]"
  }
  if (invalid_hide) {
    invalids <- c(invalids, "@hide must be a numeric of length 1 in [0.2, 0.7]")
  }
  # Early return in case of invalids
  if (length(invalids) > 0) {
    return(invalids)
  } 
  TRUE
})

setValidity("hawk", function(object) {
  invalids <- character(0)
  invalid_weight <- object@weight  < 3 || object@weight > 8
  invalid_seek <- object@seek < 0.6 || object@seek > 1
  if (invalid_weight) {
    invalids <- "@weight must be a numeric of length 1 in [3, 8]"
  }
  if (invalid_seek) {
    invalids <- c(invalids, "@seek must be a numeric of length 1 in [0.6, 1]")
  }
  # Early return in case of invalids
  if (length(invalids) > 0) {
    return(invalids)
  } 
  TRUE
})

setValidity("lynx", function(object) {
  invalids <- character(0)
  invalid_weight <- object@weight  < 20 || object@weight > 60
  invalid_seek <- object@seek < 0.5 || object@seek > 0.9
  if (invalid_weight) {
    invalids <- "@weight must be a numeric of length 1 in [20, 60]"
  }
  if (invalid_seek) {
    invalids <- c(invalids, "@seek must be a numeric of length 1 in [0.5, 0.9]")
  }
  # Early return in case of invalids
  if (length(invalids) > 0) {
    return(invalids)
  } 
  TRUE
})
