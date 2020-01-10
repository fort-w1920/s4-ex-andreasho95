# Checks validity of numeric slotsS
# Returns TRUE if invalid
check_numeric_slot <- function(slot, lower = 0, upper = 1e5) {
  is.na(slot) || length(slot) > 1 || slot < lower || slot > upper
}


setValidity("animal", function(object) {
  invalids <- character(0)
  invalid_name <- is.na(object@name) || length(object@name) > 1 || nchar(object@name) == 0
  invalid_female <- is.na(object@female) || length(object@female) > 1
  invalid_weight <- check_numeric_slot(object@weight)
  if (invalid_name) {
    invalids <- "@name must be a character of length 1"
  }
  if (invalid_female) {
    invalids <- c(invalids, "@female must be a logical of length 1")
  }
  if (invalid_weight) {
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
  invalid_hide <- check_numeric_slot(object@hide, 0, 1)
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
  invalid_seek <- check_numeric_slot(object@seek, 0, 1)
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
  invalid_weight <-  check_numeric_slot(object@weight, 0.5, 1)
  invalid_hide <- check_numeric_slot(object@hide, 0.6, 1)
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
  invalid_weight <- check_numeric_slot(object@weight, 1, 5)
  invalid_hide <- check_numeric_slot(object@weight, 0.3, 0.8)
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
  invalid_weight <- check_numeric_slot(object@weight, 15, 30)
  invalid_hide <- check_numeric_slot(object@weight, 0.2, 0.7)
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
  invalid_weight <- check_numeric_slot(object@weight, 3, 8)
  invalid_seek <- check_numeric_slot(object@seek, 0.6, 1)
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
  invalid_weight <- check_numeric_slot(object@weight, 20, 60)
  invalid_seek <- check_numeric_slot(object@seek, 0.5, 0.9)
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
