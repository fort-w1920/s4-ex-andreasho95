# Checks validity of numeric slots
# Returns NULL if valid
# Returns message for specific slot when invalid
check_numeric_slot <- function(slot, lower = -Inf, upper = Inf) {
  invalid <- is.na(slot) || length(slot) > 1 || slot < lower || slot > upper
  if (invalid) {
    char_slot <- deparse(substitute(slot))
    slot_str <- stringr::str_extract(char_slot, "(?<=@).*")
    msg <- paste0("@", slot_str, " must be a numeric of length 1")
    if (lower != -Inf & upper != Inf) {
      msg <- paste0(msg, " in [", lower, ", ", upper, "]")
    }
    return(msg)
  }
  NULL
}


setValidity("animal", function(object) {
  invalids <- character(0)
  invalid_name <- is.na(object@name) || length(object@name) > 1 || nchar(object@name) == 0
  invalid_female <- is.na(object@female) || length(object@female) > 1

  if (invalid_name) {
    invalids <- "@name must be a character of length 1"
  }
  if (invalid_female) {
    invalids <- c(invalids, "@female must be a logical of length 1")
  }

  invalids <- c(invalids, check_numeric_slot(object@weight, 0, Inf))
  
  
  # Early return in case of invalids
  if (length(invalids) > 0) {
    return(invalids)
  } 
  TRUE
})

setValidity("prey", function(object) {
  invalids <- character(0)
  invalids <- c(invalids, check_numeric_slot(object@hide, 0, 1))

  # Early return in case of invalids
  if (length(invalids) > 0) {
    return(invalids)
  } 
  TRUE
})

setValidity("predator", function(object) {
  invalids <- character(0)
  invalids <- c(invalids, check_numeric_slot(object@seek, 0, 1))
  
  # Early return in case of invalids
  if (length(invalids) > 0) {
    return(invalids)
  } 
  TRUE
})

setValidity("mouse", function(object) {
  invalids <- character(0)
  invalids <- c(invalids, check_numeric_slot(object@weight, 0.5, 1))
  invalids <- c(invalids, check_numeric_slot(object@hide, 0.6, 1))
  
  # Early return in case of invalids
  if (length(invalids) > 0) {
    return(invalids)
  } 
  TRUE
})

setValidity("rabbit", function(object) {
  invalids <- character(0)
  invalids <- c(invalids, check_numeric_slot(object@weight, 1, 5))
  invalids <- c(invalids, check_numeric_slot(object@hide, 0.3, 0.8))
  
  # Early return in case of invalids
  if (length(invalids) > 0) {
    return(invalids)
  } 
  TRUE
})

setValidity("deer", function(object) {
  invalids <- character(0)
  invalids <- c(invalids, check_numeric_slot(object@weight, 15, 30))
  invalids <- c(invalids, check_numeric_slot(object@hide, 0.2, 0.7))
  
  # Early return in case of invalids
  if (length(invalids) > 0) {
    return(invalids)
  } 
  TRUE
})

setValidity("hawk", function(object) {
  invalids <- character(0)
  invalids <- c(invalids, check_numeric_slot(object@weight, 3, 8))
  invalids <- c(invalids, check_numeric_slot(object@seek, 0.6, 1))
  
  # Early return in case of invalids
  if (length(invalids) > 0) {
    return(invalids)
  } 
  TRUE
})

setValidity("lynx", function(object) {
  invalids <- character(0)
  invalids <- c(invalids, check_numeric_slot(object@weight, 20, 60))
  invalids <- c(invalids, check_numeric_slot(object@seek, 0.5, 0.9))
  
  # Early return in case of invalids
  if (length(invalids) > 0) {
    return(invalids)
  } 
  TRUE
})
