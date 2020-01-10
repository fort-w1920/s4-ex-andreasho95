animal <- function(name = NA_character_, weight = NA_real_, female = NA) {
  new("animal", name = name, weight = weight, female = female)
}


prey <- function(name = NA_character_, weight = NA_real_, female = NA, hide = NA) {
  new("prey", name = name, weight = weight, female = female, hide = hide)
}