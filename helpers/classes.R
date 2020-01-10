# Animal 
setClass("animal",
  slots = c(
    name = "character",
    weight = "numeric",
    female = "logical"
  ),
  prototype = list(
    name = NA_character_,
    weight = NA_real_,
    female = NA
  )
)


### Prey
setClass("prey",
  contains = "animal",
  slots = c(
    hide = "numeric"
  ),
  prototype = list(
    hide = NA_real_
  )
)

##### Mouse
setClass("mouse",
  contains = "prey"
)

##### Rabbit
setClass("rabbit",
  contains = "prey"
)

##### Deer
setClass("deer",
  contains = "prey"
)


### Predator
setClass("predator",
  contains = "animal",
  slots = c(
    seek = "numeric"
  ),
  prototype = list(
    seek = NA_real_
  )
)

##### Hawk
setClass("hawk",
  contains = "predator"
)

##### Lynx
setClass("lynx",
  contains = "predator"
)




# 
# rabbit <- function(name = NA_character_, weight = NULL, female = NA, hide = NULL) {
#   
#   # Draw values from intervall if no values specified
#   if (is.null(weight)) {
#     weight <- runif(1, 1, 5)
#   }
#   if (is.null(hide)) {
#     hide <- runif(1, 0.3, 0.8)
#   }
#   
#   new("rabbit", name = name, weight = weight, female = female, hide = hide)
# }
# 
# 

# deer <- function(name = NA_character_, weight = NULL, female = NA, hide = NULL) {
#   
#   # Draw values from intervall if no values specified
#   if (is.null(weight)) {
#     weight <- runif(1, 15, 30)
#   }
#   if (is.null(hide)) {
#     hide <- runif(1, 0.2, 0.7)
#   }
#   
#   new("deer", name = name, weight = weight, female = female, hide = hide)
# }
# 
# 
# 
# 
# hawk <- function(name = NA_character_, weight = NULL, female = NA, seek = NULL) {
#   
#   # Draw values from intervall if no values specified
#   if (is.null(weight)) {
#     weight <- runif(1, 3, 8)
#   }
#   if (is.null(seek)) {
#     seek <- runif(1, 0.6, 1)
#   }
#   
#   new("hawk", name = name, weight = weight, female = female, seek = seek)
# }
# 
# 
# 
# 
# lynx <- function(name = NA_character_, weight = NULL, female = NA, seek = NULL) {
#   
#   # Draw values from intervall if no values specified
#   if (is.null(weight)) {
#     weight <- runif(1, 20, 60)
#   }
#   if (is.null(seek)) {
#     seek <- runif(1, 0.5, 0.9)
#   }
#   
#   new("lynx", name = name, weight = weight, female = female, seek = seek)
# }
# 

