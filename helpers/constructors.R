# Constructor for animal class
#
# inputs:
#   name: a character vector of length 1. Specifies the name of the animal.
#   weight: a numeric vector of length 1. Specifies the weight of the animal.
#   female: a logical vector of length 1. TRUE indicates female.
animal <- function(name = make_name(), weight = NA_real_,
                   female = sample(c(TRUE, FALSE), 1)) {
  new("animal", name = name, weight = weight, female = female)
}

# Constructor for prey class
#
# inputs:
#   name: a character vector of length 1. Specifies the name of the prey
#   weight: a numeric vector of length 1. Specifies the weight of the prey
#   female: a logical vector of length 1. TRUE indicates female.
#   hide: a numeric vector of length 1 in [0, 1]. Specifices hiding capabilities
prey <- function(name = make_name(), weight = NA_real_, 
                 female = sample(c(TRUE, FALSE), 1), hide = NA) {
  new("prey", name = name, weight = weight, female = female, hide = hide)
}

# Constructor for predator class
#
# inputs:
#   name: a character vector of length 1. Specifies the name of the predator
#   weight: a numeric vector of length 1. Specifies the weight of the predator
#   female: a logical vector of length 1. TRUE indicates female.
#   seek: a numeric vector of length 1 in [0, 1]. Specifices seeking capabilities
predator <- function(name = make_name(), weight = NA_real_, 
                     female = sample(c(TRUE, FALSE), 1), seek = NA) {
  new("predator", name = name, weight = weight, female = female, seek = seek)
}

# Constructor for mouse class
#
# inputs:
#   name: a character vector of length 1. Specifies the name of the mouse
#   weight: a numeric vector of length 1 in [0.5, 1]. Specifies the weight of the mouse
#   female: a logical vector of length 1. TRUE indicates female.
#   hide: a numeric vector of length 1 in [0.6, 1]. Specifices hiding capabilities.
mouse <- function(name = make_name(), weight = runif(1, 0.5, 1), 
                  female = sample(c(TRUE, FALSE), 1), hide = runif(1, 0.6, 1)) {
  new("mouse", name = name, weight = weight, female = female, hide = hide)
}

# Constructor for rabbit class
#
# inputs:
#   name: a character vector of length 1. Specifies the name of the rabbit
#   weight: a numeric vector of length 1 in [1, 5]. Specifies the weight of the rabbit
#   female: a logical vector of length 1. TRUE indicates female.
#   hide: a numeric vector of length 1 in [0.3, 0.8]. Specifices hiding capabilities.
rabbit <- function(name = make_name(), weight = runif(1, 1, 5), 
                   female = sample(c(TRUE, FALSE), 1), hide = runif(1, 0.3, 0.8)) {
  new("rabbit", name = name, weight = weight, female = female, hide = hide)
}

# Constructor for deer class
#
# inputs:
#   name: a character vector of length 1. Specifies the name of the deer
#   weight: a numeric vector of length 1 in [15, 30]. Specifies the weight of the deer
#   female: a logical vector of length 1. TRUE indicates female.
#   hide: a numeric vector of length 1 in [0.2, 0.7]. Specifices hiding capabilities.
deer <- function(name = make_name(), weight = runif(1, 15, 30), 
                 female = sample(c(TRUE, FALSE), 1), hide = runif(1, 0.2, 0.7)) {
  new("deer", name = name, weight = weight, female = female, hide = hide)
}

# Constructor for hawk class
#
# inputs:
#   name: a character vector of length 1. Specifies the name of the hawk
#   weight: a numeric vector of length 1 in [3, 8]. Specifies the weight of the hawk
#   female: a logical vector of length 1. TRUE indicates female.
#   hide: a numeric vector of length 1 in [0.6, 1]. Specifices hiding capabilities.
hawk <- function(name = make_name(), weight = runif(1, 3, 8), 
                 female = sample(c(TRUE, FALSE), 1), seek = runif(1, 0.6, 1)) {
  new("hawk", name = name, weight = weight, female = female, seek = seek)
}

# Constructor for lynx class
#
# inputs:
#   name: a character vector of length 1. Specifies the name of the lynx
#   weight: a numeric vector of length 1 in [20, 60]. Specifies the weight of the lynx
#   female: a logical vector of length 1. TRUE indicates female.
#   hide: a numeric vector of length 1 in [0.5, 0.9]. Specifices hiding capabilities.
lynx <- function(name = make_name(), weight = runif(1, 20, 60), 
                 female = sample(c(TRUE, FALSE), 1), seek = runif(1, 0.5, 0.9)) {
  new("lynx", name = name, weight = weight, female = female, seek = seek)
}
