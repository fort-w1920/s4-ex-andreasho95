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

##### Hawk
setClass("hawk",
  contains = "predator"
)

##### Lynx
setClass("lynx",
  contains = "predator"
)


