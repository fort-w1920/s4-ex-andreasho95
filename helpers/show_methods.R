setMethod("show", "animal", function(object) {
  cat(is(object)[[1]], "\n",
      "  Name: ", object@name, "\n",
      "  Weight:  ", object@weight, "\n",
      "  Female:  ", object@female, "\n",
      sep = ""
  )
})

# myanimal <- animal(name = "Sepp", weight = 20, female = TRUE)
# myanimal

#> animal
#>  Name: Sepp
#>  Weight: 20
#>  Female: FALSE

setMethod("show", "prey", function(object) {
  cat(is(object)[[1]], "\n",
      "  Name: ", object@name, "\n",
      "  Weight:  ", object@weight, "\n",
      "  Female:  ", object@female, "\n",
      "  Hide:  ", object@hide, "\n",
      sep = ""
  )
})

# myprey <- prey(name = "Sepp", weight = 20, female = TRUE, hide = 1)
# myprey

#> prey
#>  Name: Sepp
#>  Weight: 20
#>  Female: FALSE
#>  Hide: 1

setMethod("show", "predator", function(object) {
  cat(is(object)[[1]], "\n",
      "  Name: ", object@name, "\n",
      "  Weight:  ", object@weight, "\n",
      "  Female:  ", object@female, "\n",
      "  Seek:  ", object@seek, "\n",
      sep = ""
  )
})

# mypredator <- predator(name = "Dieter", weight = 20, female = TRUE, seek = 1)
# mypredator

#> predator
#>  Name: Dieter
#>  Weight: 20
#>  Female: TRUE
#>  Seek: 1
