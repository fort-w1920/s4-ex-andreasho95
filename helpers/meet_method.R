setGeneric("meet", function(animal_1, animal_2, ...) standardGeneric("meet"))

setMethod(
  "meet",
  signature = c("animal", "animal"),
  function(animal_1, animal_2, probs) {
    incidents <- c(
      paste(is(animal_1)[1], animal_1@name, "&", is(animal_2)[1], animal_2@name, "ignore each other\n"),
      paste(is(animal_1)[1], animal_1@name, "&", is(animal_2)[1], animal_2@name, "make sweet, sweet love\n"),
      paste(is(animal_1)[1], animal_1@name, "kills and eats", is(animal_2)[1], animal_2@name, "\n"),
      paste(is(animal_1)[1], animal_1@name, "escapes from", is(animal_2)[1], animal_2@name, "\n"),
      paste(is(animal_1)[1], animal_1@name, "&", is(animal_2)[1], animal_2@name, "fight for territory\n"),
      paste(is(animal_1)[1], animal_1@name, "&", is(animal_2)[1], animal_2@name, "sniff each others' butts\n")
    )
    incident <- sample(incidents, 1, prob = probs)
    incident
  }
)

setMethod(
  "meet",
  signature = c(animal_1 = "prey", animal_2 = "prey"),
  function(animal_1, animal_2) {
    if (identical(animal_1, animal_2)) {
      msg <- get_reflection_message(animal_1)
      return(msg)
    }
    same_species <- is(animal_1)[1] == is(animal_2)[1]
    same_gender <- animal_1@female == animal_2@female
    if (same_species & !same_gender) {
      probs <- c(0.25, 0.5, 0, 0, 0, 0.25)
    } else {
      probs <- c(0.5, 0, 0, 0, 0, 0.5)
    }
    callNextMethod(animal_1, animal_2, probs = probs)
  }
)

setMethod(
  "meet",
  signature = c(animal_1 = "predator", animal_2 = "predator"),
  function(animal_1, animal_2) {
    if (identical(animal_1, animal_2)) {
      msg <- get_reflection_message(animal_1)
      return(msg)
    }
    same_species <- is(animal_1)[1] == is(animal_2)[1]
    same_gender <- animal_1@female == animal_2@female
    if (same_species & !same_gender) {
      probs <- c(0, 0.5, 0, 0, 0.5, 0)
    } else {
      probs <- c(1/3, 0, 0, 0, 1/3, 1/3)
    }
    callNextMethod(animal_1, animal_2, probs = probs)
  }
)

setMethod(
  "meet",
  signature = c(animal_1 = "prey", animal_2 = "predator"),
  function(animal_1, animal_2) {
    prey <- animal_1
    predator <- animal_2
    if (prey@weight > 0.05 * predator@weight & prey@weight < 0.7 * predator@weight) {
      prob_kills <- min(1 , max(0, 0,6 + predator@seek - prey@hide))
      prob_escapes <- 1 - prob_kills
      probs <- c(0, 0, prob_kills, prob_escapes, 0, 0)
    } else {
      probs <- c(0.5, 0, 0, 0, 0, 0.5)
    }
    callNextMethod(predator, prey, probs = probs)
  }
)

setMethod(
  "meet",
  signature = c(animal_1 = "predator", animal_2 = "prey"),
  function(animal_1, animal_2) {
    prey <- animal_2
    predator <- animal_1
    if (prey@weight > 0.05 * predator@weight & prey@weight < 0.7 * predator@weight) {
      prob_kills <- min(1 , max(0, 0,6 + predator@seek - prey@hide))
      prob_escapes <- 1 - prob_kills
      probs <- c(0, 0, prob_kills, prob_escapes, 0, 0)
    } else {
      probs <- c(0.5, 0, 0, 0, 0, 0.5)
    }
    callNextMethod(predator, prey, probs = probs)
  }
)


get_reflection_message <- function(animal){
  if (animal@female) {
    pronoun <- "her"
  } else {
    pronoun <- "his"
  }
  paste(is(animal)[1], animal@name, "gazes at", pronoun, "reflection in a puddle")
}


