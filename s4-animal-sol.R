source("helpers/make_name.R")
source("helpers/classes.R")
source("helpers/validity_checks.R")
source("helpers/constructors.R")
source("helpers/show_methods.R")
source("helpers/meet_method.R")

set.seed(20191121)
animals <- list(
  mouse(female = TRUE),
  rabbit(),
  hawk(female = TRUE),
  deer(),
  lynx(female = TRUE),
  lynx(female = FALSE),
  deer(),
  mouse(female = FALSE),
  deer(female = TRUE)
)

for (animal1 in animals[1:5]) {
  for (animal2 in animals[9:5]) {
    cat(meet(animal1, animal2))
  }
}
