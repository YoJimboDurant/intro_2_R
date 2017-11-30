library(knitr)
library(rmarkdown)
source("./R/RmdFlip.R")
source("./R/createLessonMaster.R")

x <- list.files("Rmd_lessons", pattern = "[.]Rmd", full.names = TRUE)
x <- x[grepl("RUG_Talk_2.Rmd", x)]
createLessonMaster(x, type="beamer")
render("Master_beamer.Rmd")
