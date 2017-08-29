library(knitr)
library(rmarkdown)
source("./R/RmdFlip.R")
source("./R/createLessonMaster.R")

x <- list.files("Rmd_lessons", pattern = "[.]Rmd", full.names = TRUE)

createLessonMaster(x, type="tangohtml", headFile = "./Rmd_templates/tangohtml.Rmd")
render("Master_tangohtml.Rmd")

