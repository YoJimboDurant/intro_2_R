library(knitr)
library(rmarkdown)
source("./R/RmdFlip.R")
source("./R/createLessonMaster.R")

x <- list.files("Rmd_lessons", pattern = "[.]Rmd", full.names = TRUE)

createLessonMaster(x, type="shower", headFile = "./Rmd_templates/shower.Rmd")
render("Master_showerhtml.Rmd")

