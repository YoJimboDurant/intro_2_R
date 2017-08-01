library(knitr)
library(rmarkdown)
source("./R/RmdFlip.R")
source("./R/createLessonMaster.R")

x <- list.files("Rmd_lessons", pattern = "[.]Rmd", full.names = TRUE)

createLessonMaster(x, type="article", headFile = "./Rmd_templates/openoffice.Rmd")
render("Master_article.Rmd")
