createLessonMaster <- function(RmdFiles, type=c("article","beamer","tangohtml"),
                               headFile = "./Rmd_templates/beamer.Rmd"){
  outType = match.arg(type)
  stopifnot(all(file.exists(RmdFiles)))
  dir.create(path = "./working_rmd", showWarnings = FALSE)
  outFiles <- paste("./working_rmd", gsub(".*/", "", RmdFiles), sep="/")
  print(outFiles)

  
  if(outType == "tangohtml"){
    xRmdFiles <- lapply(RmdFiles, RmdFlip)
    lapply(seq_along(xRmdFiles), function(i){
      write(xRmdFiles[[i]], outFiles[[i]])
    })
    
    headMaster <- readLines(headFile)
  }
  
  if(outType == "beamer"){
      xRmdFiles <- lapply(RmdFiles, RmdFlip)
      lapply(seq_along(xRmdFiles), function(i){
        write(xRmdFiles[[i]], outFiles[[i]])
      })
      
      headMaster <- readLines(headFile)
    }
  
  if(outType == "article"){
    xRmdFiles <- lapply(RmdFiles, RmdFlip, tagOnly=TRUE)
    lapply(seq_along(xRmdFiles), function(i){
      write(xRmdFiles[[i]], outFiles[[i]])
    })
    
    headMaster <- readLines(headFile)
  }
  
  outFile <- paste0("Master_", outType, ".Rmd")
  writeLines(headMaster, outFile)
  
  lapply(seq_along(outFiles), function(i){
    cat("\n",file = outFile, append=TRUE)
    cat(
      paste0("```{r ", gsub(".*/.+/", "", RmdFiles[[i]]), ", child = \'", outFiles[[i]], "\'}\n```"),
      file = outFile, append=TRUE
      
    )
  })
  
  
}