RmdFlip <- function(rmdFile, startTag = "<article>", endTag = "<\\\\article>",
                    tagOnly = FALSE){
  require(stringr)
  x <- readLines(rmdFile)
  if(tagOnly){
    x <-  gsub(paste(startTag, endTag, sep="|"), "", x)
    
  }else{
    artStart <- str_which(x, startTag)
    artStop <- str_which(x,endTag)
    
    if(length(artStart) != length(artStop)) stop("Tag brackets length not equal, check lines")
    
    
    z <- paste(str_which(x, startTag),
               str_which(x, endTag), sep=":")
    
    zLines <- eval(parse(text=paste0("c(",paste(z, collapse = ","),")")))
    x <- x[!1:length(x) %in% zLines]
  }
  
  return(x)
}
