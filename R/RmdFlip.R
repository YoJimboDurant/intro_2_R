RmdFlip <- function(rmdFile){
  x <- readLines(rmdFile)
  artStart <- str_which(x, "<article>")
  artStop <- str_which(x,"<\\\\article>")
  
  if(length(artStart) != length(artStop)) stop("Article brackets length not equal check lines")
  
                        
  z <- paste(str_which(x, "<article>"),
      str_which(x, "<\\\\article>"), sep=":")
  
  zLines <- eval(parse(text=paste0("c(",paste(z, collapse = ","),")")))
  x[!1:length(x) %in% zLines]
}
