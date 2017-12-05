## ----get_pack, echo=FALSE, warning=FALSE, results='hold', message=FALSE----
rm(list=ls())

mypacks <- c("stringr", "knitr", 
             "miniCRAN", "igraph",
             "kableExtra", "ctv",
             "EpiCurve", "Epi",
             "compareGroups", "epiR",
             "pander", "DiagrammeR")

x <- sapply(mypacks[!mypacks %in% installed.packages()], install.packages)


library(stringr)
library(knitr)
library(miniCRAN)
library(igraph)
library(kableExtra)
opts_chunk$set(concordance=TRUE)
opts_knit$set(self.contained=FALSE)

#
options(width=60)

set.seed(13456)

today <- format(Sys.Date(), "%m-%d-%Y")
myDepFun <- function (pkg, availPkgs, repos = getOption("repos"), type = "source", 
                      suggests = TRUE, enhances = FALSE, includeBasePkgs = FALSE, 
                      ...) 
{
  if (!requireNamespace("igraph")) {
    stop(igraphNotAvailableMessage)
  }
  if (missing(availPkgs)) {
    availPkgs <- pkgAvail(repos = repos, type = type)
  }
  pkgs <- availPkgs
  rownames(pkgs) <- as.vector(pkgs[, "Package"])
  allPkgs <- rownames(pkgs)
  if (!length(allPkgs)) 
    stop("no packages in specified repositories")
  pkgEdge <- function(p, type = c("Imports", "Depends", "LinkingTo"), 
                      pdb) {
    do.call(rbind, lapply(type, function(t) miniCRAN:::addDepType(p, 
                                                                  t, pdb = pdb)))
  }
  pkgEdges <- function(pp, type = c("Imports", "Depends", 
                                    "LinkingTo"), pdb) {
    do.call(rbind, lapply(pp, pkgEdge, type = type, pdb = pdb))
  }
  pkg_orig <- pkg
  if (suggests) {
    edges1 <- pkgEdges(pkg, type = c("Suggests"), availPkgs)
    p_sug <- unique(unlist(tools::package_dependencies(pkg, 
                                                       db = availPkgs, which = "Suggests", recursive = FALSE)))
    pkg <- unique(c(p_sug, pkg))
  }
  if (enhances) {
    edges2 <- pkgEdges(pkg_orig, type = c("Enhances"), availPkgs)
    p_enh <- unique(unlist(tools::package_dependencies(pkg_orig, 
                                                       db = availPkgs, which = "Enhances", recursive = FALSE)))
    pkg <- unique(c(p_enh, pkg))
  }
  p_dep <- unique(unlist(tools::package_dependencies(pkg, 
                                                     db = availPkgs, which = c("Imports", "Depends", "LinkingTo"), 
                                                     recursive = TRUE)))
  pkg <- unique(c(p_dep, pkg))
  edges <- pkgEdges(pkg, type = c("Imports", "Depends", "LinkingTo"), 
                    availPkgs)
  if (suggests) {
    edges <- rbind(edges, edges1)
  }
  if (enhances) {
    edges <- rbind(edges, edges2)
  }
  nedges <- nrow(edges)
  if (nedges && !includeBasePkgs) 
    edges <- edges[!(edges[["dep"]] %in% basePkgs()), ]
  vert <- unique(c(pkg_orig, edges[["dep"]], edges[["package"]]))
  ret <- igraph::graph.data.frame(d = edges, directed = TRUE, 
                                  vertices = vert)
  class(ret) <- c("pkgDepGraph", "igraph")
  attr(ret, "pkgs") <- pkg_orig
  ret
}

# code to get number of packages
xLines <- readLines("https://cran.r-project.org/web/packages/")
xContrib <- grep("Currently, the CRAN package repository features", xLines, value=TRUE)
xPacks <- prettyNum(str_extract(xContrib, "[0-9]+"), big.mark=",")

# code to get base packages
packLines <- readLines("https://stat.ethz.ch/R-manual/R-devel/doc/html/packages.html")
defaultPacks <- XML::readHTMLTable(packLines, col.names=FALSE)[[1]]
names(defaultPacks) <- c("Package", "Description")

## ----rconsole, echo =FALSE, fig.cap="R in interactive use", warning=FALSE, message=FALSE----
library(DiagrammeR)

nodes <-
  create_node_df(
    n = 3,
    type = "a",
    label = c("Computer", "R", "User"),
    color = "lightblue")

edges1 <- create_edge_df(from=c(3,2, 1,2),
                       to = c(2,1, 2,3),
                       rel = "related",
                       label = c("R Expressions","Binary Functions", "Binary Results","Text, Graphics"))

mygraph <- create_graph(nodes_df = nodes,
                        edges_df = edges1)
render_graph(mygraph, layout = "tree")


## ---- eval=FALSE---------------------------------------------------------
## 1 + 1  #Add 1 plus 1
## sqrt(5000) # square root 5000
## 3/3
## 4*2
## 2*pi
## pi%%pi
## 5 %/% 2

## ----arthmatic, echo=FALSE-----------------------------------------------
dfx <- data.frame(
      Operator = c("+", "-", "\\text{*}", "/", "^", "\\text{**}", "%%", "%/%"),
      Operation = c("addition", "subtraction", "multiplication", 
                    "division", "exponentiation", "exponentiation",
                    "modulus", "integer division")
      )
    kable(dfx, escape = FALSE)

## ----eee-----------------------------------------------------------------
1.3e3 ; 1.3e-3; 1e4 ; 1e-4

## ----log2----------------------------------------------------------------
log(9, base = 3)

## ----trig, fig.height=3.5------------------------------------------------
plot(x = seq(0, 2, 0.1) * pi, y = sin(seq(0, 2, 0.1) * pi), 
     type="l", xlab = "radians", ylab = "sin(x)")
abline(h=0, lty = 3, col = "grey") 

## ----error=TRUE----------------------------------------------------------
x = 7 # same as:
x <- 7 
print(x)
print(X) # error!
y <- 10 - 9 

## ---- eval = FALSE-------------------------------------------------------
## objects()

## ------------------------------------------------------------------------
rm(list=ls())

## ----logic---------------------------------------------------------------
x <- 1
x > 2
x < 0
x >= 4
x <= 5
x == 1
x != 1
x <= 1 & x < 2
x == 1 | x < 0

## ----logics, echo=FALSE--------------------------------------------------
dfx <- data.frame(
      Operator = c("<", "<=", ">", ">=", "==", "!=", "!", "|", "&", "||", "&&"),
      Operation = c("less than", "less than or equal", "greater than", 
                    "greater than or equal", 
                    "equal", "not equal", "NOT",
                    "OR", "AND", "ALL OR", "ALL AND")
      )
    kable(dfx, escape = FALSE)

## ----atomic--------------------------------------------------------------
#Variable classes
x <- 1.7
class(x)
typeof(x)
y <- 'Hello World!'
class(y)
z <- TRUE
class(z)

## ----more_class----------------------------------------------------------
a_complex_number <- 1+0i
class(a_complex_number)
xx <- charToRaw("I am not raw")
xx
class(xx)
rawToChar(xx)

## ----wtf-----------------------------------------------------------------
1e-324 == 0 ; 1e-323 == 0
1 - 1e-16 == 1 ;1 - 1e-17 == 1
a <- sqrt(2) ; a^2 == 2

## ----vector--------------------------------------------------------------
X <- c(10.4, 5.6, 3.1, 6.4, 21.7)   # creates a numeric vector
1/X                                
Y <- 1/X
X**2
summary(Y)

## ----sequences-----------------------------------------------------------
#regular sequences
seq(from = -5, to = 5, by = 2)
1:15

## ----par-----------------------------------------------------------------
set.seed(123)
randomNums <- rnorm(1000)
summary(randomNums); mean(randomNums); sd(randomNums)
fivenum(randomNums)

## ----sumVect, fig.height = 3.5-------------------------------------------
oldpar <- par(mfrow = c(1,2))
boxplot(randomNums, main = "Boxplot", col="lightblue")
hist(randomNums, main = "Histogram", col="lightblue", freq=FALSE) 
lines(density(randomNums), main = "Density")
par(oldpar)

## ----vector2, error=TRUE-------------------------------------------------
Stooges <- c("Moe", "Larry", "Curly")    
lastNames <- c("Howard", "Fine", "Howard")
1/Stooges # Error
ThreeStooges <- paste(Stooges, lastNames) 
ThreeStooges # prints
plot(ThreeStooges)
table(ThreeStooges)

## ----vector3-------------------------------------------------------------
logicVector1 <- c(TRUE, FALSE, FALSE, TRUE)    
logicVector2 <- c(T,F,F,T) #T or F means TRUE or FALSE
(logicVector3 <- X < 10)
summary(logicVector3)
(logicVector4 <- lastNames == "Howard")

## ----logicplot, fig.height=3.5-------------------------------------------
oldpar <- par(mfrow=c(1,4))
plot(logicVector1, pch = 19)
plot(logicVector2, pch = 19)
plot(logicVector3, pch = 19)
plot(logicVector4, pch = 19)
par(oldpar)

## ----vector4-------------------------------------------------------------
V <- 1:10
V[V<5]
lastNames[1]
lastNames[c(1,3)]
logicVector1[logicVector1]

## ----vectorfactor--------------------------------------------------------
factorStates <- as.factor(sample(sample(state.abb, 10), 100, 
                                 replace = TRUE))  
factorStates
summary(factorStates)
# table(factorStates)

## ----vectorfactor2-------------------------------------------------------
plot(factorStates)

## ----dangerfactor--------------------------------------------------------
as.numeric(factorStates)
class(c("One", 2, 3, "four"))
(dontdothis <-as.numeric(factor(c(10,20,30))))
(lookout <- as.numeric(c("One", 2, 3, "four")))
(dothis <- as.numeric(as.character((factor(c(10,20,30))))))

## ----date1---------------------------------------------------------------
(adate <- as.Date("11/11/2011", format = "%m/%d/%Y"))
class(adate)
(bdate <- strptime("11-17-2014 13:30", 
              format = "%m-%d-%Y %H:%M", tz = "US/Eastern"))
class(bdate) ; bdate$wday; format(bdate, "%a")

## ----vector_manip--------------------------------------------------------
ThreeStooges[2] <- "James Durant"
ThreeStooges
X[X<5] <- 0
X
X[X<5] <- rnorm(100) #warning
X[X>50] <- rnorm(100) 

## ----nas-----------------------------------------------------------------
x <- c(1:3,NA)
is.na(x)
x == NA

## ----makeanna------------------------------------------------------------
mydata <- c( 1, 2.4, pi, 40, -999)
mean(mydata)
is.na(mydata) <- mydata == -999
mean(mydata)
mean(mydata, na.rm=TRUE)

## ----notanumber----------------------------------------------------------
x[5] <- 0/0
x[6] <- Inf - Inf
is.na(x)
is.nan(x)

## ----vectorcals----------------------------------------------------------
X1 <- c(1,2,3,4,5)
X2 <-c(1,2,3)
X1 + X2
X2 <- c(X2, 4, 5)
X1 + X2

## ----regex---------------------------------------------------------------
grep("A", state.name)                        
grep("A", state.name, value=TRUE)           
grep("A", state.name, ignore.case = TRUE)  
grep("A", state.name, ignore.case = TRUE, 
     invert = TRUE, value = TRUE )         
grep("^A", state.name, value=TRUE)         
grep("a$", state.name, value=TRUE)         
grep("^A|^V", state.name, value = TRUE)    

(ThreeStooges <- gsub("James", "Jim", ThreeStooges))  

## ----regex2--------------------------------------------------------------
grep("^A.+b", state.name, value = TRUE)  
grep("9.*11", c("911", "9/11", "9/123116/1212"))
grep("9.*11$", c("911", "9/11", "9/123116/1212"))                   

## ----dfx-----------------------------------------------------------------
library(readxl)
 Age_sl <- read_excel(
   "../data/AGE_Surveillance_Log.xls", 
   skip = 7)
names(Age_sl)[
  grepl(
    "^X__[0-9]|Y/N__[0-9]|#|^.F$|^Y/N$",names(Age_sl))
  ] <-
  c(
    "Age", "Sex", "Pax_or_Crew", "Cabin", "Meal_seat",
    "Crew_position", "Diarrhea_n", "Vomiting_n",
    "Fever", "Temp", "Cramps", "Headache", "Myalgia",
    "Spec_req", "Spec_rec", "AD_meds", "Reportable",
    "UI"
  )

## ----vectdf--------------------------------------------------------------
summary(Age_sl$Age)
table(Age_sl$Sex)

