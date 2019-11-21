## Read SMS output from details.out
## Labels: Stock numbers, Fishing mortality, etc.

read.output <- function(file, label)
{
  txt <- readLines(file)
  beg <- which(txt==label) + 2
  end <- c(which(txt=="")-1, length(txt))
  end <- min(end[end > beg])
  txt <- txt[beg:end]
  year <- grep("# Year", txt, value=TRUE)
  year <- as.integer(sub("# Year:", "", year))
  age <- grep("# age", txt, value=TRUE)[1]
  age <- scan(text=age, what="", quiet=TRUE)[-(1:2)]
  out <- read.table(text=txt)
  out <- data.frame(rep(year,each=2), 1:2, out)
  names(out) <- c("Year", "Step", age)
  out
}


## Convert a "step" table to long format
## Input is a data frame like this: Year Step 0 1 2 3 4+
## Output is a data frame like this: Year Step Age Value
## For convenience, user can pass a name for the last column:
##   step2long(catage, names="Catch")

step2long <- function(x, names=c("Year","Step","Age","Value"))
{
  if(length(names) == 1)
    names <- c("Year", "Step", "Age", names)
  row.names(x) <- paste(x[[1]], x[[2]], sep=":")
  x <- x[-(1:2)]
  y <- as.data.frame(as.table(as.matrix(x)))
  y1 <- sub(":.*", "", y[[1]])
  y2 <- sub(".*:", "", y[[1]])
  z <- data.frame(y1, y2, y[-1])
  z <- type.convert(z, as.is=TRUE)
  names(z) <- names
  z <- z[order(z[[1]], z[[2]], z[[3]]),]
  row.names(z) <- NULL
  z
}


## Convert a "step" table to crosstab format
## Input is a data frame like this: Year Step 0 1 2 3 4+
## Output is a data frame like this:
##   Year 0.1 0.2 1.1 1.2 2.1 2.2 3.1 3.2 4+.1 4+.2

step2xtab <- function(x)
{
  x <- step2long(x)
  y <- xtabs(x[[4]] ~ x[[1]] + paste(x[[3]],x[[2]],sep="."))
  z <- xtab2taf(y)
  z
}


## Convert sandeel table:
## - crosstab
## - remove age 0.1
## - calculate means
## - round numbers
## - full column names
## Note that colnames have commas, so write.taf with quotes=TRUE

sandeel.table <- function(x, digits)
{
  x <- step2xtab(x)
  x <- x[names(x) != "0.1"]
  x <- rbind(x, colMeans(x))
  x[nrow(x),1] <- "arith. mean"
  x <- rnd(x, -1, digits)
  age <- c("0", "1", "1", "2", "2", "3", "3", "4+", "4+")
  half <- c("2nd", "1st")
  names(x) <- c("Year", paste0("Age ", age, ", ", half, " half"))
  x
}


## Increase text size of lattice plot
## This function has been added to the icesTAF package (25 Mar 2019)
## and will soon be a part of the icesTAF 2.3-0 version on CRAN.

zoom <- function(obj, cex=1.8, cex.main=1.3*cex, cex.lab=1.1*cex, cex.axis=cex,
                 cex.strip=cex, cex.symbol=cex, cex.sub=0.7*cex,
                 cex.legend=0.7*cex)
{
  if(class(obj) != "trellis")
    stop("'obj' must be a trellis object")
  suppressWarnings({
    obj$main$cex <- cex.main
    obj$xlab$cex <- cex.lab
    obj$ylab$cex <- cex.lab
    obj$x.scales$cex <- rep(cex.axis, length(obj$x.scales$cex))
    obj$y.scales$cex <- rep(cex.axis, length(obj$y.scales$cex))
    obj$par.strip.text$cex <- cex.strip
    obj$par.settings$superpose.symbol$cex <- cex.symbol
    obj$sub$cex <- cex.sub
    if(!is.null(obj$legend))
      obj$legend$right$args$cex <- cex.legend
  })
  print(obj)
}
