## ----loadLibs, echo=FALSE, results="hide"-------------------------------------
library(foreach)
registerDoSEQ()

## ----init1,echo=FALSE,results="hide"------------------------------------------
sim <- function(a, b) 10 * a + b
avec <- 1:2
bvec <- 1:4

## ----for1---------------------------------------------------------------------
x <- matrix(0, length(avec), length(bvec))
for (j in 1:length(bvec)) {
  for (i in 1:length(avec)) {
    x[i,j] <- sim(avec[i], bvec[j])
  }
}
x

## ----foreach1-----------------------------------------------------------------
x <-
  foreach(b=bvec, .combine='cbind') %:%
    foreach(a=avec, .combine='c') %do% {
      sim(a, b)
    }
x

## ----foreach2-----------------------------------------------------------------
x <-
  foreach(b=bvec, .combine='cbind') %:%
    foreach(a=avec, .combine='c') %dopar% {
      sim(a, b)
    }
x

## ----foreach3-----------------------------------------------------------------
opts <- list(chunkSize=2)
x <-
  foreach(b=bvec, .combine='cbind', .options.nws=opts) %:%
    foreach(a=avec, .combine='c') %dopar% {
      sim(a, b)
    }
x

## ----init2, echo=FALSE, results="hide"----------------------------------------
sim <- function(a, b) {
  x <- 10 * a + b
  err <- abs(a - b)
  list(x=x, err=err)
}

## ----for2---------------------------------------------------------------------
n <- length(bvec)
d <- data.frame(x=numeric(n), a=numeric(n), b=numeric(n), err=numeric(n))

for (j in 1:n) {
  err <- Inf
  best <- NULL
  for (i in 1:length(avec)) {
    obj <- sim(avec[i], bvec[j])
    if (obj$err < err) {
      err <- obj$err
      best <- data.frame(x=obj$x, a=avec[i], b=bvec[j], err=obj$err)
    }
  }
  d[j,] <- best
}
d

## ----innercombine-------------------------------------------------------------
comb <- function(d1, d2) if (d1$err < d2$err) d1 else d2

## ----foreach4-----------------------------------------------------------------
opts <- list(chunkSize=2)
d <-
  foreach(b=bvec, .combine='rbind', .options.nws=opts) %:%
    foreach(a=avec, .combine='comb', .inorder=FALSE) %dopar% {
      obj <- sim(a, b)
      data.frame(x=obj$x, a=a, b=b, err=obj$err)
    }
d

## ----foreach5-----------------------------------------------------------------
library(iterators)
opts <- list(chunkSize=2)
d <-
  foreach(b=bvec, j=icount(), .combine='rbind', .options.nws=opts) %:%
    foreach(a=avec, i=icount(), .combine='comb', .inorder=FALSE) %dopar% {
      obj <- sim(a, b)
      data.frame(x=obj$x, i=i, j=j, err=obj$err)
    }
d

