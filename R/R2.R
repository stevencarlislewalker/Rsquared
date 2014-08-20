##' R squared statistics
##' 
##' A package for computing R squared statistics
##'
##' @docType package
##' @name Rsquared
##' @aliases Rsquared package-Rsquared Rsquared-package
NULL

##' R squared
##'
##' Calculate R squared statistics from fitted model objects
##'
##' So far there are six different types of R squared statistics.
##' 
##' @param object Fitted model object
##' @param ... Additional parameters
##' @rdname R2
##' @aliases R2adj R2gcv R2press R2fpe
##' @export
R2 <- function(object,...){
  UseMethod("R2")
}

##' @rdname R2
##' @export
R2adj <- function(object,...){
  UseMethod("R2adj")
}

##' @rdname R2
##' @export
R2adj2 <- function(object,...){
  UseMethod("R2adj2")
}

##' @rdname R2
##' @export
R2gcv <- function(object,...){
  UseMethod("R2gcv")
}

##' @rdname R2
##' @export
R2press <- function(object,...){
  UseMethod("R2press")
}

##' @rdname R2
##' @export
R2fpe <- function(object,...){
  UseMethod("R2fpe")
}

##' @export
R2.default <- function(object,...){
    stop("no method for this kind of object")
}

##' @export
R2adj.default <- function(object,...){
    stop("no method for this kind of object")
}

##' @export
R2adj2.default <- function(object, ...){
    stop("no method for this kind of object")
}

##' @export
R2gcv.default <- function(object,...){
    stop("no method for this kind of object")
}

##' @export
R2press.default <- function(object,...){
    stop("no method for this kind of object")
}

##' @export
R2fpe.default <- function(object,...){
    stop("no method for this kind of object")
}

##' @export
R2.numeric <- function(object, x, ...){
  if(is.array(object)) stop("no method for this kind of object")
  R2.(object, x)
}

##' @export
R2adj.numeric <- function(object, x, ...){
  if(is.array(object)) stop("no method for this kind of object")
  n <- length(object)
  1 - ((n-1)/(n-2))*(1-R2.(object, x))
}

##' @export
R2press.numeric <- function(object, x, ...){
  if(is.array(object)) stop("no method for this kind of object")
  m <- lm(object ~ x)
  R2press(m)
}

##' @export
R2gcv.numeric <- function(object, x, ...){
  if(is.array(object)) stop("no method for this kind of object")
  n <- length(object)
  ## 1 - ((n^2)/((n-2)*(n-1)))*(1-R2.(object, x))
  1 - ((n*(n-1))/((n-2)^2))*(1-R2.(object, x))
}

##' @export
R2.lm <- function(object,...)
  summary(object)$r.sq


##' @export
R2adj.lm <- function(object,...){
  summary(object)$adj.r.sq
  #y <- model.response(model.frame(object))
  #if(is.matrix(y)) stop("can not do multivariate models yet")
  #else n <- length(y)
  #1 - ((n-1)/(n-2))*(1-summary(object)$r.sq)
}

##' @export
R2adj2.lm <-  function(object,...){
  y <- model.response(model.frame(object))
  if(is.matrix(y)) stop("can not do multivariate models yet")
  else n <- length(y)
  1 - ((n-1)/(n-2))*(1-summary(object)$adj.r.sq)
}

##' @export
R2press.lm <- function(object,...){
  pr <- resid(object)/(1 - lm.influence(object)$hat)
  return(1 - mean(pr^2)/var(model.response(model.frame(object))))
}

##' @export
R2gcv.lm <- function(object,...){
  y <- model.response(model.frame(object))
  if(is.matrix(y)) stop("can not do multivariate models yet")
  else n <- length(y)
  p <- length(object$coef)
  1 - (((n-1)*n)/((n-p)^2))*(1-summary(object)$r.sq)
}

##' @export
R2fpe.lm <- function(object,...){
  y <- model.response(model.frame(object))
  if(is.matrix(y)) stop("can not do multivariate models yet")
  else n <- length(y)
  summary(object)$adj.r.sq*((n+2)/(n+1)) - (1/(n+1))
}
