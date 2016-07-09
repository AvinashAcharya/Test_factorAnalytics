#' @title  R-squared and Adjusted R-squared for a Portfolio
#'
#' @description Calcluate and plot the R-squared and Adjusted R-squared for a portfolio of assets
#'
#' @importFrom zoo as.yearmon
#' @importFrom factorAnalytics fitFfm
#' @importFrom graphics barplot
#' 
#' @param ffmObj   an object of class \code{ffm} produced by \code{fitFfm}
#' @param rsq      logical; if \code{TRUE}, R-squared values are computed for the portfolio. Default is \code{TRUE}.
#' @param rsqAdj   logical; if \code{TRUE}, Adjusted R-squared values are computed for the portfolio. Default is \code{FALSE}.
#' @param VIF      logical; if \code{TRUE}, Variance Inflation factor is calculated. Default is \code{FALSE}.
#' @param digits   an integer indicating the number of decimal places to be used for rounding. Default is 2.
#' @param ...      potentially further arguments passed.
#' @author Avinash Acharya
#'
#' @return \code{portRsqr} returns the sample mean and plots the time series of corresponding R squared values for the portfolio
#'                        depending on the values of \code{rsq} and \code{rsqAdj}.
#'
#' @examples
#'
#' #Load the data
#'  data("factorDataSetDjia5Yrs")
#'
#' #Fit a Ffm
#' require(factorAnalytics)
#'  fit <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN",
#'               date.var="DATE", exposure.vars="SECTOR")
#'
#' #Calcuate and plot the portfolio R-squared values
#'  portRsqr(fit)
#'
#'
#'
#' @export

# Not the final version
portRsqr <- function(ffmObj, rsq=T, rsqAdj=F, VIF=F, digits=2, ...)
{
  # set defaults and check input validity
  if (!inherits(ffmObj, "ffm"))
  {
    stop("Invalid argument: Object should be of class'ffm'.")
  }
  
  if (!(rsq) && !(rsqAdj))
  {
    stop("Invalid arguments: Inputs rsq and rsqAdj both cannot be False.")
  }
  
  n.assets <- length(ffmObj$asset.names)
  r2<- ffmObj$r2
  
  if(rsq)
  {
    barplot(r2,las=2,col=5,
            names.arg= as.yearmon(names(r2)),
            cex.names=0.5,
            main="R-squared Values for the Portfolio")
    r2.mean<- round(mean(r2),digits = digits)
    names(r2.mean) <- "Mean R-Square"
    out<- r2.mean
  }
  if(rsqAdj)
  {
    K <- length(ffmObj$factor.name)
    p <- K-1
    adj.r2 <- 1 - ((n.assets - 1)*(1- r2) / (n.assets - p - 1))
    barplot(adj.r2,las=2,col=5,
            names.arg= as.yearmon(names(r2)),
            cex.names=0.5,
            main="Adjusted R-squared Values for the Portfolio")
    adj.r2.mean<- round(mean(adj.r2),digits = digits)
    names(adj.r2.mean) <- "Mean Adj R-Square"
    out<- adj.r2.mean
  }
  if(rsqAdj && rsq)
  {
    out<- c(r2.mean, adj.r2.mean)
  }
  if(VIF)
  {
    object = ffmObj$factor.returns  
    object <- as.matrix(object)
    ncols <- dim(object)[2]
    vif = lapply(seq(ncols), function(x) 1/(1 - summary(lm(object[, x] ~ 
                                                             object[, -x]))$r.squared))
    names(vif) <- dimnames(object)[[2]]
    vif = unlist(vif)
    out<- c(out, list("VIF" = vif))
    
  }
  print(out)
}
