#' @title  R-squared and Adjusted R-squared for a Portfolio
#'
#' @description Calcluate and plot the R-squared and Adjusted R-squared for a portfolio of assets
#'
#' @importFrom zoo as.yearmon
#' @importFrom factorAnalytics fitFfm
#' @importFrom graphics barplot
#' @importFrom stats lm
#' @importFrom xts xts
#' 
#' @param ffmObj   an object of class \code{ffm} produced by \code{fitFfm}
#' @param rsq      logical; if \code{TRUE}, R-squared values are computed for the portfolio. Default is \code{TRUE}.
#' @param rsqAdj   logical; if \code{TRUE}, Adjusted R-squared values are computed for the portfolio. Default is \code{FALSE}.
#' @param VIF      logical; if \code{TRUE}, Variance Inflation factor is calculated. Default is \code{FALSE}.
#' @param digits   an integer indicating the number of decimal places to be used for rounding. Default is 2.
#' @param ...      potentially further arguments passed.
#' @param isPrint  logical. if \code{TRUE}, the time series of the output is printed. Default is \code{FALSE}, 
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
#'  fit1 <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN",
#'               date.var="DATE", exposure.vars=c("SECTOR", "P2B", "EV2S", "MARKETCAP"))
#'
#' #Calcuate and plot the portfolio R-squared values and VIF for the continous variables
#'  portRsqr(fit1, VIF=TRUE)
#' @export

# Not the final version
portRsqr <- function(ffmObj, rsq=T, rsqAdj=F, VIF=F, digits=2,isPrint=F, ...)
{
  # set defaults and check input validity
  if (!inherits(ffmObj, "ffm"))
  {
    stop("Invalid argument: Object should be of class'ffm'.")
  }
  
  if (!(rsq) && !(rsqAdj) && !(VIF))
  {
    stop("Invalid arguments: Inputs rsq, rsqAdj and VIF cannot be False.")
  }
  
  n.assets <- length(ffmObj$asset.names)
  r2<- ffmObj$r2
  out<- list()
  ret<- list()
  
  if(rsq)
  {
    barplot(r2,las=2,col=5,
            names.arg= as.yearmon(names(r2)),
            cex.names=0.5,
            main="R-squared Values for the Portfolio")
    r2.mean<- round(mean(r2),digits = digits)
    names(r2.mean) <- "Mean R-Square"
    out<- r2.mean
    ret<- list("R-squared" = r2)
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
    ret<- list("Adj.r-Squared" = adj.r2)
  }
  if(rsqAdj && rsq)
  {
    out<- c(r2.mean, adj.r2.mean)
    ret<- list("R-Squared"= r2, "Adj.R-Squared" = adj.r2)
  }
  if(VIF)
  {
    exposure.vars= ffmObj$exposure.vars
    which.numeric <- sapply(ffmObj$data[,exposure.vars,drop=FALSE], is.numeric)
    exposures.num <- exposure.vars[which.numeric]
    object = ffmObj$data[exposures.num]  
    object <- as.matrix(object)
    ncols <- dim(object)[2]
    time.periods = length(ffmObj$time.periods)
    vifs = matrix(0, nrow = time.periods, ncol = ncols)
    for(i in 1:60)
      {
      vifs[i,1:ncols] = sapply(seq(ncols), function(x)
                                1/(1 - summary(lm(object[((i-1)*n.assets+1) : (i*n.assets), x] ~ 
                                                  object[((i-1)*n.assets+1) :(i*n.assets), -x]))$r.squared))
      }
    colnames(vifs) <- dimnames(object)[[2]]
    vifs.xts = xts(vifs, order.by = ffmObj$time.periods)
    vifs.mean = colMeans(vifs.xts)
    
    for(i in 1:ncols)
      {
        barplot(vifs.xts[,i],las=2,col=5,
                names.arg= as.yearmon(index(vifs.xts)),
                cex.names=0.5,
                main=paste("Varinace Inflation Factor - ", exposures.num[i] ))
      }
    out<- append(out, list("Mean.VIF" = vifs.mean))
    ret<- append(ret, list("VIF" = vifs.xts))
  }

  if(isPrint)
    {
      print(out)
      print(ret)
      return(c(out, ret))
    }
  print(out)
}

