#' @title  t-stats and Plots for a fitted Fundamental Factor Model
#' 
#' @description
#'  Calculate and plot the time series of the t-statistic values and the
#'  number of risk indices with significant t-stats for a fundamentally fit object.
#' @importFrom xts xts
#' @importFrom zoo plot.zoo
#' @importFrom zoo as.yearmon
#' @importFrom factorAnalytics fitFfm
#' @importFrom graphics barplot
#' @importFrom lattice panel.abline xyplot panel.xyplot
#'  
#' @param ffmObj   an object of class \code{ffm} produced by \code{fitFfm}
#' @param isPlot   logical. If \code{FALSE} no plots are displayed.
#' @param isPrint  logical. if \code{TRUE}, the time series of the computed factor model values is printed. Default is \code{FALSE}, 
#' @param myColor  length 2 vector specifying the plotting color for t-stats plot and for barplot 
#'                 respectively. Default is \code{c("black", "cyan")}
#' @param lwd      line width relative to the default. Default is 2.
#' @param digits   an integer indicating the number of decimal places to be used for rounding. Default is 2.
#' @param z.alpha  critical value corresponding to the confidence interval. Default is 1.96 i.e 95\% C.I
#' @param layout   numeric vector of length 2 or 3 giving the number of columns, rows, and pages (optional) in the xyplot of t-statistics. Default is c(2,3).
#' @param type     character. Type of the xyplot of t-statistics; \code{"l"} for lines, \code{"p"} for points, \code{"h"} for histogram like (or high-density) vertical lines
#'                 and \code{"b"} for both. Deafault is \code{"h"}.

#' @param ...     potentially further arguments passed.
#' 
#' @author Doug Martin, Avinash Acharya
#' 
#' @return \code{ffmTstats} plots the t-stats and significant t-stats values  if \code{isPlot} is \code{TRUE} and returns a list with following components:
#' \item{tstats}{ an xts object of t-stats values.}
#' \item{z.alpha}{ critical value corresponding to the confidence interval.}
#' @examples 
#'  
#'  data("factorDataSetDjia5Yrs")
#'
#'#Fit a Ffm with style factors only
#'  require(factorAnalytics)
#'  fit <- fitFfm(data = factorDataSetDjia5Yrs,exposure.vars = c("MKTCAP","ENTVAL","P2B","EV2S"),
#'              date.var = "DATE", ret.var = "RETURN", asset.var = "TICKER", fit.method="WLS",z.score = TRUE)
#'
#'#Compute time series of t-stats and number of significant t-stats 
#'  stats = ffmTstats(fit, isPlot = TRUE, lwd = 2, myColor = c("blue", "blue"), z.alpha =1.96)
#'
#'# Fit a Ffm with sector factors as well as style factors
#'  fit2 <- fitFfm(data = factorDataSetDjia5Yrs,exposure.vars = c("SECTOR","MKTCAP","ENTVAL","P2B","EV2S"),
#'               date.var = "DATE", ret.var = "RETURN", asset.var = "TICKER", fit.method="WLS",z.score = TRUE)
#'
#'#Compute time series of t-stats and number of significant t-stats 
#'  stats = ffmTstats(fit2, isPlot = TRUE, z.alpha =1.96)  
#' 
#' @export

ffmTstats<- function(ffmObj, isPlot = TRUE, isPrint = FALSE, myColor = c("black", "cyan"),lwd =2, digits =2, z.alpha = 1.96, layout =c(2,3),type ="h", ... )
{
  
  # CREATE TIME SERIES OF T-STATS
  time.periods = length(ffmObj$time.periods)
  n.exposures =  length(ffmObj$exposure.vars)
  tstats = lapply(seq(time.periods), function(a) summary(ffmObj)$sum.list[[a]]$coefficients[,3])
  secNames = names(tstats[[1]])
  tstats = matrix(unlist(tstats), byrow = TRUE, nrow = time.periods)
  colnames(tstats)=secNames
  tstatsTs = xts(tstats,order.by=as.yearmon(names(ffmObj$r2)))
  
  # COUNT NUMBER OF RISK INDICES WITH SIGNIFICANT T-STATS EACH MONTH
  sigTstats = as.matrix(rowSums(ifelse(abs(tstats) > z.alpha,1,0)))
  sigTstatsTs = xts(sigTstats,order.by=as.yearmon(names(ffmObj$r2)))
  
  if(isPlot)
  {
    hlines1 = rep(z.alpha, n.exposures+1)
    hlines2 = rep(-z.alpha, n.exposures+1)
    panel =  function(...){
      panel.abline(h=hlines1,lty = 3, col = "red")
      panel.abline(h=hlines2,lty = 3, col = "red")
      panel.xyplot(...)
    }
    #plot.new()
    # PLOT T-STATS WITH XYPLOT

   plt1 <- xyplot(tstatsTs, panel = panel, type = type, scales = list(y = list(cex = 1), x = list(cex = 1)),
                 layout = layout,  main = "t-statistic values", col = myColor[1], lwd = lwd, strip.left = T, strip = F)
   print(plt1)

    # PLOT NUMBER OF RISK INDICES WITH SIGNIFICANT T-STATS EACH MONTH
   barplot(sigTstatsTs,col = myColor[2], main = "Number of Risk Indices with significant t-stats")
   
  }
  out = list("tstats" =round(tstatsTs, digits), "z.alpha" =z.alpha)
  if(isPrint){print(out)}else invisible(out)
    
}
