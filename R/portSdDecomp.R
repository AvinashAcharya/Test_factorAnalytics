#' @title Portfolio variance/covariance decomposition report
#' 
#' @description Calculate factor contribution to portfolio variance based on fundamental factor model. This method takes fundamental factor model fit, "ffm" object, and portfolio weight as inputs and generates numeric summary and plot visualization. 
#' 
#' @importFrom zoo as.yearmon coredata index
#' @importFrom xts as.xts
#' 
#' @param ffmObj an object of class ffm returned by fitFfm.
#' @param weights a vector of weights of the assets in the portfolio. Default is NULL.
#' @param isPlot logical variable to generate plot or not.
#' @param stripLeft logical variable to choose the position of strip, "TRUE" for drawing strips on the left of each panel, "FALSE" for drawing strips on the top of each panel. Used only when isPlot = 'TRUE'
#' @param layout layout is a numeric vector of length 2 or 3 giving the number of columns, rows, and pages (optional) in a multipanel display.
#' @param scaleType scaleType controls if use a same scale of y-axis, choose from c('same', 'free')
#' @param ... other graphics parameters available in tsPlotMP(time series plot only) can be passed in through the ellipses 
#' @author Douglas Martin, Lingjie Yi
#' @examples 
#'
#' #Load fundamental and return data 
#' data("stocks145scores6")
#' dat = stocks145scores6
#' dat$DATE = as.yearmon(dat$DATE)
#' dat = dat[dat$DATE >=as.yearmon("2008-01-01") & dat$DATE <= as.yearmon("2012-12-31"),]
#'
#' #Load long-only GMV weights for the return data
#' data("wtsStocks145GmvLo")
#' wtsStocks145GmvLo = round(wtsStocks145GmvLo,5)                         
#'                                                                                  
#' #fit a fundamental factor model
#' require(factorAnalytics) 
#' fit <- fitFfm(data = dat, 
#'               exposure.vars = c("SECTOR","ROE","BP","PM12M1M","SIZE","ANNVOL1M","EP"),
#'               date.var = "DATE", ret.var = "RETURN", asset.var = "TICKER", 
#'               fit.method="WLS", z.score = TRUE)
#'
#' portSdDecomp(fit, wtsStocks145GmvLo, isPlot = FALSE)
#' portSdDecomp(fit, wtsStocks145GmvLo, isPlot = TRUE, add.grid = TRUE, 
#'              scaleType = 'same')
#' @export


portSdDecomp <- function(ffmObj, weights = NULL, isPlot = TRUE, layout =NULL, scaleType = 'free',
                      stripLeft = TRUE, ...) {
  
  if (!inherits(ffmObj, "ffm")) {
    stop("Invalid argument: ffmObjshould be of class'ffm'.")
  }
  
  which.numeric <- sapply(ffmObj$data[,ffmObj$exposure.vars,drop=FALSE], is.numeric)
  exposures.num <- ffmObj$exposure.vars[which.numeric]
  exposures.char <- ffmObj$exposure.vars[!which.numeric]
  exposures.char.name <- as.vector(unique(ffmObj$data[,exposures.char]))
  
  # get factor model returns from 
  facRet = ffmObj$factor.returns
  
  if(!length(exposures.char)){
    facRet = facRet[,-1]
  }
  
  # get parameters from the factor model fit  
  beta = ffmObj$beta
  n.assets = nrow(beta)
  asset.names <- unique(ffmObj$data[[ffmObj$asset.var]])
  TP = length(ffmObj$time.periods)
  
  # check if there is weight input
  if(is.null(weights)){
    weights = rep(1/n.assets, n.assets)
  }else{
    # check if number of weight parameter matches 
    if(n.assets != length(weights)){
      stop("Invalid argument: incorrect number of weights")
    }
    weights = weights[asset.names]
  }
  
  
  #Diagonal Covariance Matrix
  D = diag(ffmObj$resid.var)
  rownames(D) = names(ffmObj$resid.var)
  colnames(D) = names(ffmObj$resid.var)
  
  if(length(exposures.char)){
    dat <- ffmObj$data[ffmObj$data$DATE==ffmObj$time.periods[TP], ]
    B <- as.matrix(table(dat$TICKER,dat$SECTOR))
    B[B>0] <- 1
    B <- B[asset.names,]
  }else{
    B = c()
  }
  
  #calculate x = t(w) * B
  X = c()
  for(i in 1:TP){
    dat <- ffmObj$data[ffmObj$data$DATE==ffmObj$time.periods[i], ]
    beta <- as.matrix(dat[,exposures.num])
    rownames(beta) <- asset.names
    beta = cbind(beta,B)
    
    temp = as.data.frame(weights %*% beta)
    temp = cbind('Date'=ffmObj$time.periods[i],temp)
    X = rbind(X,temp)
  }
  X = as.xts(X[,-1],order.by = X[,1])
  
  Sig = ffmObj$factor.cov
    
  comRisk = as.xts(diag(coredata(X) %*% coredata(Sig)  %*% t(coredata(X))), order.by = index(X)) 
  names(comRisk) = 'Common Risk'
  speRisk = weights %*% D %*% weights
  names(speRisk) = 'Residual Risk'
  tolRisk = comRisk + as.xts(rep(speRisk, TP), order.by = index(X)) 
  
  fac.sig = diag(Sig)
  facRisk = X * X * fac.sig
  
  ret = list('FactorContribution' = facRisk, 'CommonRisk' = comRisk, 'ResidualRisk' = speRisk)  

  if(isPlot){
    
    if(is.null(layout)){
      layout = c(3,3)
    }
    
    tsPlotMP(facRisk, 
             main = "Time Series plot of portfolio Facotor Contribution", layout = layout, stripLeft = stripLeft, 
             scaleType = scaleType, ...)
    
  }  
  
  return(ret)
  
}