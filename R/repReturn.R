#' @title Portfolio return reports for risk decomposition and performance analysis
#' 
#' @description 
#' 
#' 
#'Not the final version 
#'


repReturn <- function(object, weights = NULL, ...){
  # check input object validity
  if (!inherits(object, c("tsfm", "sfm", "ffm"))) {
    stop("Invalid argument: Object should be of class 'tsfm', 'sfm' or 'ffm'.")
  }
  UseMethod("repReturn")
}


repReturn.ffm <- function(object, weights = NULL, ...) {
  
  which.numeric <- sapply(object$data[,object$exposure.vars,drop=FALSE], is.numeric)
  exposures.num <- object$exposure.vars[which.numeric]
  exposures.char <- object$exposure.vars[!which.numeric]
  exposures.char.name <- as.vector(unique(object$data[,exposures.char]))
  
  # get factor model returns from 
  facRet = object$factor.returns
  
  if(!length(exposures.char)){
    alpha = facRet[,1]
    colnames(alpha) = 'Alpha'
    facRet = facRet[,-1]
  }else{
    alpha = c()
  }
  sig = object$residuals
  
  # get parameters from the factor model fit  
  beta = object$beta
  n.assets = nrow(beta)
  asset.names <- unique(object$data[[object$asset.var]])
  TP = length(object$time.periods)
  
  # check if there is weight input
  if(is.null(weights)){
    weights = rep(1/n.assets, n.assets)
  }
  # check if number of weight parameter matches 
  if(n.assets != length(weights)){
    stop("Invalid argument: incorrect number of weights")
  }
  
  #portfolio residuals
  sig.p <- sig * weights
  sig.p <- as.xts(rowSums(coredata(sig.p)), order.by = index(sig.p))
  colnames(sig.p) = 'Residuals'
  
  
  if(length(exposures.char)){
    dat <- object$data[object$data$DATE==object$time.periods[TP], ]
    B <- as.matrix(table(dat$TICKER,dat$SECTOR))
    B[B>0] <- 1
    B <- B[asset.names,]
  }else{
    B = c()
  }
  
  #calculate x = t(w) * B
  X = c()
  for(i in 1:TP){
    dat <- object$data[object$data$DATE==object$time.periods[i], ]
    beta <- as.matrix(dat[,exposures.num])
    rownames(beta) <- asset.names
    beta = cbind(beta,B)
    
    temp = as.data.frame(weights %*% beta)
    temp = cbind('Date'=object$time.periods[i],temp)
    X = rbind(X,temp)
  }
  X = as.xts(X[,-1],order.by = X[,1])
  
  rk = as.xts(coredata(X) * coredata(facRet), order.by = index(sig.p)) 
  facRet.p = as.xts(rowSums(coredata(rk)), order.by = index(sig.p))
  colnames(facRet.p) = 'facRet'
  
  if(!length(exposures.char)){
    ret.p = alpha + facRet.p + sig.p
  }else{
    ret.p =facRet.p + sig.p
  }  
  
  colnames(ret.p) = 'Return'
  
  dat = merge(ret.p, alpha, facRet.p, rk, sig.p)
  
  boxplot(coredata(dat),col=5,
          cex.names=0.5,
          main=paste("Distribution of Factors"))
  
  tsPlotMP(dat[,c('Return','Alpha','facRet','Residuals')], main = "Time Series of Factors", scaleType = "free", layout = c(3,3))
  tsPlotMP(dat[,c('facRet',exposures.num,'Residuals')], main = "Time Series of Risk Indices", scaleType = "free", layout = c(3,3))
  tsPlotMP(dat[,c(exposures.char.name)], main = "Time Series of Sectors", scaleType = "free", layout = c(3,4))
  
  # tabular report 
  sum = summary(coredata(dat))
  print(sum)
}


# plotting
library(lattice)

tsPlotMP = function(ret,add.grid = F,cex = 1.0, layout = NULL,type = "l",
                    pct = 100, yname = "RETURNS (%)",scaleType = "free",
                    lwd = 1, color = "black", main)
{
  if(add.grid) {type = c("l","g")} else
  {type = type}
  pl = xyplot(pct*ret,par.strip.text = list(cex = cex),type = type,
                xlab="", ylab = list(label = yname,cex = cex), lwd = lwd,
                scales = list(y = list(cex = cex,relation=scaleType),
                              x = list(cex = cex)),layout = layout,
                col = color, strip = F, strip.left = T, main=main)
  print(pl)
}
