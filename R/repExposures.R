#' @title Portfolio tabular reports for risk decomposition and performance analysis
#' 
#' @description 


###################################################################################
##sample data
data = "stocks145scores6.csv"
stacked.df <- read.table(file=data,header=TRUE,
                         sep=",",as.is=TRUE)
head(stacked.df)

# GET FIVE YEAR SEGMENT
short = T
if(short)
{stacked.df$DATE = as.yearmon(stacked.df$DATE)
stacked.df = stacked.df[stacked.df$DATE >=as.yearmon("2008-01-01") &
                          stacked.df$DATE <= as.yearmon("2012-12-31"),]}
names(stacked.df)

# FIT FUNDFACMOD
industry.mod <- fitFfm(data = stacked.df, # Change fit object to mixed.mod
                       ##exposure.vars = c("SECTOR","ROE","BP","PM12M1M","SIZE","ANNVOL1M","EP"), it works fine too 
                       exposure.vars = c("ROE","BP","PM12M1M","SIZE","ANNVOL1M","EP"), 
                       date.var = "DATE", 
                       ret.var = "RETURN", 
                       asset.var = "TICKER", 
                       fit.method="WLS",
                       z.score = T)

repExposures(industry.mod)
####################################################################################

# plotting
library(lattice)

tsPlotMP = function(ret,add.grid = F,cex = 1.0, layout = NULL,type = "l",
                    pct = 100, yname = "RETURNS (%)",scaleType = "free",
                    lwd = 1, color = "black")
{
  if(add.grid) {type = c("l","g")} else
  {type = type}
  print( xyplot(pct*ret,par.strip.text = list(cex = cex),type = type,
                xlab="", ylab = list(label = yname,cex = cex), lwd = lwd,
                scales = list(y = list(cex = cex,relation=scaleType),
                              x = list(cex = cex)),layout = layout,
                col = color, strip = F, strip.left = T) )
}

repExposures <- function(object, weights = NULL, ...){
  # check input object validity
  if (!inherits(object, c("tsfm", "sfm", "ffm"))) {
    stop("Invalid argument: Object should be of class 'tsfm', 'sfm' or 'ffm'.")
  }
  UseMethod("repExposures")
}



repExposures.ffm <- function(object, weights = NULL, ...) {
  
  # get parameter from the factor model fit
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
  
  which.numeric <- sapply(object$data[,object$exposure.vars,drop=FALSE], is.numeric)
  exposures.num <- object$exposure.vars[which.numeric]
  exposures.char <- object$exposure.vars[!which.numeric]
  
  if(length(exposures.char)){
    dat <- object$data[object$data$DATE==object$time.periods[TP], ]
    B <- as.matrix(table(dat$TICKER,dat$SECTOR))
    B[B>0] <- 1
    B <- B[asset.names,]
  }else{
    B = c()
  }
  
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
  
  ###generate plots
  ##print(tsPlotMP(X[,exposures.num]/100, yname = "Portfolio Exposures", scaleType = "same"))
  
  for(i in 1:ncol(X[,exposures.num])){
    name = colnames(X[,exposures.num])[i]
    barplot(X[,exposures.num][,i],las=2,col=5,
            names.arg= as.yearmon(index(X)),
            cex.names=0.5,
            main=paste("Time Series for",name))
  }
  
  
  tsPlotMP(X[,exposures.num]/100, yname = "Portfolio Exposures", scaleType = "same")
  
  # tabular report 
  sum = summary(coredata(X))
  return(sum)
}

