#' @title Decompose portfolio risk into individual factor contributions and provide tabular report
#' 
#' @description Compute the factor contributions to standard deviation (SD), Value-at-Risk (VaR), 
#' Expected Tail Loss or Expected Shortfall (ES) of the return of individual asset within a portfolio 
#' return of a portfolio based on Euler's theorem, given the fitted factor model.
#' 
#' 
#' @param object fit object of class \code{tsfm}, or \code{ffm}.
#' @param p confidence level for calculation. Default is 0.95.
#' @param weights a vector of weights of the assets in the portfolio, names of 
#' the vector should match with asset names. Default is NULL, in which case an 
#' equal weights will be used.
#' @param risk.factor one of 'Sd' (standard deviation), 'VaR' (Value-at-Risk) or 'Es' (Expected Tail 
#' Loss or Expected Shortfall for calculating risk decompositon. Default is 'Sd'
#' @param risk.budget one of 'RM' (risk measure), 'FMCR' (factor marginal contribution to risk), 
#' 'FCR' 'factor contribution to risk' or 'FPCR' (factor percent contribution to risk). Default is 'RM'
#' @param nrowPrint a numerical value deciding number of assets/portfolio in result vector/table to print  
#' @param type one of "np" (non-parametric) or "normal" for calculating VaR & Es. 
#' Default is "np".
#' @param use an optional character string giving a method for computing factor
#' covariances in the presence of missing values. This must be (an 
#' abbreviation of) one of the strings "everything", "all.obs", 
#' "complete.obs", "na.or.complete", or "pairwise.complete.obs". Default is 
#' "pairwise.complete.obs".
#' @param ... other optional arguments passed to \code{\link[stats]{quantile}} and 
#' optional arguments passed to \code{\link[stats]{cov}}
#'
#' @return A table containing 
#' \item{risk.budget = 'RM'}{length-(N + 1) vector of factor model risk measure of portfolio return 
#' as well assets return.}
#' \item{risk.budget = 'FMCR'}{(N + 1) * (K + 1) matrix of marginal contributions to risk of portfolio 
#' return as well assets return.}
#' \item{risk.budget = 'FCR'}{(N + 1) * (K + 1) matrix of component contributions to risk of portfolio 
#' return as well assets return.}
#' \item{risk.budget = 'FPCR'}{(N + 1) * (K + 1) matrix of percentage component contributions to risk 
#' of portfolio return as well assets return.}
#' Where, K is the number of factors, N is the number of assets.
#' 
#' @author Douglas Martin, Lingjie Yi
#' 
#' 
#' @seealso \code{\link{fitTsfm}}, \code{\link{fitSfm}}, \code{\link{fitFfm}}
#' for the different factor model fitting functions.
#' 
#' \code{\link{portVaRDecomp}} for factor model VaR decomposition.
#' \code{\link{portEsDecomp}} for factor model ES decomposition.
#' 
#' 
#' @examples
#' # Time Series Factor Model
#' data(managers)
#' fit.macro <- factorAnalytics::fitTsfm(asset.names=colnames(managers[,(1:6)]),
#'                      factor.names=colnames(managers[,(7:9)]),
#'                      rf.name="US.3m.TR", data=managers)
#' report <- repRisk(fit.macro, risk.factor = "Es", risk.budget = 'FPCR', nrowPrint = 10)
#' report 
#' 
#' # random weights 
#' wts = runif(6)
#' wts = wts/sum(wts)
#' names(wts) <- colnames(managers)[1:6]
#' repRisk(fit.macro, risk.factor = "VaR", risk.budget = 'FMCR', nrowPrint = 10)
#' 
#' # Fundamental Factor Model
#' data("stocks145scores6")
#' dat = stocks145scores6
#' dat$DATE = as.yearmon(dat$DATE)
#' dat = dat[dat$DATE >=as.yearmon("2008-01-01") & dat$DATE <= as.yearmon("2012-12-31"),]
#'
#' # Load long-only GMV weights for the return data
#' data("wtsStocks145GmvLo")
#' wtsStocks145GmvLo = round(wtsStocks145GmvLo,5)  
#'                                                      
#' # fit a fundamental factor model
#' fit.cross <- fitFfm(data = dat, 
#'               exposure.vars = c("SECTOR","ROE","BP","PM12M1M","SIZE","ANNVOL1M","EP"),
#'               date.var = "DATE", ret.var = "RETURN", asset.var = "TICKER", 
#'               fit.method="WLS", z.score = TRUE)
#' repRisk(fit.cross, risk.factor = "Sd", risk.budget = 'FCR', nrowPrint = 10) 
#' # get the factor contributions of risk 
#' repRisk(fit.cross, wtsStocks145GmvLo, risk.factor = "Sd", risk.budget = 'FPCR', nrowPrint = 10)               
#'  
#' @export    


repRisk <- function(object, ...){
  # check input object validity
  if (!inherits(object, c("tsfm", "ffm"))) {
    stop("Invalid argument: Object should be of class 'tsfm',  or 'ffm'.")
  }
  UseMethod("repRisk")
}

#' @rdname repRisk
#' @method repRisk tsfm
#' @export

repRisk.tsfm <- function(object, weights = NULL, risk.factor = c("Sd", "VaR", "Es"), 
                         risk.budget = c("RM", 'FMCR', 'FCR', 'FPCR'),
                         nrowPrint = 20, p=0.95, type=c("np","normal"), use="pairwise.complete.obs", ...) {
  
  # set default for type
  type = type[1]
  risk.factor = risk.factor[1]
  risk.budget = risk.budget[1]
  
  if (!(type %in% c("np","normal"))) {
    stop("Invalid args: type must be 'np' or 'normal' ")
  }
  
  if (!prod(risk.factor %in% c("Sd", "VaR", "Es"))) {
    stop("Invalid args: risk.factor must be 'Sd', 'VaR' or 'Es' ")
  }
  
  if (!prod(risk.budget %in% c("RM", 'FMCR', 'FCR', 'FPCR'))) {
    stop("Invalid args: risk.budget must be 'RM', 'FMCR', 'FCR' or 'FPCR' ")
  }
  
  if(length(which(risk.factor == "Sd"))){
    port.Sd = portSdDecomp(object, weights = weights, use = use, ... )
    asset.Sd = factorAnalytics::fmSdDecomp(object, use = use, ... )
    
    if(risk.budget == "RM"){
      port = port.Sd$Sd.fm
      asset = asset.Sd$Sd.fm
      result = c(port, asset)
      names(result)[1] = 'Portfolio'
      result = head(result, nrowPrint)
    }
    
    else if(risk.budget == "FMCR"){
      port = port.Sd$mSd
      asset = asset.Sd$mSd
      result = rbind(port, asset)
      rownames(result)[1] = 'Portfolio'
      result = head(result, nrowPrint)
    }
    
    else if(risk.budget == "FCR"){
      port = port.Sd$cSd
      asset = asset.Sd$cSd
      result = rbind(port, asset)
      rownames(result)[1] = 'Portfolio'
      result = head(result, nrowPrint)
    }
    
    else if(risk.budget == "FPCR"){
      port = port.Sd$pcSd
      asset = asset.Sd$pcSd
      result = rbind(port, asset)
      rownames(result)[1] = 'Portfolio'
      result = head(result, nrowPrint)
    }
    
  }
  
  else if(length(which(risk.factor == "VaR"))){
    port.VaR = portVaRDecomp(object, weights = weights, p = p, type = type, use = use, ... )
    asset.VaR = factorAnalytics::fmVaRDecomp(object, p = p, type = type, use = use, ... )
    
    if(risk.budget == "RM"){
      port = port.VaR$VaR.fm
      asset = asset.VaR$VaR.fm
      result = c(port, asset)
      names(result)[1] = 'Portfolio'
      result = head(result, nrowPrint)
    }
    
    else if(risk.budget == "FMCR"){
      port = port.VaR$mVaR
      asset = asset.VaR$mVaR
      result = rbind(port, asset)
      rownames(result)[1] = 'Portfolio'
      result = head(result, nrowPrint)
    }
    
    else if(risk.budget == "FCR"){
      portRM = port.Sd$Sd.fm
      assetRM = asset.Sd$Sd.fm
      resultRM = c(portRM, assetRM)
      
      port = port.Sd$cSd
      asset = asset.Sd$cSd
      result = cbind(resultRM,rbind(port, asset))
      rownames(result)[1] = 'Portfolio'
      colnames(result)[1] = 'RM'
      result = head(result, nrowPrint)
    }
    
    else if(risk.budget == "FPCR"){
      port = port.VaR$pcVaR
      asset = asset.VaR$pcVaR
      result = rbind(port, asset)
      rownames(result)[1] = 'Portfolio'
      result = head(result, nrowPrint)
    }
    
  }

  else if(length(which(risk.factor == "Es"))){
    port.Es = portEsDecomp(object, weights = weights, p = p, type = type, use = use, ... )
    asset.Es = factorAnalytics::fmEsDecomp(object, p = p, type = type, use = use, ... )
    
    if(risk.budget == "RM"){
      port = port.Es$Es.fm
      asset = asset.Es$Es.fm
      result = c(port, asset)
      names(result)[1] = 'Portfolio'
      result = head(result, nrowPrint)
    }
    
    else if(risk.budget == "FMCR"){
      port = port.Es$mES
      asset = asset.Es$mES
      result = rbind(port, asset)
      rownames(result)[1] = 'Portfolio'
      result = head(result, nrowPrint)
    }
    
    else if(risk.budget == "FCR"){
      portRM = port.Sd$Sd.fm
      assetRM = asset.Sd$Sd.fm
      resultRM = c(portRM, assetRM)
      
      port = port.Sd$cSd
      asset = asset.Sd$cSd
      result = cbind(resultRM,rbind(port, asset))
      rownames(result)[1] = 'Portfolio'
      colnames(result)[1] = 'RM'
      result = head(result, nrowPrint)
    }
    
    else if(risk.budget == "FPCR"){
      port = port.Es$pcES
      asset = asset.Es$pcES
      result = rbind(port, asset)
      rownames(result)[1] = 'Portfolio'
      result = head(result, nrowPrint)
    }
    
  }
  
  return(result)
}

#' @rdname repRisk
#' @method repRisk ffm
#' @export

repRisk.ffm <- function(object, weights = NULL, risk.factor = c("Sd", "VaR", "Es"),
                        risk.budget = c("RM", 'FMCR', 'FCR', 'FPCR'),
                        nrowPrint = 20, p=0.95, type=c("np","normal"), ...) {
  
  # set default for type
  type = type[1]
  risk.factor = risk.factor[1]
  risk.budget = risk.budget[1]
  
  if (!(type %in% c("np","normal"))) {
    stop("Invalid args: type must be 'np' or 'normal' ")
  }
  
  if (!prod(risk.factor %in% c("Sd", "VaR", "Es"))) {
    stop("Invalid args: risk.factor must be 'Sd', 'VaR' or 'Es' ")
  }
  
  if (!prod(risk.budget %in% c("RM", 'FMCR', 'FCR', 'FPCR'))) {
    stop("Invalid args: risk.budget must be 'RM', 'FMCR', 'FCR' or 'FPCR' ")
  }
  
  if(length(which(risk.factor == "Sd"))){
    port.Sd = portSdDecomp(object, weights = weights, ... )
    asset.Sd = factorAnalytics::fmSdDecomp(object, ... )
    
    if(risk.budget == "RM"){
      port = port.Sd$Sd.fm
      asset = asset.Sd$Sd.fm
      result = c(port, asset)
      names(result)[1] = 'Portfolio'
      result = head(result, nrowPrint)
    }
    
    else if(risk.budget == "FMCR"){
      port = port.Sd$mSd
      asset = asset.Sd$mSd
      result = rbind(port, asset)
      rownames(result)[1] = 'Portfolio'
      result = head(result, nrowPrint)
    }
    
    else if(risk.budget == "FCR"){
      portRM = port.Sd$Sd.fm
      assetRM = asset.Sd$Sd.fm
      resultRM = c(portRM, assetRM)

      port = port.Sd$cSd
      asset = asset.Sd$cSd
      result = cbind(resultRM,rbind(port, asset))
      rownames(result)[1] = 'Portfolio'
      colnames(result)[1] = 'RM'
      result = head(result, nrowPrint)
    }
    
    else if(risk.budget == "FPCR"){
      port = port.Sd$pcSd
      asset = asset.Sd$pcSd
      result = rbind(port, asset)
      rownames(result)[1] = 'Portfolio'
      result = head(result, nrowPrint)
    }
    
  }
  
  else if(length(which(risk.factor == "VaR"))){
    port.VaR = portVaRDecomp(object, weights = weights, p = p, type = type, ... )
    asset.VaR = factorAnalytics::fmVaRDecomp(object, p = p, type = type, ... )
    
    if(risk.budget == "RM"){
      port = port.VaR$VaR.fm
      asset = asset.VaR$VaR.fm
      result = c(port, asset)
      names(result)[1] = 'Portfolio'
      result = head(result, nrowPrint)
    }
    
    else if(risk.budget == "FMCR"){
      port = port.VaR$mVaR
      asset = asset.VaR$mVaR
      result = rbind(port, asset)
      rownames(result)[1] = 'Portfolio'
      result = head(result, nrowPrint)
    }
    
    else if(risk.budget == "FCR"){
      portRM = port.Sd$Sd.fm
      assetRM = asset.Sd$Sd.fm
      resultRM = c(portRM, assetRM)
      
      port = port.Sd$cSd
      asset = asset.Sd$cSd
      result = cbind(resultRM,rbind(port, asset))
      rownames(result)[1] = 'Portfolio'
      colnames(result)[1] = 'RM'
      result = head(result, nrowPrint)
    }
    
    else if(risk.budget == "FPCR"){
      port = port.VaR$pcVaR
      asset = asset.VaR$pcVaR
      result = rbind(port, asset)
      rownames(result)[1] = 'Portfolio'
      result = head(result, nrowPrint)
    }
    
  }
  
  else if(length(which(risk.factor == "Es"))){
    port.Es = portEsDecomp(object, weights = weights, p = p, type = type, ... )
    asset.Es = factorAnalytics::fmEsDecomp(object, p = p, type = type, ... )
    
    if(risk.budget == "RM"){
      port = port.Es$Es.fm
      asset = asset.Es$Es.fm
      result = c(port, asset)
      names(result)[1] = 'Portfolio'
      result = head(result, nrowPrint)
    }
    
    else if(risk.budget == "FMCR"){
      port = port.Es$mES
      asset = asset.Es$mES
      result = rbind(port, asset)
      rownames(result)[1] = 'Portfolio'
      result = head(result, nrowPrint)
    }
    
    else if(risk.budget == "FCR"){
      portRM = port.Sd$Sd.fm
      assetRM = asset.Sd$Sd.fm
      resultRM = c(portRM, assetRM)
      
      port = port.Sd$cSd
      asset = asset.Sd$cSd
      result = cbind(resultRM,rbind(port, asset))
      rownames(result)[1] = 'Portfolio'
      colnames(result)[1] = 'RM'
      result = head(result, nrowPrint)
    }
    
    else if(risk.budget == "FPCR"){
      port = port.Es$pcES
      asset = asset.Es$pcES
      result = rbind(port, asset)
      rownames(result)[1] = 'Portfolio'
      result = head(result, nrowPrint)
    }
    
  }
  
  return(result)

}
