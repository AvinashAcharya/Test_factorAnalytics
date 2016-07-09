
#Load fundamental and return data 
data("stocks145scores6")
dat = stocks145scores6
dat$DATE = as.yearmon(dat$DATE)
dat = dat[dat$DATE >=as.yearmon("2008-01-01") & dat$DATE <= as.yearmon("2012-12-31"),]

#Load long-only GMV weights for the return data
data("wtsStocks145GmvLo")
wtsStocks145GmvLo = round(wtsStocks145GmvLo,5)  

#fit a fundamental factor model
require(factorAnalytics) 
fit <- fitFfm(data = dat, 
              exposure.vars = c("SECTOR","ROE","BP","PM12M1M","SIZE","ANNVOL1M","EP"),
              date.var = "DATE", ret.var = "RETURN", asset.var = "TICKER", 
              fit.method="WLS", z.score = TRUE)

#generating statistic
expect_equal(is.list(portSdDecomp(fit, wtsStocks145GmvLo, isPlot = FALSE)), TRUE) 
expect_equal(is.list(portSdDecomp(fit, wtsStocks145GmvLo, isPlot = TRUE, which = 1)), TRUE) 

#testing error message
expect_error(portSdDecomp(fit, weights = c(0.5,0.5), isPlot = TRUE, which = 1,
                          add.grid = FALSE, zeroLine = TRUE, color = 'Blue'), 
             "Invalid argument: incorrect number of weights") 