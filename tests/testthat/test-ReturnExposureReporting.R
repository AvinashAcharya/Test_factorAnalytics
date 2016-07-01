
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
repExposures(fit, wtsStocks145GmvLo, isPlot = FALSE, digits = 4)

#generating plot
repExposures(fit, wtsStocks145GmvLo, isPrint = FALSE, isPlot = TRUE, which = 2,
             add.grid = TRUE, scaleType = 'same', layout = c(3,2))

#testing error message
out = repExposures(fit, weights = c(0.5,0.5), isPlot = TRUE, which = 1,
             add.grid = FALSE, zeroLine = TRUE, color = 'Blue')
expect_error(out, "Error: Length of weight should be equal to the number of assets") 




#generating statistic
repReturn(fit, wtsStocks145GmvLo, isPlot = FALSE, digits = 4)

#generating plot
repReturn(fit, wtsStocks145GmvLo, isPrint = FALSE, isPlot = TRUE, which = 4)

#generating plot
repReturn(fit, wtsStocks145GmvLo, isPrint = FALSE, isPlot = TRUE, which = 1,
          add.grid = TRUE, scaleType = 'same')

#testing error message
out = repReturn(fit, weights = c(0.5,0.5), isPrint = FALSE, isPlot = TRUE, which = 2,
          add.grid = FALSE, zeroLine = TRUE, color = 'Blue')  
expect_error(out, "Error: Length of weight should be equal to the number of assets") 



