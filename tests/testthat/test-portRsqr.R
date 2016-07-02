#Load the data 
data("factorDataSetDjia5Yrs")
data("wtsDjiaGmvLo")
w = wtsDjiaGmvLo
#Fit a Ffm
fit <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN", 
              date.var="DATE", exposure.vars="SECTOR")

#test for output lengths
out <- portRsqr(fit)
expect_equal(length(out), 1)

out <- portRsqr(fit, rsq = F, rsqAdj = T)
expect_equal(length(out), 1)

out <- portRsqr(fit, rsq = T, rsqAdj = T)
expect_equal(length(out), 2)

#test for error msg
expect_error(portRsqr(fit, rsq = F, rsqAdj = F), 
             "Invalid arguments: Inputs rsq and rsqAdj both cannot be False") 
#Make w=N+1 to generate error msg
w<- c(w,"dummy" = 0.02)
expect_error(portRsqr(fit, rsq = T, rsqAdj = F, weight = w),
             "Error: Length of weight should be equal to the number of assets") 
