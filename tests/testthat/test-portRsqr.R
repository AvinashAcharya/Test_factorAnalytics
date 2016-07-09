#Load the data 
data("factorDataSetDjia5Yrs")

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

