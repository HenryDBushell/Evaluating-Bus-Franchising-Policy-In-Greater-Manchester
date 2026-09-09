source("Code for project/06 Adding Extra Regressors to Dataframe.R")

##**RUNNING THE ECONOMETRIC MODELS**
a <- plm(Bus.Journeys.Per.Capita.Per.Year ~ Treatment + a.1 + a.2 + a.3 + a.4 + a.5 + a.6 + a.7 + a.8 + a.9 + a.10 + a.11 + a.12 + a.13 + a.14 + a.15, 
         data = BusJourneys_PrivateCars_TramJourneys_RealPay, index = c("LA.or.Region", "Year"), model = "pooling") ##This runs the first model outlined in the paper, pooled OLS with time dummies included
summary(a) ##This prints the regression results
coeftest(a, vcov=vcovHC(a, type="sss")) ##This prints the regression results with cluster- and heteroscedasticity-robust standard errors

b <- plm(Bus.Journeys.Per.Capita.Per.Year ~ Treatment + Private.cars.licensed.in.each.region.in.each.year + Journeys.on.light.rail.trams.per.year + Real.median.pay.in.each.region + a.1 + a.2 + a.3 + a.4 + a.5 + a.6 + a.7 + a.8 + a.9 + a.10 + a.11 + a.12 + a.13 + a.14 + a.15, 
         data = BusJourneys_PrivateCars_TramJourneys_RealPay, index = c("LA.or.Region", "Year"), model = "pooling") ##This runs the second model outlined in the paper, pooled OLS with time dummies and controls included
summary(b)
coeftest(b, vcov=vcovHC(b, type="sss")) ##This prints the regression results with cluster- and heteroscedasticity-robust standard errors

c <- plm(Bus.Journeys.Per.Capita.Per.Year ~ Treatment + Private.cars.licensed.in.each.region.in.each.year + Journeys.on.light.rail.trams.per.year + Real.median.pay.in.each.region, 
         data = BusJourneys_PrivateCars_TramJourneys_RealPay, index = c("LA.or.Region", "Year"), model = "within", effect = 'twoways') ##This runs the third model outlined in the paper, two-way fixed effects with controls included
summary(c)
coeftest(c, vcov=vcovHC(c, type="sss")) ##This prints the regression results with cluster- and heteroscedasticity-robust standard errors

d <- plm(Bus.Journeys.Per.Capita.Per.Year ~ Treatment + Private.cars.licensed.in.each.region.in.each.year + Journeys.on.light.rail.trams.per.year + Real.median.pay.in.each.region + a.1 + a.2 + a.3 + a.4 + a.5 + a.6 + a.7 + a.8 + a.9 + a.10 + a.11 + a.12 + a.13 + a.14 + a.15, 
         data = BusJourneys_PrivateCars_TramJourneys_RealPay, index = c("LA.or.Region", "Year"), model = "fd") ##This runs the fouth model outlined in the paper, first differencing with time dummies and controls included
summary(d)
coeftest(d, vcov=vcovHC(d, type="sss")) ##This prints the regression results with cluster- and heteroscedasticity-robust standard errors

e <- plm(Bus.Journeys.Per.Capita.Per.Year ~ Treatment + Private.cars.licensed.in.each.region.in.each.year + Journeys.on.light.rail.trams.per.year + Real.median.pay.in.each.region + t + Covid, 
         data = BusJourneys_PrivateCars_TramJourneys_RealPay, index = c("LA.or.Region", "Year"), model = "within") ##This runs the final model outlined in the paper, one-way fixed effects with a linear time trend/covid dummy and controls included
summary(e)
coeftest(e, vcov=vcovHC(e, type="sss")) ##This prints the regression results with cluster- and heteroscedasticity-robust standard errors