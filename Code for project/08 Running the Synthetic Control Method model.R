source("Code for project/07 Running the Econometric Models.R")

##**CREATING THE NUMERIC UNIT VARIABLE (NEEDED FOR unit.variable BELOW)**


##**RUNNING THE SCM MODEL**
#view(BusJourneys_PrivateCars_TramJourneys_RealPay)
dataprep_out <- dataprep( ##We need to prepare the data to be used in the synth function
  foo = BusJourneys_PrivateCars_TramJourneys_RealPay, ##This is the dataframe to be analysed
  predictors = c("Private.cars.licensed.in.each.region.in.each.year", "Journeys.on.light.rail.trams.per.year", "Real.median.pay.in.each.region"), ##These are the predictors which we will use
  special.predictors = list(
    list("Bus.Journeys.Per.Capita.Per.Year", 2, c("mean")),
    list("Bus.Journeys.Per.Capita.Per.Year", 6, c("mean")),
    list("Bus.Journeys.Per.Capita.Per.Year", 10, c("mean")),
    list("Bus.Journeys.Per.Capita.Per.Year", 14, c("mean"))
  ), ##This includes the lagged values of the dependent variable which we want to include as predictors
  dependent = "Bus.Journeys.Per.Capita.Per.Year", ##This sets the dependent variable
  unit.variable = "", ##Sets the cross sectional units, must be a numeric
  time.variable = "t", ##Sets the time units, must be a numeric
  treatment.identifier = "Greater Manchester CA", ##Sets the cross sectional treatment unit
  controls.identifier = unique(BusJourneys_PrivateCars_TramJourneys_RealPay$LA.or.Region[
    -which(BusJourneys_PrivateCars_TramJourneys_RealPay$LA.or.Region == "Greater Manchester CA")
  ]), ##Picks the control units, which is every unit in the sample aside from Greater Manchester CA - the function above creates this list automatically
  time.predictors.prior = 1:15, ##Selects the pre-treatment period, including the treatment year??
  time.optimize.ssr = 1:15, ##Again, selects the pre-treatment period, including the treatment year??
  time.plot = 1:16,
  unit.names.variable = "LA.or.Region"
)
###Got up to 31:00 in video