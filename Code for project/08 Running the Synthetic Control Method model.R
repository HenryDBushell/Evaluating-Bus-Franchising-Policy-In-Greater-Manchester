source("Code for project/07 Running the Econometric Models.R")

##**REMOVING ISLE OF SCILLY AS FULLY MISSING DATA MESSES WITH SCM**
BusJourneys_PrivateCars_TramJourneys_RealPay <- BusJourneys_PrivateCars_TramJourneys_RealPay %>% filter(`LA.or.Region` != "Isles of Scilly") ##Filters out Isles of Scilly as it is almost entirely missing values
BusJourneys_PrivateCars_TramJourneys_RealPay <- BusJourneys_PrivateCars_TramJourneys_RealPay %>% filter(`LA.or.Region` != "Bournemouth, Christchurch and Poole" & `LA.or.Region` != "Buckinghamshire excluding Milton Keynes" & `LA.or.Region` != "Dorset excluding Poole & Bournemouth") ##Filters out more regions which have missing values for key lagged values of the dependent variable (i.e. lagged values of the dependent variable used in the estimation process)

##**CREATING THE NUMERIC UNIT VARIABLE (NEEDED FOR unit.variable BELOW)**
LA <- data.frame(LA = c(1:84, 1:84, 1:84, 1:84, 1:84, 1:84, 1:84, 1:84, 1:84, 1:84, 1:84, 1:84, 1:84, 1:84, 1:84, 1:84)) ##Creates a dataframe which is just the numbers 1-88 repeated 88 times
LA <- LA %>% arrange(LA) ##Arranges this dataframe numerically so it can represent the LA numbers numerically
BusJourneys_PrivateCars_TramJourneys_RealPay = data.frame(BusJourneys_PrivateCars_TramJourneys_RealPay, LA) ##Stitches this dataframe onto the main dataframe
#View(BusJourneys_PrivateCars_TramJourneys_RealPay)

##**RUNNING THE SCM MODEL AND PLOTTING THE RESULTS**
BusJourneys_PrivateCars_TramJourneys_RealPay <- BusJourneys_PrivateCars_TramJourneys_RealPay %>% mutate(`Year` = as.numeric(`Year`)) ##Transforms the year column into a numeric as needed by the SCM function
dataprep_out <- dataprep( ##We need to prepare the data to be used in the synth function
  foo = BusJourneys_PrivateCars_TramJourneys_RealPay, ##This is the dataframe to be analysed
  predictors = c("Private.cars.licensed.in.each.region.in.each.year", "Journeys.on.light.rail.trams.per.year", "Real.median.pay.in.each.region"), ##These are the predictors which we will use
  special.predictors = list(
    list("Bus.Journeys.Per.Capita.Per.Year", 2011, c("mean")),
    list("Bus.Journeys.Per.Capita.Per.Year", 2015, c("mean")),
    list("Bus.Journeys.Per.Capita.Per.Year", 2019, c("mean")),
    list("Bus.Journeys.Per.Capita.Per.Year", 2023, c("mean"))
  ), ##This includes the lagged values of the dependent variable which we want to include as predictors
  dependent = "Bus.Journeys.Per.Capita.Per.Year", ##This sets the dependent variable
  unit.variable = "LA", ##Sets the cross sectional units, must be a numeric
  time.variable = "Year", ##Sets the time units, must be a numeric
  treatment.identifier = "Greater Manchester CA", ##Sets the cross sectional treatment unit
  controls.identifier = unique(BusJourneys_PrivateCars_TramJourneys_RealPay$LA.or.Region[
    -which(BusJourneys_PrivateCars_TramJourneys_RealPay$LA.or.Region == "Greater Manchester CA")
  ]), ##Picks the control units, which is every unit in the sample aside from Greater Manchester CA - the function above creates this list automatically
  time.predictors.prior = 2010:2023, ##Selects the pre-treatment period
  time.optimize.ssr = 2012:2023, ##This selects the period with which the ssr is minimised over. Error messages are thrown up for certain values of this, so I chose the largest range of values possible which doesn't throw up an error message
  time.plot = 2010:2025, ##This is just the period you want to plot the graph for, which is the whole of the dataset really
  unit.names.variable = "LA.or.Region" ##Need this as we're using the name "Greater Manchester CA" in the above categories instead of using the LA number identifier
)

synth_out <- synth(
  data.prep.obj = dataprep_out
) ##Runs the SCM model

plot( ##This allows us to plot the synthetic control
  2010:2025, ##This is the years we are plotting between
  dataprep_out$Y0 %*% synth_out$solution.w, ##This multiplies the matrix of local authority data by the weights assigned by the synthetic control method, i.e creating the untreated unit
  type = "l", ##This produces a line graph
  lty = "dashed", ##This makes sure the line is dashed
  ylab = "Bus Journeys per Capita per Year",
  xlab = "Year" ##Sets titles for axes
)
lines(2010:2025, dataprep_out$Y1) ##Adds in the real data for Greater Manchester
abline(v = 2023, lty = "dotted") ##Adds in the treatment indicator line

diff = dataprep_out$Y1 - dataprep_out$Y0 %*% synth_out$solution.w ##Creates a vector equal to the difference between the treated unit and the synthetic control unit
sprintf("The treatment effect in year 1 is equal to: %f", diff[15, ]) ##Prints the year 1 treatment effect
sprintf("The treatment effect in year 2 is equal to: %f", diff[16, ]) ##Prints the year 2 treatment effect

##**RUNNING THE MODELS WITH DIFFERENT CHOICES OF LAGGED DEPENDENT VARIABLES AND A BACKDATED TREATMENT YEAR**
##Alternative model 1
dataprep_out1 <- dataprep( 
  foo = BusJourneys_PrivateCars_TramJourneys_RealPay, 
  predictors = c("Private.cars.licensed.in.each.region.in.each.year", "Journeys.on.light.rail.trams.per.year", "Real.median.pay.in.each.region"), 
  special.predictors = list(
    list("Bus.Journeys.Per.Capita.Per.Year", 2010, c("mean")),
    list("Bus.Journeys.Per.Capita.Per.Year", 2014, c("mean")),
    list("Bus.Journeys.Per.Capita.Per.Year", 2018, c("mean")),
    list("Bus.Journeys.Per.Capita.Per.Year", 2022, c("mean"))
  ), ##This includes the lagged values of the dependent variable which we want to include as predictors
  dependent = "Bus.Journeys.Per.Capita.Per.Year", 
  unit.variable = "LA", 
  time.variable = "Year", 
  treatment.identifier = "Greater Manchester CA", 
  controls.identifier = unique(BusJourneys_PrivateCars_TramJourneys_RealPay$LA.or.Region[
    -which(BusJourneys_PrivateCars_TramJourneys_RealPay$LA.or.Region == "Greater Manchester CA")
  ]), 
  time.predictors.prior = 2010:2023, 
  time.optimize.ssr = 2010:2023, ##Error messages are thrown up for certain values of this, so I chose the largest range of values possible which doesn't throw up an error message
  time.plot = 2010:2025, 
  unit.names.variable = "LA.or.Region" 
)

synth_out1 <- synth(data.prep.obj = dataprep_out1) 

plot( 
  2010:2025, 
  dataprep_out1$Y0 %*% synth_out1$solution.w, 
  type = "l", 
  lty = "dashed", 
  ylab = "Bus Journeys per Capita per Year",
  xlab = "Year" 
)
lines(2010:2025, dataprep_out1$Y1) 
abline(v = 2023, lty = "dotted") 

diff1 = dataprep_out1$Y1 - dataprep_out1$Y0 %*% synth_out1$solution.w 
sprintf("The treatment effect in year 1 is equal to: %f", diff1[15, ]) 
sprintf("The treatment effect in year 2 is equal to: %f", diff1[16, ]) 

##Alternative model 2
dataprep_out2 <- dataprep( 
  foo = BusJourneys_PrivateCars_TramJourneys_RealPay, 
  predictors = c("Private.cars.licensed.in.each.region.in.each.year", "Journeys.on.light.rail.trams.per.year", "Real.median.pay.in.each.region"), 
  special.predictors = list(
    list("Bus.Journeys.Per.Capita.Per.Year", 2012, c("mean")),
    list("Bus.Journeys.Per.Capita.Per.Year", 2016, c("mean")),
    list("Bus.Journeys.Per.Capita.Per.Year", 2020, c("mean")),
    list("Bus.Journeys.Per.Capita.Per.Year", 2023, c("mean"))
  ), ##This includes the lagged values of the dependent variable which we want to include as predictors
  dependent = "Bus.Journeys.Per.Capita.Per.Year", 
  unit.variable = "LA", 
  time.variable = "Year", 
  treatment.identifier = "Greater Manchester CA", 
  controls.identifier = unique(BusJourneys_PrivateCars_TramJourneys_RealPay$LA.or.Region[
    -which(BusJourneys_PrivateCars_TramJourneys_RealPay$LA.or.Region == "Greater Manchester CA")
  ]), 
  time.predictors.prior = 2010:2023, 
  time.optimize.ssr = 2011:2023, ##Error messages are thrown up for certain values of this, so I chose the largest range of values possible which doesn't throw up an error message
  time.plot = 2010:2025, 
  unit.names.variable = "LA.or.Region" 
)

synth_out2 <- synth(data.prep.obj = dataprep_out2) 

plot( 
  2010:2025, 
  dataprep_out2$Y0 %*% synth_out2$solution.w, 
  type = "l", 
  lty = "dashed", 
  ylab = "Bus Journeys per Capita per Year",
  xlab = "Year" 
)
lines(2010:2025, dataprep_out2$Y1) 
abline(v = 2023, lty = "dotted") 

diff2 = dataprep_out2$Y1 - dataprep_out2$Y0 %*% synth_out2$solution.w 
sprintf("The treatment effect in year 1 is equal to: %f", diff2[15, ]) 
sprintf("The treatment effect in year 2 is equal to: %f", diff2[16, ]) 

##Alternative model 3
dataprep_out3 <- dataprep( 
  foo = BusJourneys_PrivateCars_TramJourneys_RealPay, 
  predictors = c("Private.cars.licensed.in.each.region.in.each.year", "Journeys.on.light.rail.trams.per.year", "Real.median.pay.in.each.region"), 
  special.predictors = list(
    list("Bus.Journeys.Per.Capita.Per.Year", 2011, c("mean")),
    list("Bus.Journeys.Per.Capita.Per.Year", 2015, c("mean")),
    list("Bus.Journeys.Per.Capita.Per.Year", 2019, c("mean")),
    list("Bus.Journeys.Per.Capita.Per.Year", 2022, c("mean"))
  ), ##This includes the lagged values of the dependent variable which we want to include as predictors
  dependent = "Bus.Journeys.Per.Capita.Per.Year", 
  unit.variable = "LA", 
  time.variable = "Year", 
  treatment.identifier = "Greater Manchester CA", 
  controls.identifier = unique(BusJourneys_PrivateCars_TramJourneys_RealPay$LA.or.Region[
    -which(BusJourneys_PrivateCars_TramJourneys_RealPay$LA.or.Region == "Greater Manchester CA")
  ]), 
  time.predictors.prior = 2010:2022, 
  time.optimize.ssr = 2010:2022, ##Error messages are thrown up for certain values of this, so I chose the largest range of values possible which doesn't throw up an error message
  time.plot = 2010:2025, 
  unit.names.variable = "LA.or.Region" 
)

synth_out3 <- synth(data.prep.obj = dataprep_out3) 

plot( 
  2010:2025, 
  dataprep_out3$Y0 %*% synth_out3$solution.w, 
  type = "l", 
  lty = "dashed", 
  ylab = "Bus Journeys per Capita per Year",
  xlab = "Year" 
)
lines(2010:2025, dataprep_out3$Y1) 
abline(v = 2022, lty = "dotted") 

diff3 = dataprep_out3$Y1 - dataprep_out3$Y0 %*% synth_out3$solution.w 
sprintf("The treatment effect in year 1 is equal to: %f", diff3[14, ])
sprintf("The treatment effect in year 2 is equal to: %f", diff3[15, ]) 
sprintf("The treatment effect in year 3 is equal to: %f", diff3[16, ]) 
