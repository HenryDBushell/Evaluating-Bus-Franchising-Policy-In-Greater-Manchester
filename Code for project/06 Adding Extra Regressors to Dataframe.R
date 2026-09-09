source("Code for project/05 Wrangling the Real Median Income Data.R")

##**CREATING THE TREATMENT INDICATOR**
Treatment <- as.vector(matrix(0, nrow=1408)) ##Creates a vector of zeros of length 1,408
for (i in 1:1408){
  if (BusJourneys_PrivateCars_TramJourneys_RealPay[i, c(2)]=="Greater Manchester CA" & (BusJourneys_PrivateCars_TramJourneys_RealPay[i, c(3)]=="2024" | BusJourneys_PrivateCars_TramJourneys_RealPay[i, c(3)]=="2025")){
    Treatment[i] <- 1
  }
} ##Sets the treatment indicator to be equal to 1 only for the treatment units (i.e. Greater Manchester after 2024)
BusJourneys_PrivateCars_TramJourneys_RealPay <- data.frame(BusJourneys_PrivateCars_TramJourneys_RealPay, Treatment) ##Stitches the treatment indicator vector onto the main dataset
#View(BusJourneys_PrivateCars_TramJourneys_RealPay)

##**CREATING THE COVID INDICATOR**
Covid <- as.vector(matrix(0, nrow=1408)) ##Creates a vector of zeros of length 1,408
for (i in 1:1408){
  if (BusJourneys_PrivateCars_TramJourneys_RealPay[i, c(3)]=="2021" | BusJourneys_PrivateCars_TramJourneys_RealPay[i, c(3)]=="2022"){
    Covid[i] <- 1
  }
} ##Sets the covid indicator to be equal to 1 only when Year = 2021 or 2022
BusJourneys_PrivateCars_TramJourneys_RealPay <- data.frame(BusJourneys_PrivateCars_TramJourneys_RealPay, Covid) ##Stitches the covid indicator vector onto the main dataset
#View(BusJourneys_PrivateCars_TramJourneys_RealPay)

##**CREATING THE LINEAR TIME TREND**
BusJourneys_PrivateCars_TramJourneys_RealPay_Time <- BusJourneys_PrivateCars_TramJourneys_RealPay[ , c(3)] ##Creates a dataframe which is just the year column from the main dataframe
BusJourneys_PrivateCars_TramJourneys_RealPay_Time <- as.numeric(BusJourneys_PrivateCars_TramJourneys_RealPay_Time) ##Turns this year series dataframe into numbers
for (i in 1:16){
  BusJourneys_PrivateCars_TramJourneys_RealPay_Time[BusJourneys_PrivateCars_TramJourneys_RealPay_Time==(2009+i)] <- i
} ##In the created dataframe, this code replaces 2010 with 1, 2011 with 2, 2012 with 3 etc. 
BusJourneys_PrivateCars_TramJourneys_RealPay <- data.frame(BusJourneys_PrivateCars_TramJourneys_RealPay, BusJourneys_PrivateCars_TramJourneys_RealPay_Time) ##Stitches this linear time trend vector onto the main dataframe
colnames(BusJourneys_PrivateCars_TramJourneys_RealPay)[colnames(BusJourneys_PrivateCars_TramJourneys_RealPay) == "BusJourneys_PrivateCars_TramJourneys_RealPay_Time"] <- "t" ##Renames the linear time trend column accordingly
#View(BusJourneys_PrivateCars_TramJourneys_RealPay)

##**CREATING THE TIME DUMMIES**
for (k in 1:16){
  a <- as.vector(matrix(0, nrow=1408)) ##Creates a vector of zeros of length 1,408
  for (i in 1:1408){
    if (BusJourneys_PrivateCars_TramJourneys_RealPay[i, c(12)] == k){
      a[i] <- 1
    }
  } ##Creates time dummies for t = 1, 2, 3, ..., 16
  BusJourneys_PrivateCars_TramJourneys_RealPay <- data.frame(BusJourneys_PrivateCars_TramJourneys_RealPay, a) ##Stitches these time dummies onto the main dataframe
}
#View(BusJourneys_PrivateCars_TramJourneys_RealPay)

