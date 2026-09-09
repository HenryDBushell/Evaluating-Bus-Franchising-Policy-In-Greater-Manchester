source("Code for project/04 Wrangling the Tram&Light Rail Data.R")

##**WRANGLING THE REAL MEDIAN INCOME DATA**
#=> Starting with the 2010 data, I want to unify the local authorities included with the ones in my main dataframe. This will be the first step. 
if (file.exists('Sources of data/Income data/2010-revised-table-8') == FALSE) {
  zip.file <- "Sources of data/Income data/2010-revised-table-8.zip"
  unzip(zip.file, exdir = "Sources of data/Income data/2010-revised-table-8")
} ##This code is automatically only run the first time the programme is run. It looks for whether the 2010 income data has been unzipped, and if it hasn't, unzips it.
MedPay_2010 = read_excel("Sources of data/Income data/2010-revised-table-8/REVISED - Home Geography Table 8.7a   Annual pay - Gross 2010.xls", sheet = 1) ##Creates a dataframe from the 2010 median pay data
MedPay_2010 <- MedPay_2010[4:417, ] ##Deletes the first 3 rows from the table and the Wales/Scotland data t
colnames(MedPay_2010) <- MedPay_2010[1, ] ##Renames the columns to the correct titles
MedPay_2010 <- MedPay_2010[6:414, ] ##Gets rid of a few more rows we don't want
MedPay_2010 <- MedPay_2010[, c(1,2,4)] ##Filters only for columns we want
MedPay_2010$`Code` <- as.numeric(MedPay_2010$`Code`) ##Transforms the code from characters into numbers so we can filter based on their size
MedPay_2010$`Median` <- as.numeric(MedPay_2010$`Median`) ##Transforms the median pay from characters into numbers for later

#=> I want to get rid of lots of the entries included in these median pay data, the data are more granular than we want
MedPay_2010 <- MedPay_2010 %>%
  dplyr::filter(`Code` <= 199 | `Code` >= 600) ##Filters out more of the units we don't want. The rows we want all have codes smaller than 199 and larger than 600.
MedPay_2010 <- MedPay_2010[-(57:91), ] ##Gets rid of the London regions, we don't want these
MedPay_2010 <- MedPay_2010[order(MedPay_2010$`Description`), ] ##Sorts the data alphabetically by name of local/combined authority
BusJourneys_PrivateCars_TramJourneys_2010 <- BusJourneys_PrivateCars_TramJourneys %>%
  dplyr::filter(`Year` == 2010) ##Creates a new dataframe from the master dataframe, only containing 2010 entries

#=> I am now going to align the units included in the median pay data with the units in the main dataframe by adding and removing units where applicable
MedPay_2010 <- MedPay_2010 %>%
  dplyr::filter(`Description` != "Bedfordshire" & `Description` != "Cheshire") ##Removes Cheshire and Bedfordshire from the sample. These are just collections of Cheshire East/Cheshire West and Chester, and Bedford and Central Bedfordshire respectively, so they aren't needed.
MedPay_2010[MedPay_2010=="County Durham"]<-"Durham" ##Renames County Durham to Durham in the pay data to make sure it's aligned
MedPay_2010 <- MedPay_2010 %>% add_row(Description='Isles of Scilly', Code=NA, Median=NA) ##Adds the Isles of Scilly into the sample for completion's sake
#=> The median pay data includes Bournemouth and Poole separately. I am going to combine them to align with what we have above. The number of jobs is comparable between the two locations, so I am just going to take an average as an approximation
MedPay_2010_Bournemouth <- MedPay_2010 %>%
  dplyr::filter(`Description` == "Bournemouth UA") ##This isolates the Bournemouth row of data
MedPay_2010_Bournemouth <- MedPay_2010_Bournemouth[ , c(3)] ##This isolates just the median salary for Bournemouth
MedPay_2010_Poole <- MedPay_2010 %>%
  dplyr::filter(`Description` == "Poole UA") 
MedPay_2010_Poole <- MedPay_2010_Poole[ , c(3)] ##This does the same for Poole
MedPay_2010_BournemouthPoole = (MedPay_2010_Bournemouth + MedPay_2010_Poole)/2
MedPay_2010 <- MedPay_2010 %>%
  dplyr::filter(`Description` != "Bournemouth UA" & `Description` != "Poole UA")  ##Filters out the Bournemouth and Poole entries prior to them being re-added
MedPay_2010 <- MedPay_2010 %>% add_row(Description='Bournemouth and Poole', Code=51, Median=MedPay_2010_BournemouthPoole[1,1]) ##Adds in the new Bournemouth/Poole combined entry

#=> These few lines of code here allow me to cross-check the units of each dataframe by stitching them together and viewing them. I then go through and tweak them until the units (regions/LAs/CAs) are unified between the dataframes
MedPay_2010 <- MedPay_2010[order(MedPay_2010$`Description`), ] ##Sorts the data alphabetically by name of local/combined authority
MedPay_2010 <- MedPay_2010[1:100, ]
BusJourneys_PrivateCars_TramJourneys_2010 <- BusJourneys_PrivateCars_TramJourneys_2010[1:100, ] ##Makes the size of the dataframes equal
BusJourneys_PrivateCars_TramJourneys_MedPay_2010 <- data.frame(BusJourneys_PrivateCars_TramJourneys_2010, MedPay_2010) ##Stitches the dataframes together horizontally so I can cross-check them
BusJourneys_PrivateCars_TramJourneys_MedPay_2010 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2010[1:88, ] ##Gets rid of the excess rows at the end
BusJourneys_PrivateCars_TramJourneys_MedPay_2010 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2010[ , c(1:6, 9)]
#View(BusJourneys_PrivateCars_TramJourneys_MedPay_2010)

#=> Now I want to do the same thing for the years 2011-2025 following the same process. I start with 2011. If a bit of doesn't have a '## description' to the right, that's because it's doing the same as the above for the 2010 data
if (file.exists('Sources of data/Income data/2011-provisional-table-8') == FALSE) {
  zip.file <- "Sources of data/Income data/2011-provisional-table-8.zip"
  unzip(zip.file, exdir = "Sources of data/Income data/2011-provisional-table-8")
}
MedPay_2011 = read_excel("Sources of data/Income data/2011-provisional-table-8/REVISED - Home Geography Table 8.7a   Annual pay - Gross 2011.xls", sheet = 1)
MedPay_2011 <- MedPay_2011[4:378, ] ##Gets rid of Scottish and Welsh data, and the first few rows which aren't needed
colnames(MedPay_2011) <- MedPay_2011[1, ] 
MedPay_2011 <- MedPay_2011[6:375, ] 
MedPay_2011 <- MedPay_2011[, c(1,2,4)] 
MedPay_2011$`Code` <- as.numeric(MedPay_2011$`Code`) 
MedPay_2011$`Median` <- as.numeric(MedPay_2011$`Median`) 
MedPay_2011 <- MedPay_2011 %>%
  dplyr::filter(`Code` <= 199 | `Code` >= 600) 
MedPay_2011 <- MedPay_2011[-(55:89), ] ##Gets rid of the London rows
MedPay_2011 <- MedPay_2011[order(MedPay_2011$`Description`), ]
BusJourneys_PrivateCars_TramJourneys_2011 <- BusJourneys_PrivateCars_TramJourneys %>%
  dplyr::filter(`Year` == 2011) 

MedPay_2011_Bournemouth <- MedPay_2011 %>%
  dplyr::filter(`Description` == "Bournemouth UA") 
MedPay_2011_Bournemouth <- MedPay_2011_Bournemouth[ , c(3)] 
MedPay_2011_Poole <- MedPay_2011 %>%
  dplyr::filter(`Description` == "Poole UA") 
MedPay_2011_Poole <- MedPay_2011_Poole[ , c(3)] 
MedPay_2011_BournemouthPoole = (MedPay_2011_Bournemouth + MedPay_2011_Poole)/2
MedPay_2011 <- MedPay_2011 %>%
  dplyr::filter(`Description` != "Bournemouth UA" & `Description` != "Poole UA")  
MedPay_2011 <- MedPay_2011 %>% add_row(Description='Bournemouth and Poole', Code=51, Median=MedPay_2011_BournemouthPoole[1,1]) 

MedPay_2011[MedPay_2011=="County Durham UA"]<-"Durham"
MedPay_2011 <- MedPay_2011 %>% add_row(Description='Isles of Scilly', Code=NA, Median=NA)

MedPay_2011 <- MedPay_2011[order(MedPay_2011$`Description`), ] 
MedPay_2011 <- MedPay_2011[1:100, ]
BusJourneys_PrivateCars_TramJourneys_2011 <- BusJourneys_PrivateCars_TramJourneys_2011[1:100, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2011 <- data.frame(BusJourneys_PrivateCars_TramJourneys_2011, MedPay_2011) 
BusJourneys_PrivateCars_TramJourneys_MedPay_2011 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2011[1:88, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2011 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2011[ , c(1:6, 9)]
#View(BusJourneys_PrivateCars_TramJourneys_MedPay_2011)

#=> Now for 2012's income data
if (file.exists('Sources of data/Income data/2012-revised-table-8') == FALSE) {
  zip.file <- "Sources of data/Income data/2012-revised-table-8.zip"
  unzip(zip.file, exdir = "Sources of data/Income data/2012-revised-table-8")
}
MedPay_2012 = read_excel("Sources of data/Income data/2012-revised-table-8/Home Geography Table 8.7a   Annual pay - Gross 2012.xls", sheet = 1)
MedPay_2012 <- MedPay_2012[4:378, ]
colnames(MedPay_2012) <- MedPay_2012[1, ] 
MedPay_2012 <- MedPay_2012[6:375, ] 
MedPay_2012 <- MedPay_2012[, c(1,2,4)] 
MedPay_2012$`Median` <- as.numeric(MedPay_2012$`Median`) 
MedPay_2012_E06 <- MedPay_2012 %>%
  filter(str_detect(`Code`, "^E06"))
MedPay_2012_E10 <- MedPay_2012 %>%
  filter(str_detect(`Code`, "^E10"))
MedPay_2012_E11 <- MedPay_2012 %>%
  filter(str_detect(`Code`, "^E11")) ##Creates three dataframes, one filtering median pay data for regions with code starting E06, and ones for E10 and E11. 
MedPay_2012_list <- list(MedPay_2012_E06, MedPay_2012_E10, MedPay_2012_E11) ##Creates a list of the three created dataframes
MedPay_2012 <- MedPay_2012_list %>% reduce(full_join) ##Combines the list of the three created dataframes and then overwrites this onto the MedPay_2012 dataframe
MedPay_2012 <- MedPay_2012[order(MedPay_2012$`Description`), ]
BusJourneys_PrivateCars_TramJourneys_2012 <- BusJourneys_PrivateCars_TramJourneys %>%
  dplyr::filter(`Year` == 2012) 

MedPay_2012_Bournemouth <- MedPay_2012 %>%
  dplyr::filter(`Description` == "Bournemouth UA") 
MedPay_2012_Bournemouth <- MedPay_2012_Bournemouth[ , c(3)] 
MedPay_2012_Poole <- MedPay_2012 %>%
  dplyr::filter(`Description` == "Poole UA") 
MedPay_2012_Poole <- MedPay_2012_Poole[ , c(3)] 
MedPay_2012_BournemouthPoole = (MedPay_2012_Bournemouth + MedPay_2012_Poole)/2
MedPay_2012 <- MedPay_2012 %>%
  dplyr::filter(`Description` != "Bournemouth UA" & `Description` != "Poole UA")  
MedPay_2012 <- MedPay_2012 %>% add_row(Description='Bournemouth and Poole', Code="E06000028", Median=MedPay_2012_BournemouthPoole[1,1]) 

MedPay_2012[MedPay_2012=="County Durham UA"]<-"Durham"

MedPay_2012 <- MedPay_2012[order(MedPay_2012$`Description`), ] 
MedPay_2012 <- MedPay_2012[1:100, ]
BusJourneys_PrivateCars_TramJourneys_2012 <- BusJourneys_PrivateCars_TramJourneys_2012[1:100, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2012 <- data.frame(BusJourneys_PrivateCars_TramJourneys_2012, MedPay_2012) 
BusJourneys_PrivateCars_TramJourneys_MedPay_2012 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2012[1:88, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2012 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2012[ , c(1:6, 9)]
#View(BusJourneys_PrivateCars_TramJourneys_MedPay_2012)

#=> Now for 2013's income data
if (file.exists('Sources of data/Income data/2013-revised-table-8') == FALSE) {
  zip.file <- "Sources of data/Income data/2013-revised-table-8.zip"
  unzip(zip.file, exdir = "Sources of data/Income data/2013-revised-table-8")
}
MedPay_2013 = read_excel("Sources of data/Income data/2013-revised-table-8/Home Geography Table 8.7a   Annual pay - Gross 2013.xls", sheet = 2) ##Creates a dataframe equal to the 2nd sheet of the pay data
MedPay_2013 <- MedPay_2013[4:378, ]
colnames(MedPay_2013) <- MedPay_2013[1, ] 
MedPay_2013 <- MedPay_2013[6:375, ] 
MedPay_2013 <- MedPay_2013[, c(1,2,4)] 
MedPay_2013$`Median` <- as.numeric(MedPay_2013$`Median`) 
MedPay_2013_E06 <- MedPay_2013 %>%
  filter(str_detect(`Code`, "^E06"))
MedPay_2013_E10 <- MedPay_2013 %>%
  filter(str_detect(`Code`, "^E10"))
MedPay_2013_E11 <- MedPay_2013 %>%
  filter(str_detect(`Code`, "^E11")) 
MedPay_2013_list <- list(MedPay_2013_E06, MedPay_2013_E10, MedPay_2013_E11) 
MedPay_2013 <- MedPay_2013_list %>% reduce(full_join)
MedPay_2013 <- MedPay_2013[order(MedPay_2013$`Description`), ]
BusJourneys_PrivateCars_TramJourneys_2013 <- BusJourneys_PrivateCars_TramJourneys %>%
  dplyr::filter(`Year` == 2013)

MedPay_2013_Bournemouth <- MedPay_2013 %>%
  dplyr::filter(`Description` == "Bournemouth UA") 
MedPay_2013_Bournemouth <- MedPay_2013_Bournemouth[ , c(3)] 
MedPay_2013_Poole <- MedPay_2013 %>%
  dplyr::filter(`Description` == "Poole UA") 
MedPay_2013_Poole <- MedPay_2013_Poole[ , c(3)] 
MedPay_2013_BournemouthPoole = (MedPay_2013_Bournemouth + MedPay_2013_Poole)/2
MedPay_2013 <- MedPay_2013 %>%
  dplyr::filter(`Description` != "Bournemouth UA" & `Description` != "Poole UA")  
MedPay_2013 <- MedPay_2013 %>% add_row(Description='Bournemouth and Poole', Code="E06000028", Median=MedPay_2013_BournemouthPoole[1,1]) 

MedPay_2013[MedPay_2013=="County Durham UA"]<-"Durham"

MedPay_2013 <- MedPay_2013[order(MedPay_2013$`Description`), ] 
MedPay_2013 <- MedPay_2013[1:100, ]
BusJourneys_PrivateCars_TramJourneys_2013 <- BusJourneys_PrivateCars_TramJourneys_2013[1:100, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2013 <- data.frame(BusJourneys_PrivateCars_TramJourneys_2013, MedPay_2013) 
BusJourneys_PrivateCars_TramJourneys_MedPay_2013 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2013[1:88, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2013 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2013[ , c(1:6, 9)]
#View(BusJourneys_PrivateCars_TramJourneys_MedPay_2013)

#=> Now for 2014's income data
if (file.exists('Sources of data/Income data/rft-8(1)') == FALSE) {
  zip.file <- "Sources of data/Income data/rft-8(1).zip"
  unzip(zip.file, exdir = "Sources of data/Income data/rft-8(1)")
}
MedPay_2014 = read_excel("Sources of data/Income data/rft-8(1)/Home Geography Table 8.7a   Annual pay - Gross 2014.xls", sheet = 2) 
MedPay_2014 <- MedPay_2014[4:378, ]
colnames(MedPay_2014) <- MedPay_2014[1, ] 
MedPay_2014 <- MedPay_2014[6:375, ] 
MedPay_2014 <- MedPay_2014[, c(1,2,4)] 
MedPay_2014$`Median` <- as.numeric(MedPay_2014$`Median`)
MedPay_2014_E06 <- MedPay_2014 %>%
  filter(str_detect(`Code`, "^E06"))
MedPay_2014_E10 <- MedPay_2014 %>%
  filter(str_detect(`Code`, "^E10"))
MedPay_2014_E11 <- MedPay_2014 %>%
  filter(str_detect(`Code`, "^E11")) 
MedPay_2014_list <- list(MedPay_2014_E06, MedPay_2014_E10, MedPay_2014_E11) 
MedPay_2014 <- MedPay_2014_list %>% reduce(full_join)
MedPay_2014 <- MedPay_2014[order(MedPay_2014$`Description`), ]
BusJourneys_PrivateCars_TramJourneys_2014 <- BusJourneys_PrivateCars_TramJourneys %>%
  dplyr::filter(`Year` == 2014)

MedPay_2014_Bournemouth <- MedPay_2014 %>%
  dplyr::filter(`Description` == "Bournemouth UA") 
MedPay_2014_Bournemouth <- MedPay_2014_Bournemouth[ , c(3)] 
MedPay_2014_Poole <- MedPay_2014 %>%
  dplyr::filter(`Description` == "Poole UA") 
MedPay_2014_Poole <- MedPay_2014_Poole[ , c(3)] 
MedPay_2014_BournemouthPoole = (MedPay_2014_Bournemouth + MedPay_2014_Poole)/2
MedPay_2014 <- MedPay_2014 %>%
  dplyr::filter(`Description` != "Bournemouth UA" & `Description` != "Poole UA")  
MedPay_2014 <- MedPay_2014 %>% add_row(Description='Bournemouth and Poole', Code="E06000028", Median=MedPay_2014_BournemouthPoole[1,1]) 

MedPay_2014[MedPay_2014=="County Durham UA"]<-"Durham"

MedPay_2014 <- MedPay_2014[order(MedPay_2014$`Description`), ] 
MedPay_2014 <- MedPay_2014[1:100, ]
BusJourneys_PrivateCars_TramJourneys_2014 <- BusJourneys_PrivateCars_TramJourneys_2014[1:100, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2014 <- data.frame(BusJourneys_PrivateCars_TramJourneys_2014, MedPay_2014) 
BusJourneys_PrivateCars_TramJourneys_MedPay_2014 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2014[1:88, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2014 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2014[ , c(1:6, 9)]
#View(BusJourneys_PrivateCars_TramJourneys_MedPay_2014)

#=> Now for 2015's income data
if (file.exists('Sources of data/Income data/table82015revised') == FALSE) {
  zip.file <- "Sources of data/Income data/table82015revised.zip"
  unzip(zip.file, exdir = "Sources of data/Income data/table82015revised")
}
MedPay_2015 = read_excel("Sources of data/Income data/table82015revised/Home Geography Table 8.7a   Annual pay - Gross 2015.xls", sheet = 2) 
MedPay_2015 <- MedPay_2015[4:378, ]
colnames(MedPay_2015) <- MedPay_2015[1, ] 
MedPay_2015 <- MedPay_2015[6:375, ] 
MedPay_2015 <- MedPay_2015[, c(1,2,4)] 
MedPay_2015$`Median` <- as.numeric(MedPay_2015$`Median`)
MedPay_2015_E06 <- MedPay_2015 %>%
  filter(str_detect(`Code`, "^E06"))
MedPay_2015_E10 <- MedPay_2015 %>%
  filter(str_detect(`Code`, "^E10"))
MedPay_2015_E11 <- MedPay_2015 %>%
  filter(str_detect(`Code`, "^E11")) 
MedPay_2015_list <- list(MedPay_2015_E06, MedPay_2015_E10, MedPay_2015_E11) 
MedPay_2015 <- MedPay_2015_list %>% reduce(full_join)
MedPay_2015 <- MedPay_2015[order(MedPay_2015$`Description`), ]
BusJourneys_PrivateCars_TramJourneys_2015 <- BusJourneys_PrivateCars_TramJourneys %>%
  dplyr::filter(`Year` == 2015)

MedPay_2015_Bournemouth <- MedPay_2015 %>%
  dplyr::filter(`Description` == "Bournemouth UA") 
MedPay_2015_Bournemouth <- MedPay_2015_Bournemouth[ , c(3)] 
MedPay_2015_Poole <- MedPay_2015 %>%
  dplyr::filter(`Description` == "Poole UA") 
MedPay_2015_Poole <- MedPay_2015_Poole[ , c(3)] 
MedPay_2015_BournemouthPoole = (MedPay_2015_Bournemouth + MedPay_2015_Poole)/2
MedPay_2015 <- MedPay_2015 %>%
  dplyr::filter(`Description` != "Bournemouth UA" & `Description` != "Poole UA")  
MedPay_2015 <- MedPay_2015 %>% add_row(Description='Bournemouth and Poole', Code="E06000028", Median=MedPay_2015_BournemouthPoole[1,1]) 

MedPay_2015[MedPay_2015=="County Durham UA"]<-"Durham"

MedPay_2015 <- MedPay_2015[order(MedPay_2015$`Description`), ] 
MedPay_2015 <- MedPay_2015[1:100, ]
BusJourneys_PrivateCars_TramJourneys_2015 <- BusJourneys_PrivateCars_TramJourneys_2015[1:100, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2015 <- data.frame(BusJourneys_PrivateCars_TramJourneys_2015, MedPay_2015) 
BusJourneys_PrivateCars_TramJourneys_MedPay_2015 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2015[1:88, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2015 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2015[ , c(1:6, 9)]
#View(BusJourneys_PrivateCars_TramJourneys_MedPay_2015)

#=> Now for 2016's income data
if (file.exists('Sources of data/Income data/table82016revised') == FALSE) {
  zip.file <- "Sources of data/Income data/table82016revised.zip"
  unzip(zip.file, exdir = "Sources of data/Income data/table82016revised")
}
MedPay_2016 = read_excel("Sources of data/Income data/table82016revised/Home Geography Table 8.7a   Annual pay - Gross 2016.xls", sheet = 2) 
MedPay_2016 <- MedPay_2016[4:378, ]
colnames(MedPay_2016) <- MedPay_2016[1, ] 
MedPay_2016 <- MedPay_2016[6:375, ]
MedPay_2016 <- MedPay_2016[, c(1,2,4)] 
MedPay_2016$`Median` <- as.numeric(MedPay_2016$`Median`)
MedPay_2016_E06 <- MedPay_2016 %>%
  filter(str_detect(`Code`, "^E06"))
MedPay_2016_E10 <- MedPay_2016 %>%
  filter(str_detect(`Code`, "^E10"))
MedPay_2016_E11 <- MedPay_2016 %>%
  filter(str_detect(`Code`, "^E11")) 
MedPay_2016_list <- list(MedPay_2016_E06, MedPay_2016_E10, MedPay_2016_E11) 
MedPay_2016 <- MedPay_2016_list %>% reduce(full_join)
MedPay_2016 <- MedPay_2016[order(MedPay_2016$`Description`), ]
BusJourneys_PrivateCars_TramJourneys_2016 <- BusJourneys_PrivateCars_TramJourneys %>%
  dplyr::filter(`Year` == 2016)

MedPay_2016_Bournemouth <- MedPay_2016 %>%
  dplyr::filter(`Description` == "Bournemouth") ##Creates a dataframe with just the Bournemouth data in
MedPay_2016_Bournemouth <- MedPay_2016_Bournemouth[ , c(3)] 
MedPay_2016_Poole <- MedPay_2016 %>%
  dplyr::filter(`Description` == "Poole") ##Creates a dataframe with just the Poole data in
MedPay_2016_Poole <- MedPay_2016_Poole[ , c(3)] 
MedPay_2016_BournemouthPoole = (MedPay_2016_Bournemouth + MedPay_2016_Poole)/2
MedPay_2016 <- MedPay_2016 %>%
  dplyr::filter(`Description` != "Bournemouth" & `Description` != "Poole") ##Deletes the Bournemouth and Poole entries from the median pay dataset prior to the insertion of the combined Bournemouth and Poole row 
MedPay_2016 <- MedPay_2016 %>% add_row(Description='Bournemouth and Poole', Code="E06000028", Median=MedPay_2016_BournemouthPoole[1,1]) 

MedPay_2016[MedPay_2016=="County Durham"]<-"Durham" ##Renames the County Durham row to just Durham

MedPay_2016 <- MedPay_2016[order(MedPay_2016$`Description`), ] 
MedPay_2016 <- MedPay_2016[1:100, ]
BusJourneys_PrivateCars_TramJourneys_2016 <- BusJourneys_PrivateCars_TramJourneys_2016[1:100, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2016 <- data.frame(BusJourneys_PrivateCars_TramJourneys_2016, MedPay_2016) 
BusJourneys_PrivateCars_TramJourneys_MedPay_2016 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2016[1:88, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2016 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2016[ , c(1:6, 9)]
#View(BusJourneys_PrivateCars_TramJourneys_MedPay_2016)

#=> Now for 2017's income data
if (file.exists('Sources of data/Income data/table82017revised') == FALSE) {
  zip.file <- "Sources of data/Income data/table82017revised.zip"
  unzip(zip.file, exdir = "Sources of data/Income data/table82017revised")
}
MedPay_2017 = read_excel("Sources of data/Income data/table82017revised/Home Geography Table 8.7a   Annual pay - Gross 2017.xls", sheet = 2) 
MedPay_2017 <- MedPay_2017[4:378, ]
colnames(MedPay_2017) <- MedPay_2017[1, ] 
MedPay_2017 <- MedPay_2017[6:375, ]
MedPay_2017 <- MedPay_2017[, c(1,2,4)] 
MedPay_2017$`Median` <- as.numeric(MedPay_2017$`Median`)
MedPay_2017_E06 <- MedPay_2017 %>%
  filter(str_detect(`Code`, "^E06"))
MedPay_2017_E10 <- MedPay_2017 %>%
  filter(str_detect(`Code`, "^E10"))
MedPay_2017_E11 <- MedPay_2017 %>%
  filter(str_detect(`Code`, "^E11")) 
MedPay_2017_list <- list(MedPay_2017_E06, MedPay_2017_E10, MedPay_2017_E11) 
MedPay_2017 <- MedPay_2017_list %>% reduce(full_join)
MedPay_2017 <- MedPay_2017[order(MedPay_2017$`Description`), ]
BusJourneys_PrivateCars_TramJourneys_2017 <- BusJourneys_PrivateCars_TramJourneys %>%
  dplyr::filter(`Year` == 2017)

MedPay_2017_Bournemouth <- MedPay_2017 %>%
  dplyr::filter(`Description` == "Bournemouth") 
MedPay_2017_Bournemouth <- MedPay_2017_Bournemouth[ , c(3)] 
MedPay_2017_Poole <- MedPay_2017 %>%
  dplyr::filter(`Description` == "Poole") 
MedPay_2017_Poole <- MedPay_2017_Poole[ , c(3)] 
MedPay_2017_BournemouthPoole = (MedPay_2017_Bournemouth + MedPay_2017_Poole)/2
MedPay_2017 <- MedPay_2017 %>%
  dplyr::filter(`Description` != "Bournemouth" & `Description` != "Poole") 
MedPay_2017 <- MedPay_2017 %>% add_row(Description='Bournemouth and Poole', Code="E06000028", Median=MedPay_2017_BournemouthPoole[1,1]) 

MedPay_2017[MedPay_2017=="County Durham"]<-"Durham" 

MedPay_2017 <- MedPay_2017[order(MedPay_2017$`Description`), ] 
MedPay_2017 <- MedPay_2017[1:100, ]
BusJourneys_PrivateCars_TramJourneys_2017 <- BusJourneys_PrivateCars_TramJourneys_2017[1:100, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2017 <- data.frame(BusJourneys_PrivateCars_TramJourneys_2017, MedPay_2017) 
BusJourneys_PrivateCars_TramJourneys_MedPay_2017 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2017[1:88, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2017 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2017[ , c(1:6, 9)]
#View(BusJourneys_PrivateCars_TramJourneys_MedPay_2017)

#=> Now for 2018's income data
if (file.exists('Sources of data/Income data/table82018revised') == FALSE) {
  zip.file <- "Sources of data/Income data/table82018revised.zip"
  unzip(zip.file, exdir = "Sources of data/Income data/table82018revised")
}
MedPay_2018 = read_excel("Sources of data/Income data/table82018revised/Home Geography Table 8.7a   Annual pay - Gross 2018.xls", sheet = 2) 
MedPay_2018 <- MedPay_2018[4:368, ] ##Gets rid of the first few lines and of the Scottish/Welsh entries
colnames(MedPay_2018) <- MedPay_2018[1, ] 
MedPay_2018 <- MedPay_2018[6:365, ] ##Gets rid of a few more unneeded lines
MedPay_2018 <- MedPay_2018[, c(1,2,4)] 
MedPay_2018$`Median` <- as.numeric(MedPay_2018$`Median`)
MedPay_2018_E06 <- MedPay_2018 %>%
  filter(str_detect(`Code`, "^E06"))
MedPay_2018_E10 <- MedPay_2018 %>%
  filter(str_detect(`Code`, "^E10"))
MedPay_2018_E11 <- MedPay_2018 %>%
  filter(str_detect(`Code`, "^E11")) 
MedPay_2018_list <- list(MedPay_2018_E06, MedPay_2018_E10, MedPay_2018_E11) 
MedPay_2018 <- MedPay_2018_list %>% reduce(full_join)
MedPay_2018 <- MedPay_2018[order(MedPay_2018$`Description`), ]
BusJourneys_PrivateCars_TramJourneys_2018 <- BusJourneys_PrivateCars_TramJourneys %>%
  dplyr::filter(`Year` == 2018)

MedPay_2018[MedPay_2018=="County Durham"]<-"Durham" 

MedPay_2018 <- MedPay_2018[order(MedPay_2018$`Description`), ] 
MedPay_2018 <- MedPay_2018[1:100, ]
BusJourneys_PrivateCars_TramJourneys_2018 <- BusJourneys_PrivateCars_TramJourneys_2018[1:100, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2018 <- data.frame(BusJourneys_PrivateCars_TramJourneys_2018, MedPay_2018) 
BusJourneys_PrivateCars_TramJourneys_MedPay_2018 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2018[1:88, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2018 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2018[ , c(1:6, 9)]
#View(BusJourneys_PrivateCars_TramJourneys_MedPay_2018)

#=> Now for 2019's income data
if (file.exists('Sources of data/Income data/table82019revised') == FALSE) {
  zip.file <- "Sources of data/Income data/table82019revised.zip"
  unzip(zip.file, exdir = "Sources of data/Income data/table82019revised")
}
MedPay_2019 = read_excel("Sources of data/Income data/table82019revised/Home Geography Table 8.7a   Annual pay - Gross 2019.xls", sheet = 2) 
MedPay_2019 <- MedPay_2019[4:364, ] ##Gets rid of the first few lines and of the Scottish/Welsh entries
colnames(MedPay_2019) <- MedPay_2019[1, ] 
MedPay_2019 <- MedPay_2019[6:361, ] ##Gets rid of a few more unneeded lines
MedPay_2019 <- MedPay_2019[, c(1,2,4)] 
MedPay_2019$`Median` <- as.numeric(MedPay_2019$`Median`)
MedPay_2019_E06 <- MedPay_2019 %>%
  filter(str_detect(`Code`, "^E06"))
MedPay_2019_E10 <- MedPay_2019 %>%
  filter(str_detect(`Code`, "^E10"))
MedPay_2019_E11 <- MedPay_2019 %>%
  filter(str_detect(`Code`, "^E11")) 
MedPay_2019_list <- list(MedPay_2019_E06, MedPay_2019_E10, MedPay_2019_E11) 
MedPay_2019 <- MedPay_2019_list %>% reduce(full_join)
MedPay_2019 <- MedPay_2019[order(MedPay_2019$`Description`), ]
BusJourneys_PrivateCars_TramJourneys_2019 <- BusJourneys_PrivateCars_TramJourneys %>%
  dplyr::filter(`Year` == 2019)

MedPay_2019[MedPay_2019=="County Durham"]<-"Durham" 

MedPay_2019 <- MedPay_2019[order(MedPay_2019$`Description`), ] 
MedPay_2019 <- MedPay_2019[1:100, ]
BusJourneys_PrivateCars_TramJourneys_2019 <- BusJourneys_PrivateCars_TramJourneys_2019[1:100, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2019 <- data.frame(BusJourneys_PrivateCars_TramJourneys_2019, MedPay_2019) 
BusJourneys_PrivateCars_TramJourneys_MedPay_2019 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2019[1:88, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2019 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2019[ , c(1:6, 9)]
#View(BusJourneys_PrivateCars_TramJourneys_MedPay_2019)

#=> Now for 2020's income data
if (file.exists('Sources of data/Income data/table82020revised') == FALSE) {
  zip.file <- "Sources of data/Income data/table82020revised.zip"
  unzip(zip.file, exdir = "Sources of data/Income data/table82020revised")
}
MedPay_2020 = read_excel("Sources of data/Income data/table82020revised/Home Geography Table 8.7a   Annual pay - Gross 2020.xls", sheet = 2) 
MedPay_2020 <- MedPay_2020[4:358, ] ##Gets rid of the first few lines and of the Scottish/Welsh entries
colnames(MedPay_2020) <- MedPay_2020[1, ] 
MedPay_2020 <- MedPay_2020[6:355, ] ##Gets rid of a few more unneeded lines
MedPay_2020 <- MedPay_2020[, c(1,2,4)] 
MedPay_2020$`Median` <- as.numeric(MedPay_2020$`Median`)
MedPay_2020_E06 <- MedPay_2020 %>%
  filter(str_detect(`Code`, "^E06"))
MedPay_2020_E10 <- MedPay_2020 %>%
  filter(str_detect(`Code`, "^E10"))
MedPay_2020_E11 <- MedPay_2020 %>%
  filter(str_detect(`Code`, "^E11")) 
MedPay_2020_list <- list(MedPay_2020_E06, MedPay_2020_E10, MedPay_2020_E11) 
MedPay_2020 <- MedPay_2020_list %>% reduce(full_join)
MedPay_2020 <- MedPay_2020[order(MedPay_2020$`Description`), ]
BusJourneys_PrivateCars_TramJourneys_2020 <- BusJourneys_PrivateCars_TramJourneys %>%
  dplyr::filter(`Year` == 2020)

MedPay_2020[MedPay_2020=="County Durham UA"]<-"Durham" ##Renames County Durham UA to Durham in the medium pay data

#=> From 2020 onwards in the median pay data, Northamptonshire is split into North Northamptonshire and West Northamptonshire (as we've seen before). So, as before, I am going to combine them into Northamptonshire. 
MedPay_2020_North_Northamptonshire <- MedPay_2020 %>%
  dplyr::filter(`Description` == "North Northamptonshire UA") ##Isolates the North Northamptonshire row of the medium pay data
MedPay_2020_North_Northamptonshire <- MedPay_2020_North_Northamptonshire[ , c(3)] ##Selects specifically for the medium pay for North Northamptonshire
MedPay_2020_West_Northamptonshire <- MedPay_2020 %>%
  dplyr::filter(`Description` == "West Northamptonshire UA") 
MedPay_2020_West_Northamptonshire <- MedPay_2020_West_Northamptonshire[ , c(3)] ##Does the same for West Northamptonshire
MedPay_2020_Northamptonshire = (MedPay_2020_North_Northamptonshire + MedPay_2020_West_Northamptonshire)/2 ##The number of jobs in North Northamptonshire and West Northamptonshire is comparable, so to get an approximation for the median salary in Northamptonshire I am just going to take an average of North Northamptonshire's median salary and West Northamptonshire's Median salary
MedPay_2020 <- MedPay_2020 %>%
  dplyr::filter(`Description` != "North Northamptonshire UA" & `Description` != "West Northamptonshire UA") ##Removes the old North Northamptonshire and West Northamptonshire entries from the median pay data before the insertion of the new combined Northamptonshire entry
MedPay_2020 <- MedPay_2020 %>% add_row(Description='Northamptonshire UA', Code="E10000021", Median=MedPay_2020_Northamptonshire[1,1]) ##Inserts the new combined Northamptonshire entry into the median pay data

MedPay_2020 <- MedPay_2020[order(MedPay_2020$`Description`), ] 
MedPay_2020 <- MedPay_2020[1:100, ]
BusJourneys_PrivateCars_TramJourneys_2020 <- BusJourneys_PrivateCars_TramJourneys_2020[1:100, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2020 <- data.frame(BusJourneys_PrivateCars_TramJourneys_2020, MedPay_2020) 
BusJourneys_PrivateCars_TramJourneys_MedPay_2020 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2020[1:88, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2020 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2020[ , c(1:6, 9)]
#View(BusJourneys_PrivateCars_TramJourneys_MedPay_2020)

#=> Now for 2021's income data
if (file.exists('Sources of data/Income data/ashetable82021revised') == FALSE) {
  zip.file <- "Sources of data/Income data/ashetable82021revised.zip"
  unzip(zip.file, exdir = "Sources of data/Income data/ashetable82021revised")
}
MedPay_2021 = read_excel("Sources of data/Income data/ashetable82021revised/Home Geography Table 8.7a   Annual pay - Gross 2021.xls", sheet = 2) 
MedPay_2021 <- MedPay_2021[4:358, ] 
colnames(MedPay_2021) <- MedPay_2021[1, ] 
MedPay_2021 <- MedPay_2021[6:355, ] 
MedPay_2021 <- MedPay_2021[, c(1,2,4)] 
MedPay_2021$`Median` <- as.numeric(MedPay_2021$`Median`)
MedPay_2021_E06 <- MedPay_2021 %>%
  filter(str_detect(`Code`, "^E06"))
MedPay_2021_E10 <- MedPay_2021 %>%
  filter(str_detect(`Code`, "^E10"))
MedPay_2021_E11 <- MedPay_2021 %>%
  filter(str_detect(`Code`, "^E11")) 
MedPay_2021_list <- list(MedPay_2021_E06, MedPay_2021_E10, MedPay_2021_E11) 
MedPay_2021 <- MedPay_2021_list %>% reduce(full_join)
MedPay_2021 <- MedPay_2021[order(MedPay_2021$`Description`), ]
BusJourneys_PrivateCars_TramJourneys_2021 <- BusJourneys_PrivateCars_TramJourneys %>%
  dplyr::filter(`Year` == 2021)

MedPay_2021[MedPay_2021=="County Durham UA"]<-"Durham"

MedPay_2021_North_Northamptonshire <- MedPay_2021 %>%
  dplyr::filter(`Description` == "North Northamptonshire UA") 
MedPay_2021_North_Northamptonshire <- MedPay_2021_North_Northamptonshire[ , c(3)] 
MedPay_2021_West_Northamptonshire <- MedPay_2021 %>%
  dplyr::filter(`Description` == "West Northamptonshire UA") 
MedPay_2021_West_Northamptonshire <- MedPay_2021_West_Northamptonshire[ , c(3)]
MedPay_2021_Northamptonshire = (MedPay_2021_North_Northamptonshire + MedPay_2021_West_Northamptonshire)/2 
MedPay_2021 <- MedPay_2021 %>%
  dplyr::filter(`Description` != "North Northamptonshire UA" & `Description` != "West Northamptonshire UA") 
MedPay_2021 <- MedPay_2021 %>% add_row(Description='Northamptonshire UA', Code="E10000021", Median=MedPay_2021_Northamptonshire[1,1])

MedPay_2021 <- MedPay_2021[order(MedPay_2021$`Description`), ] 
MedPay_2021 <- MedPay_2021[1:100, ]
BusJourneys_PrivateCars_TramJourneys_2021 <- BusJourneys_PrivateCars_TramJourneys_2021[1:100, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2021 <- data.frame(BusJourneys_PrivateCars_TramJourneys_2021, MedPay_2021) 
BusJourneys_PrivateCars_TramJourneys_MedPay_2021 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2021[1:88, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2021 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2021[ , c(1:6, 9)]
#View(BusJourneys_PrivateCars_TramJourneys_MedPay_2021)

# => Now for 2022's income data
if (file.exists('Sources of data/Income data/ashetable82022revised') == FALSE) {
  zip.file <- "Sources of data/Income data/ashetable82022revised.zip"
  unzip(zip.file, exdir = "Sources of data/Income data/ashetable82022revised")
}
MedPay_2022 = read_excel("Sources of data/Income data/ashetable82022revised/For Publishing/Home Geography Table 8.7a   Annual pay - Gross 2022.xls", sheet = 2) 
MedPay_2022 <- MedPay_2022[4:342, ] ##Gets rid of a few unneeded header rows and the Scottish/Welsh data
colnames(MedPay_2022) <- MedPay_2022[1, ] 
MedPay_2022 <- MedPay_2022[6:339, ] ##Gets rid of some more unneeded data
MedPay_2022 <- MedPay_2022[, c(1,2,4)] 
MedPay_2022$`Median` <- as.numeric(MedPay_2022$`Median`)
MedPay_2022_E06 <- MedPay_2022 %>%
  filter(str_detect(`Code`, "^E06"))
MedPay_2022_E10 <- MedPay_2022 %>%
  filter(str_detect(`Code`, "^E10"))
MedPay_2022_E11 <- MedPay_2022 %>%
  filter(str_detect(`Code`, "^E11")) 
MedPay_2022_list <- list(MedPay_2022_E06, MedPay_2022_E10, MedPay_2022_E11) 
MedPay_2022 <- MedPay_2022_list %>% reduce(full_join)
MedPay_2022 <- MedPay_2022[order(MedPay_2022$`Description`), ]
BusJourneys_PrivateCars_TramJourneys_2022 <- BusJourneys_PrivateCars_TramJourneys %>%
  dplyr::filter(`Year` == 2022)

MedPay_2022[MedPay_2022=="County Durham UA"]<-"Durham"

MedPay_2022_North_Northamptonshire <- MedPay_2022 %>%
  dplyr::filter(`Description` == "North Northamptonshire UA") 
MedPay_2022_North_Northamptonshire <- MedPay_2022_North_Northamptonshire[ , c(3)] 
MedPay_2022_West_Northamptonshire <- MedPay_2022 %>%
  dplyr::filter(`Description` == "West Northamptonshire UA") 
MedPay_2022_West_Northamptonshire <- MedPay_2022_West_Northamptonshire[ , c(3)]
MedPay_2022_Northamptonshire = (MedPay_2022_North_Northamptonshire + MedPay_2022_West_Northamptonshire)/2 
MedPay_2022 <- MedPay_2022 %>%
  dplyr::filter(`Description` != "North Northamptonshire UA" & `Description` != "West Northamptonshire UA") 
MedPay_2022 <- MedPay_2022 %>% add_row(Description='Northamptonshire UA', Code="E10000021", Median=MedPay_2022_Northamptonshire[1,1])

#=> From 2022 onwards, Cumbria is split into 'Cumberland' and 'Westmorland and Furness', also as we've seen before. So, we're going to re-combine them into Cumbria, using the same process as above
MedPay_2022_Cumberland <- MedPay_2022 %>%
  dplyr::filter(`Description` == "Cumberland UA") ##Isolates just the Cumberland row from the median pay data
MedPay_2022_Cumberland <- MedPay_2022_Cumberland[ , c(3)] ##Now just isolates the median salary data for Cumberland 
MedPay_2022_Westmorland <- MedPay_2022 %>%
  dplyr::filter(`Description` == "Westmorland and Furness UA") 
MedPay_2022_Westmorland <- MedPay_2022_Westmorland[ , c(3)] ##Does the same for Westmorland and Furness 
MedPay_2022_Cumbria = (MedPay_2022_Cumberland + MedPay_2022_Westmorland)/2 
MedPay_2022 <- MedPay_2022 %>%
  dplyr::filter(`Description` != "Cumberland UA" & `Description` != "Westmorland and Furness UA") 
MedPay_2022 <- MedPay_2022 %>% add_row(Description='Cumbria', Code="E10000006", Median=MedPay_2022_Cumbria[1,1])

MedPay_2022 <- MedPay_2022[order(MedPay_2022$`Description`), ] 
MedPay_2022 <- MedPay_2022[1:100, ]
BusJourneys_PrivateCars_TramJourneys_2022 <- BusJourneys_PrivateCars_TramJourneys_2022[1:100, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2022 <- data.frame(BusJourneys_PrivateCars_TramJourneys_2022, MedPay_2022) 
BusJourneys_PrivateCars_TramJourneys_MedPay_2022 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2022[1:88, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2022 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2022[ , c(1:6, 9)]
#View(BusJourneys_PrivateCars_TramJourneys_MedPay_2022)

#=> Now for 2023's income data
if (file.exists('Sources of data/Income data/ashetable82023revised') == FALSE) {
  zip.file <- "Sources of data/Income data/ashetable82023revised.zip"
  unzip(zip.file, exdir = "Sources of data/Income data/ashetable82023revised")
}
MedPay_2023 = read_excel("Sources of data/Income data/ashetable82023revised/Home Geography Table 8.7a   Annual pay - Gross 2023.xlsx", sheet = 2) 
MedPay_2023 <- MedPay_2023[4:342, ] 
colnames(MedPay_2023) <- MedPay_2023[1, ] 
MedPay_2023 <- MedPay_2023[6:339, ] 
MedPay_2023 <- MedPay_2023[, c(1,2,4)] 
MedPay_2023$`Median` <- as.numeric(MedPay_2023$`Median`)
MedPay_2023_E06 <- MedPay_2023 %>%
  filter(str_detect(`Code`, "^E06"))
MedPay_2023_E10 <- MedPay_2023 %>%
  filter(str_detect(`Code`, "^E10"))
MedPay_2023_E11 <- MedPay_2023 %>%
  filter(str_detect(`Code`, "^E11")) 
MedPay_2023_list <- list(MedPay_2023_E06, MedPay_2023_E10, MedPay_2023_E11) 
MedPay_2023 <- MedPay_2023_list %>% reduce(full_join)
MedPay_2023 <- MedPay_2023[order(MedPay_2023$`Description`), ]
BusJourneys_PrivateCars_TramJourneys_2023 <- BusJourneys_PrivateCars_TramJourneys %>%
  dplyr::filter(`Year` == 2023)

MedPay_2023[MedPay_2023=="County Durham UA"]<-"Durham"

MedPay_2023_North_Northamptonshire <- MedPay_2023 %>%
  dplyr::filter(`Description` == "North Northamptonshire UA") 
MedPay_2023_North_Northamptonshire <- MedPay_2023_North_Northamptonshire[ , c(3)] 
MedPay_2023_West_Northamptonshire <- MedPay_2023 %>%
  dplyr::filter(`Description` == "West Northamptonshire UA") 
MedPay_2023_West_Northamptonshire <- MedPay_2023_West_Northamptonshire[ , c(3)]
MedPay_2023_Northamptonshire = (MedPay_2023_North_Northamptonshire + MedPay_2023_West_Northamptonshire)/2 
MedPay_2023 <- MedPay_2023 %>%
  dplyr::filter(`Description` != "North Northamptonshire UA" & `Description` != "West Northamptonshire UA") 
MedPay_2023 <- MedPay_2023 %>% add_row(Description='Northamptonshire UA', Code="E10000021", Median=MedPay_2023_Northamptonshire[1,1])

MedPay_2023_Cumberland <- MedPay_2023 %>%
  dplyr::filter(`Description` == "Cumberland UA") 
MedPay_2023_Cumberland <- MedPay_2023_Cumberland[ , c(3)] 
MedPay_2023_Westmorland <- MedPay_2023 %>%
  dplyr::filter(`Description` == "Westmorland and Furness UA") 
MedPay_2023_Westmorland <- MedPay_2023_Westmorland[ , c(3)]  
MedPay_2023_Cumbria = (MedPay_2023_Cumberland + MedPay_2023_Westmorland)/2 
MedPay_2023 <- MedPay_2023 %>%
  dplyr::filter(`Description` != "Cumberland UA" & `Description` != "Westmorland and Furness UA") 
MedPay_2023 <- MedPay_2023 %>% add_row(Description='Cumbria', Code="E10000006", Median=MedPay_2023_Cumbria[1,1])

MedPay_2023 <- MedPay_2023[order(MedPay_2023$`Description`), ] 
MedPay_2023 <- MedPay_2023[1:100, ]
BusJourneys_PrivateCars_TramJourneys_2023 <- BusJourneys_PrivateCars_TramJourneys_2023[1:100, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2023 <- data.frame(BusJourneys_PrivateCars_TramJourneys_2023, MedPay_2023) 
BusJourneys_PrivateCars_TramJourneys_MedPay_2023 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2023[1:88, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2023 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2023[ , c(1:6, 9)]
#View(BusJourneys_PrivateCars_TramJourneys_MedPay_2023)

#=> Now for 2024's income data
if (file.exists('Sources of data/Income data/ashetable82024revised') == FALSE) {
  zip.file <- "Sources of data/Income data/ashetable82024revised.zip"
  unzip(zip.file, exdir = "Sources of data/Income data/ashetable82024revised")
}
MedPay_2024 = read_excel("Sources of data/Income data/ashetable82024revised/Home Geography Table 8.7a   Annual pay - Gross 2024.xlsx", sheet = 2) 
MedPay_2024 <- MedPay_2024[4:342, ] 
colnames(MedPay_2024) <- MedPay_2024[1, ] 
MedPay_2024 <- MedPay_2024[6:339, ] 
MedPay_2024 <- MedPay_2024[, c(1,2,4)] 
MedPay_2024$`Median` <- as.numeric(MedPay_2024$`Median`)
MedPay_2024_E06 <- MedPay_2024 %>%
  filter(str_detect(`Code`, "^E06"))
MedPay_2024_E10 <- MedPay_2024 %>%
  filter(str_detect(`Code`, "^E10"))
MedPay_2024_E11 <- MedPay_2024 %>%
  filter(str_detect(`Code`, "^E11")) 
MedPay_2024_list <- list(MedPay_2024_E06, MedPay_2024_E10, MedPay_2024_E11) 
MedPay_2024 <- MedPay_2024_list %>% reduce(full_join)
MedPay_2024 <- MedPay_2024[order(MedPay_2024$`Description`), ]
BusJourneys_PrivateCars_TramJourneys_2024 <- BusJourneys_PrivateCars_TramJourneys %>%
  dplyr::filter(`Year` == 2024)

MedPay_2024[MedPay_2024=="County Durham UA"]<-"Durham"

MedPay_2024_North_Northamptonshire <- MedPay_2024 %>%
  dplyr::filter(`Description` == "North Northamptonshire UA") 
MedPay_2024_North_Northamptonshire <- MedPay_2024_North_Northamptonshire[ , c(3)] 
MedPay_2024_West_Northamptonshire <- MedPay_2024 %>%
  dplyr::filter(`Description` == "West Northamptonshire UA") 
MedPay_2024_West_Northamptonshire <- MedPay_2024_West_Northamptonshire[ , c(3)]
MedPay_2024_Northamptonshire = (MedPay_2024_North_Northamptonshire + MedPay_2024_West_Northamptonshire)/2 
MedPay_2024 <- MedPay_2024 %>%
  dplyr::filter(`Description` != "North Northamptonshire UA" & `Description` != "West Northamptonshire UA") 
MedPay_2024 <- MedPay_2024 %>% add_row(Description='Northamptonshire UA', Code="E10000021", Median=MedPay_2024_Northamptonshire[1,1])

MedPay_2024_Cumberland <- MedPay_2024 %>%
  dplyr::filter(`Description` == "Cumberland UA") 
MedPay_2024_Cumberland <- MedPay_2024_Cumberland[ , c(3)] 
MedPay_2024_Westmorland <- MedPay_2024 %>%
  dplyr::filter(`Description` == "Westmorland and Furness UA") 
MedPay_2024_Westmorland <- MedPay_2024_Westmorland[ , c(3)]  
MedPay_2024_Cumbria = (MedPay_2024_Cumberland + MedPay_2024_Westmorland)/2 
MedPay_2024 <- MedPay_2024 %>%
  dplyr::filter(`Description` != "Cumberland UA" & `Description` != "Westmorland and Furness UA") 
MedPay_2024 <- MedPay_2024 %>% add_row(Description='Cumbria', Code="E10000006", Median=MedPay_2024_Cumbria[1,1])

MedPay_2024 <- MedPay_2024[order(MedPay_2024$`Description`), ] 
MedPay_2024 <- MedPay_2024[1:100, ]
BusJourneys_PrivateCars_TramJourneys_2024 <- BusJourneys_PrivateCars_TramJourneys_2024[1:100, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2024 <- data.frame(BusJourneys_PrivateCars_TramJourneys_2024, MedPay_2024) 
BusJourneys_PrivateCars_TramJourneys_MedPay_2024 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2024[1:88, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2024 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2024[ , c(1:6, 9)]
#View(BusJourneys_PrivateCars_TramJourneys_MedPay_2024)

#=> Finally for 2025's income data
if (file.exists('Sources of data/Income data/ashetable82025provisional') == FALSE) {
  zip.file <- "Sources of data/Income data/ashetable82025provisional.zip"
  unzip(zip.file, exdir = "Sources of data/Income data/ashetable82025provisional")
}
MedPay_2025 = read_excel("Sources of data/Income data/ashetable82025provisional/PROV - Home Geography Table 8.7a   Annual pay - Gross 2025.xlsx", sheet = 2) 
MedPay_2025 <- MedPay_2025[4:342, ] 
colnames(MedPay_2025) <- MedPay_2025[1, ] 
MedPay_2025 <- MedPay_2025[6:339, ] 
MedPay_2025 <- MedPay_2025[, c(1,2,4)] 
MedPay_2025$`Median` <- as.numeric(MedPay_2025$`Median`)
MedPay_2025_E06 <- MedPay_2025 %>%
  filter(str_detect(`Code`, "^E06"))
MedPay_2025_E10 <- MedPay_2025 %>%
  filter(str_detect(`Code`, "^E10"))
MedPay_2025_E11 <- MedPay_2025 %>%
  filter(str_detect(`Code`, "^E11")) 
MedPay_2025_list <- list(MedPay_2025_E06, MedPay_2025_E10, MedPay_2025_E11) 
MedPay_2025 <- MedPay_2025_list %>% reduce(full_join)
MedPay_2025 <- MedPay_2025[order(MedPay_2025$`Description`), ]
BusJourneys_PrivateCars_TramJourneys_2025 <- BusJourneys_PrivateCars_TramJourneys %>%
  dplyr::filter(`Year` == 2025)

MedPay_2025[MedPay_2025=="County Durham UA"]<-"Durham"

MedPay_2025_North_Northamptonshire <- MedPay_2025 %>%
  dplyr::filter(`Description` == "North Northamptonshire UA") 
MedPay_2025_North_Northamptonshire <- MedPay_2025_North_Northamptonshire[ , c(3)] 
MedPay_2025_West_Northamptonshire <- MedPay_2025 %>%
  dplyr::filter(`Description` == "West Northamptonshire UA") 
MedPay_2025_West_Northamptonshire <- MedPay_2025_West_Northamptonshire[ , c(3)]
MedPay_2025_Northamptonshire = (MedPay_2025_North_Northamptonshire + MedPay_2025_West_Northamptonshire)/2 
MedPay_2025 <- MedPay_2025 %>%
  dplyr::filter(`Description` != "North Northamptonshire UA" & `Description` != "West Northamptonshire UA") 
MedPay_2025 <- MedPay_2025 %>% add_row(Description='Northamptonshire UA', Code="E10000021", Median=MedPay_2025_Northamptonshire[1,1])

MedPay_2025_Cumberland <- MedPay_2025 %>%
  dplyr::filter(`Description` == "Cumberland UA") 
MedPay_2025_Cumberland <- MedPay_2025_Cumberland[ , c(3)] 
MedPay_2025_Westmorland <- MedPay_2025 %>%
  dplyr::filter(`Description` == "Westmorland and Furness UA") 
MedPay_2025_Westmorland <- MedPay_2025_Westmorland[ , c(3)]  
MedPay_2025_Cumbria = (MedPay_2025_Cumberland + MedPay_2025_Westmorland)/2 
MedPay_2025 <- MedPay_2025 %>%
  dplyr::filter(`Description` != "Cumberland UA" & `Description` != "Westmorland and Furness UA") 
MedPay_2025 <- MedPay_2025 %>% add_row(Description='Cumbria', Code="E10000006", Median=MedPay_2025_Cumbria[1,1])

MedPay_2025 <- MedPay_2025[order(MedPay_2025$`Description`), ] 
MedPay_2025 <- MedPay_2025[1:100, ]
BusJourneys_PrivateCars_TramJourneys_2025 <- BusJourneys_PrivateCars_TramJourneys_2025[1:100, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2025 <- data.frame(BusJourneys_PrivateCars_TramJourneys_2025, MedPay_2025) 
BusJourneys_PrivateCars_TramJourneys_MedPay_2025 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2025[1:88, ] 
BusJourneys_PrivateCars_TramJourneys_MedPay_2025 <- BusJourneys_PrivateCars_TramJourneys_MedPay_2025[ , c(1:6, 9)]
#View(BusJourneys_PrivateCars_TramJourneys_MedPay_2025)

#=> Now I need to combine all of the individual above created dataframes for each year into one master dataframe
BusJourneys_PrivateCars_TramJourneys_MedPay_list <- list(BusJourneys_PrivateCars_TramJourneys_MedPay_2010, BusJourneys_PrivateCars_TramJourneys_MedPay_2011, BusJourneys_PrivateCars_TramJourneys_MedPay_2012, BusJourneys_PrivateCars_TramJourneys_MedPay_2013, BusJourneys_PrivateCars_TramJourneys_MedPay_2014, BusJourneys_PrivateCars_TramJourneys_MedPay_2015, BusJourneys_PrivateCars_TramJourneys_MedPay_2016, BusJourneys_PrivateCars_TramJourneys_MedPay_2017, BusJourneys_PrivateCars_TramJourneys_MedPay_2018, BusJourneys_PrivateCars_TramJourneys_MedPay_2019, BusJourneys_PrivateCars_TramJourneys_MedPay_2020, BusJourneys_PrivateCars_TramJourneys_MedPay_2021, BusJourneys_PrivateCars_TramJourneys_MedPay_2022, BusJourneys_PrivateCars_TramJourneys_MedPay_2023, BusJourneys_PrivateCars_TramJourneys_MedPay_2024, BusJourneys_PrivateCars_TramJourneys_MedPay_2025) ##Creates a list of the 16 dataframes
BusJourneys_PrivateCars_TramJourneys_MedPay <- BusJourneys_PrivateCars_TramJourneys_MedPay_list %>% reduce(full_join) ##Combines the list of the sixteen individual year dataframes into one new 'master' dataframe
BusJourneys_PrivateCars_TramJourneys_MedPay <- BusJourneys_PrivateCars_TramJourneys_MedPay[order(BusJourneys_PrivateCars_TramJourneys_MedPay$`LA.or.Region`), ] ##Orders the data in alphabetical order once again 
rownames(BusJourneys_PrivateCars_TramJourneys_MedPay) <- 1:nrow(BusJourneys_PrivateCars_TramJourneys_MedPay) ##Resets the row names
colnames(BusJourneys_PrivateCars_TramJourneys_MedPay)[colnames(BusJourneys_PrivateCars_TramJourneys_MedPay) == "Median"] <- "Nominal.median.pay.in.each.region" ##Renames the column names to the first row of the data, which is the title row
#View(BusJourneys_PrivateCars_TramJourneys_MedPay)

##**CONVERTING NOMINAL WAGES INTO REAL WAGES AND ADDING INTO THE MASTER DATAFRAME**
Infl = read.csv("Sources of data/series-190826.csv") ##Reads the inflation data into R
Infl <- Infl[29:44, ] ##Filters for only the rows of data we're interested in
Infl$`CPI.ANNUAL.RATE.00..ALL.ITEMS.2015.100` <- as.numeric(Infl$`CPI.ANNUAL.RATE.00..ALL.ITEMS.2015.100`) ##Converts inflation numbers into numerics
rownames(Infl) <- 1:nrow(Infl) ##Resets the row names
Infl[ , c(2)] <- 100+Infl[ ,c(2)] ##Setting up the inflation index
Infl[1, c(2)] <- 100 ##Setting 2010 = 100 for the index
Infl[ , c(2)] <- Infl[ ,c(2)]/100 ##Turning percentage into proportion
for (i in 2:16){
  Infl[i, c(2)] <- Infl[i, c(2)] * Infl[i-1, c(2)]
} ##Turns the inflation numbers into a cumulative index
BusJourneys_PrivateCars_TramJourneys_MedPay_Years <- BusJourneys_PrivateCars_TramJourneys_MedPay[ , c(3)] ##Creates a dataframe which is just the year column from the main dataframe
BusJourneys_PrivateCars_TramJourneys_MedPay_Years <- as.numeric(BusJourneys_PrivateCars_TramJourneys_MedPay_Years) ##Turns this year series dataframe into numbers
for (i in 1:16){
  BusJourneys_PrivateCars_TramJourneys_MedPay_Years[BusJourneys_PrivateCars_TramJourneys_MedPay_Years==(2009+i)]<-Infl[i, c(2)]
} ##Systematically replaces in this dataframe 2010 with the first inflation index number, 2011 with the second inflation index number, 2012 with the third inflation index number etc.
BusJourneys_PrivateCars_TramJourneys_RealPay <- data.frame(BusJourneys_PrivateCars_TramJourneys_MedPay, BusJourneys_PrivateCars_TramJourneys_MedPay_Years) ##Stitches this inflation index column onto the main dataframe 
colnames(BusJourneys_PrivateCars_TramJourneys_RealPay)[colnames(BusJourneys_PrivateCars_TramJourneys_RealPay) == "BusJourneys_PrivateCars_TramJourneys_MedPay_Years"] <- "Inflation.Index" ##Renames the inflation index column accordingly
BusJourneys_PrivateCars_TramJourneys_MedPay_MedPayOnly <- BusJourneys_PrivateCars_TramJourneys_MedPay[ , c(7)] ##Isolates the nominal wages column
BusJourneys_PrivateCars_TramJourneys_RealPayOnly <- BusJourneys_PrivateCars_TramJourneys_MedPay_MedPayOnly/BusJourneys_PrivateCars_TramJourneys_MedPay_Years ##Creates a column vector equal to real wages, by dividing nominal wages by the inflation index
BusJourneys_PrivateCars_TramJourneys_RealPay <- data.frame(BusJourneys_PrivateCars_TramJourneys_RealPay, BusJourneys_PrivateCars_TramJourneys_RealPayOnly) ##Stitches the real wages column onto the main dataframe
colnames(BusJourneys_PrivateCars_TramJourneys_RealPay)[colnames(BusJourneys_PrivateCars_TramJourneys_RealPay) == "BusJourneys_PrivateCars_TramJourneys_RealPayOnly"] <- "Real.median.pay.in.each.region" ##Renames the real pay column accordingly
#View(BusJourneys_PrivateCars_TramJourneys_RealPay)