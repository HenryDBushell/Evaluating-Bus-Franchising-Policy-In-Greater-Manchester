source("Code for project/03 Combining and Matching Bus Journey and Car Ownership Data.R")

##**WRANGLING THE TRAM/LIGHT RAIL DATA**
if (file.exists('Sources of data/light-rail-and-tram-statistics-year-ending-march-2025') == FALSE) {
  zip.file <- "Sources of data/light-rail-and-tram-statistics-year-ending-march-2025.zip"
  unzip(zip.file, exdir = "Sources of data")
} ##This code is automatically only run the first time the programme is run. It looks for whether the light rail/tram dataset has been unzipped, and if it hasn't, unzips it. 
TramJourneys = read_ods("Sources of data/light-rail-and-tram-statistics-year-ending-march-2025/lrt0101.ods", sheet = 3, as_tibble = FALSE) ##Creates a dataframe from the DfT tram journey data, lrt0101
TramJourneys <- TramJourneys[7:50, ] ##Gets rid of the first 6 rows of data which we don't want
colnames(TramJourneys) <- TramJourneys[1, ] ##Renames the column names to the first row of the data, which is the title row
TramJourneys <- TramJourneys[2:43, ] ##Deletes the title row, as this info is now stored in the column names as it should be
rownames(TramJourneys) <- 1:nrow(TramJourneys) ##Resets the row names
TramJourneys <- TramJourneys %>% 
  select(`Year ending March`|`Nottingham Express Transit`|`West Midlands Metro [note 1][note 2][note 4]`|`Sheffield Supertram`|`Tyne and Wear Metro`|`Manchester Metrolink [note 3]`|`Blackpool Tramway`) %>% ##Keeps only the columns of data we're interested in, i.e. gets rid of London, Scottish and summary data
  filter(`Year ending March` > 2009 & `Year ending March` < 2026) %>% ##Keeps only the columns of data we're interested in, i.e. gets rid of London, Scottish and summary data
  pivot_longer(
    cols = c("Nottingham Express Transit", "West Midlands Metro [note 1][note 2][note 4]", "Sheffield Supertram", "Tyne and Wear Metro", "Manchester Metrolink [note 3]", "Blackpool Tramway"),
    names_to = "LA or Region",
    values_to = "Journeys on light rail/trams per year"
  ) %>% ##Rearranges the data into the correct panel data format
  arrange(`LA or Region`) %>% ##Sorts the data alphabetically by name of local/combined authority
  mutate(`Journeys on light rail/trams per year` = as.numeric(`Journeys on light rail/trams per year`)) ##Transforms the tram journey data from characters into numbers
TramJourneys <- TramJourneys[ , c(2, 1, 3)] ##Reorders data into desired order
TramJourneys[TramJourneys=="Manchester Metrolink [note 3]"]<-"Greater Manchester CA"
TramJourneys[TramJourneys=="Nottingham Express Transit"]<-"Nottingham"
TramJourneys[TramJourneys=="Sheffield Supertram"]<-"South Yorkshire CA"
TramJourneys[TramJourneys=="Tyne and Wear Metro"]<-"Tyne and Wear CA"
TramJourneys[TramJourneys=="West Midlands Metro [note 1][note 2][note 4]"]<-"West Midlands CA" ##Aligns the location of the public transport with the local/combined authority names

#=> Blackpool tram is split across Blackpool LA and Lancashire county with (ROUGHLY!) 75% of journeys in Blackpool LA and 25% in Lancashire county. So, we need to split it accordingly. 
TramJourneys_Blackpool <- TramJourneys %>%
  filter(`LA or Region`== "Blackpool Tramway") ##Filters the TramJourneys dataframe for only the Blackpool Tramway information
TramJourneys_Blackpool[ , c(3)] <- 0.75*TramJourneys_Blackpool[ , c(3)] ##Multiplies the tram journey value by 0.75 as an approximation to the number of tram journeys taken place on the Blackpool Tramway in Blackpool LA
TramJourneys_Blackpool[TramJourneys_Blackpool=="Blackpool Tramway"]<-"Blackpool" ##Aligns the location of the public transport with Blackpool LA
TramJourneys_Lancashire <- TramJourneys %>%
  filter(`LA or Region`== "Blackpool Tramway") ##Filters the TramJourneys dataframe for only the Blackpool Tramway information
TramJourneys_Lancashire[ , c(3)] <- 0.25*TramJourneys_Lancashire[ , c(3)] ##Multiplies the tram journey value by 0.25 as an approximation to the number of tram journeys taken place on the Blackpool Tramway in Lancashire County
TramJourneys_Lancashire[TramJourneys_Lancashire=="Blackpool Tramway"]<-"Lancashire" ##Aligns the location of the public transport with Blackpool LA
#=> Now I want to split up the TramJourneys dataframe into separate dataframes for each area
TramJourneys_Greater_Manchester <- TramJourneys %>%
  filter(`LA or Region`== "Greater Manchester CA")
TramJourneys_Nottingham <- TramJourneys %>%
  filter(`LA or Region`== "Nottingham")
TramJourneys_South_Yorkshire <- TramJourneys %>%
  filter(`LA or Region`== "South Yorkshire CA")
TramJourneys_TyneWear <- TramJourneys %>%
  filter(`LA or Region`== "Tyne and Wear CA")
TramJourneys_West_Midlands <- TramJourneys %>%
  filter(`LA or Region`== "West Midlands CA") ##Creates 5 more dataframes split up as required

#=> I now want to prepare each dataframe for merging with the master dataframe. I start with Blackpool LA then move onto the rest of the regions
BusJourneys_PrivateCars_Blackpool <- BusJourneys_PrivateCars %>%
  filter(`LA.or.Region` == "Blackpool") ##Creates a dataframe from the master dataframe, but only for Blackpool LA's entries
BusJourneys_PrivateCars_TramJourneys_Blackpool <- data.frame(BusJourneys_PrivateCars_Blackpool, TramJourneys_Blackpool) ##Now we have a dataframe for Blackpool LA with all the information so far
BusJourneys_PrivateCars_Greater_Manchester <- BusJourneys_PrivateCars %>%
  filter(`LA.or.Region` == "Greater Manchester CA") ##Creates a dataframe from the master dataframe, but only for GM CA's entries
BusJourneys_PrivateCars_TramJourneys_Greater_Manchester <- data.frame(BusJourneys_PrivateCars_Greater_Manchester, TramJourneys_Greater_Manchester) ##Now we have a dataframe for Greater Manchester CA with all the information so far
BusJourneys_PrivateCars_Lancashire <- BusJourneys_PrivateCars %>%
  filter(`LA.or.Region` == "Lancashire") ##Creates a dataframe from the master dataframe, but only for Lancashire county's entries
BusJourneys_PrivateCars_TramJourneys_Lancashire <- data.frame(BusJourneys_PrivateCars_Lancashire, TramJourneys_Lancashire) ##Now we have a dataframe for Lancashire county with all the information so far
BusJourneys_PrivateCars_Nottingham <- BusJourneys_PrivateCars %>%
  filter(`LA.or.Region` == "Nottingham") ##Creates a dataframe from the master dataframe, but only for Nottingham's entries
BusJourneys_PrivateCars_TramJourneys_Nottingham <- data.frame(BusJourneys_PrivateCars_Nottingham, TramJourneys_Nottingham) ##Now we have a dataframe for Nottingham with all the information so far
BusJourneys_PrivateCars_South_Yorkshire <- BusJourneys_PrivateCars %>%
  filter(`LA.or.Region` == "South Yorkshire CA") ##Creates a dataframe from the master dataframe, but only for South Yorkshire CA's entries
BusJourneys_PrivateCars_TramJourneys_South_Yorkshire <- data.frame(BusJourneys_PrivateCars_South_Yorkshire, TramJourneys_South_Yorkshire) ##Now we have a dataframe for South Yorkshire CA with all the information so far
BusJourneys_PrivateCars_TyneWear <- BusJourneys_PrivateCars %>%
  filter(`LA.or.Region` == "Tyne and Wear CA") ##Creates a dataframe from the master dataframe, but only for Tyne and Wear CA's entries
BusJourneys_PrivateCars_TramJourneys_TyneWear <- data.frame(BusJourneys_PrivateCars_TyneWear, TramJourneys_TyneWear) ##Now we have a dataframe for Tyne and Wear CA with all the information so far
BusJourneys_PrivateCars_West_Midlands <- BusJourneys_PrivateCars %>%
  filter(`LA.or.Region` == "West Midlands CA") ##Creates a dataframe from the master dataframe, but only for West Midlands CA's entries
BusJourneys_PrivateCars_TramJourneys_West_Midlands <- data.frame(BusJourneys_PrivateCars_West_Midlands, TramJourneys_West_Midlands) ##Now we have a dataframe for Tyne and Wear CA with all the information so far

#=> I now want to add columns to the BusJourneys_PrivateCars 'master' dataframe to align it with these new broken up dataframes, so they can be added in seamlessly. 
BusJourneys_PrivateCars_TramJourneys <- BusJourneys_PrivateCars %>%
  mutate(LA.or.Region1 = 0) %>% ##Creates a new 'master' dataframe with a column of zeros named "LA.or.Region1" to align with the broken up tram dataframes
  mutate(Year.ending.March = "0") %>%
  mutate(Journeys.on.light.rail.trams.per.year = 0) %>% ##Does the same for the other two important columns
  mutate(`Journeys.on.light.rail.trams.per.year` = as.numeric(`Journeys.on.light.rail.trams.per.year`)) ##Makes sure the zero column for the tram journey data is numerical and not strings

#=> Now to delete the entries in the master dataframe which are about to be overwritten by the new data, and add the new data in
BusJourneys_PrivateCars_TramJourneys <- BusJourneys_PrivateCars_TramJourneys %>%
  filter(`LA.or.Region` != "Blackpool" & `LA.or.Region` != "Greater Manchester CA" & `LA.or.Region` != "Lancashire" & `LA.or.Region` != "Nottingham" & `LA.or.Region` != "South Yorkshire CA" & `LA.or.Region` != "Tyne and Wear CA" & `LA.or.Region` != "West Midlands CA") ##Filters out the regions we don't want in the dataframe as we're going to replace them
BusJourneys_PrivateCars_TramJourneys_Blackpool_list <- list(BusJourneys_PrivateCars_TramJourneys, BusJourneys_PrivateCars_TramJourneys_Blackpool) ##Creates a list of the main dataframe and the dataframe containing all information for Blackpool
BusJourneys_PrivateCars_TramJourneys <- BusJourneys_PrivateCars_TramJourneys_Blackpool_list %>% reduce(full_join) ##Combines the list into a new dataframe, so the Blackpool information is included
BusJourneys_PrivateCars_TramJourneys_Greater_Manchester_list <- list(BusJourneys_PrivateCars_TramJourneys, BusJourneys_PrivateCars_TramJourneys_Greater_Manchester) 
BusJourneys_PrivateCars_TramJourneys <- BusJourneys_PrivateCars_TramJourneys_Greater_Manchester_list %>% reduce(full_join)
BusJourneys_PrivateCars_TramJourneys_Lancashire_list <- list(BusJourneys_PrivateCars_TramJourneys, BusJourneys_PrivateCars_TramJourneys_Lancashire) 
BusJourneys_PrivateCars_TramJourneys <- BusJourneys_PrivateCars_TramJourneys_Lancashire_list %>% reduce(full_join)
BusJourneys_PrivateCars_TramJourneys_Nottingham_list <- list(BusJourneys_PrivateCars_TramJourneys, BusJourneys_PrivateCars_TramJourneys_Nottingham) 
BusJourneys_PrivateCars_TramJourneys <- BusJourneys_PrivateCars_TramJourneys_Nottingham_list %>% reduce(full_join)
BusJourneys_PrivateCars_TramJourneys_South_Yorkshire_list <- list(BusJourneys_PrivateCars_TramJourneys, BusJourneys_PrivateCars_TramJourneys_South_Yorkshire) 
BusJourneys_PrivateCars_TramJourneys <- BusJourneys_PrivateCars_TramJourneys_South_Yorkshire_list %>% reduce(full_join)
BusJourneys_PrivateCars_TramJourneys_TyneWear_list <- list(BusJourneys_PrivateCars_TramJourneys, BusJourneys_PrivateCars_TramJourneys_TyneWear) 
BusJourneys_PrivateCars_TramJourneys <- BusJourneys_PrivateCars_TramJourneys_TyneWear_list %>% reduce(full_join)
BusJourneys_PrivateCars_TramJourneys_West_Midlands_list <- list(BusJourneys_PrivateCars_TramJourneys, BusJourneys_PrivateCars_TramJourneys_West_Midlands) 
BusJourneys_PrivateCars_TramJourneys <- BusJourneys_PrivateCars_TramJourneys_West_Midlands_list %>% reduce(full_join) ##Does this process for each of the 7 regions with tram/light rail, so we now have the master list all in one. 

BusJourneys_PrivateCars_TramJourneys <- BusJourneys_PrivateCars_TramJourneys %>% 
  select(-(LA.or.Region1|LA.or.Region.1|Year.ending.March)) %>% ##Deletes the columns we don't want/need
  arrange(`LA.or.Region`) ##Sorts the data alphabetically by name of local/combined authority
rownames(BusJourneys_PrivateCars_TramJourneys) <- 1:nrow(BusJourneys_PrivateCars_TramJourneys) ##Resets the row names
#View(BusJourneys_PrivateCars_TramJourneys)