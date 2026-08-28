##install.packages("readODS")
##install.packages("tidyverse")
##install.packages("skimr")
##install.packages("dplyr")
##install.packages("stringr")
##install.packages("readxl")

pacman::p_load(
  tidyverse,
  skimr,
  readODS,
  dplyr,
  stringr,
  readxl
)

##**WRANGLING THE BUS JOURNEY DATA**
BusJourneys = read_ods("Sources of data/bus01.ods", sheet = 9, as_tibble = FALSE) ##Creates a dataframe from the DfT bus journey data, bus01f
BusJourneys <- BusJourneys[8:110, ] ##Deletes the first 7 rows from the table so we just have the data we want
colnames(BusJourneys) <- BusJourneys[1, ] ##Renames the column names to the first row of the data, which is the title row
BusJourneys <- BusJourneys[2:102, ] ##Deletes the title row, as this info is now stored in the column names as it should be
rownames(BusJourneys) <- 1:nrow(BusJourneys) ##Resets the row names
BusJourneys <- BusJourneys[-c(1, 10, 21, 30, 42, 51, 63, 64, 84, 101), ] ##Deletes the rows with the county-level summaries, i.e. deletes Yorkshire and the Humber, West Midlands, London, and just keeps the local/combined authority-level data
rownames(BusJourneys) <- 1:nrow(BusJourneys) ##Resets the row names
BusJourneys <- BusJourneys[ , 1:18] ##Deletes the notes column which isn't needed
BusJourneys <- BusJourneys %>%
  pivot_longer(
    cols = c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025"),
    names_to = "Year",
    values_to = "Bus Journeys Per Capita Per Year"
  ) ##Rearranges the data into the correct panel data format, ready for combination with the control variable data
BusJourneys <- BusJourneys[order(BusJourneys$`LA or Region`), ] ##Sorts the data alphabetically by name of local/combined authority
BusJourneys$`Bus Journeys Per Capita Per Year` <- as.numeric(BusJourneys$`Bus Journeys Per Capita Per Year`) ##Transforms the bus journey data from characters into numbers
#View(BusJourneys) ##Lets us view the BusJourneys dataframe in an easy-to-read spreadsheet format

##**WRANGLING THE CAR OWNERSHIP DATA**
PrivateCars = read_ods("Sources of data/veh0105.ods", sheet = 4, as_tibble = FALSE) ##Creates a dataframe by reading the fourth tab of the VEH0105 sheet
PrivateCars <- PrivateCars[4:40721, ] ##Deletes the first 3 rows from the table as these aren't needed
colnames(PrivateCars) <- PrivateCars[1, ] ##Renames the column names to the first row of the data, which is the title row
PrivateCars <- PrivateCars[2:40718, ] ##Gets rid of the old title row as this is no longer needed
rownames(PrivateCars) <- 1:nrow(PrivateCars) ##Resets the row names
PrivateCars <- PrivateCars %>%
  dplyr::filter(BodyType == "Cars" & Keepership == "Private" & Fuel == "Total") ##Filters for the information in the spreadsheet that we actually want
PrivateCarsE06 <- PrivateCars %>%
  filter(str_detect(`ONS Code`, "^E06"))
PrivateCarsE10 <- PrivateCars %>%
  filter(str_detect(`ONS Code`, "^E10"))
PrivateCarsE11 <- PrivateCars %>%
  filter(str_detect(`ONS Code`, "^E11")) ##Creates three more dataframes, filtering for regions with ONS Code starting E06 (cities), E10 (counties) and E11 (combined authorities) respectively
PrivateCars_list <- list(PrivateCarsE06, PrivateCarsE10, PrivateCarsE11) ##Creates a list of the three created dataframes
PrivateCars <- PrivateCars_list %>% reduce(full_join) ##Combines the list of the three created dataframes and then overwrites this onto the PrivateCars dataframe
PrivateCars <- PrivateCars[ ,c(6, 7, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60, 64, 68, 72)] ##Selects for only the columns we want in the dataset (we only want the Q1s in the dataset. The bus data is 'year ending March 20xx' so we take Q1 values to match dates)
#for (i in 2010:2025){
#  PrivateCars <- PrivateCars %>% rename(`i` = `i Q1`)
#}
PrivateCars <- PrivateCars %>% rename(`2025` = `2025 Q1`)
PrivateCars <- PrivateCars %>% rename(`2024` = `2024 Q1`)
PrivateCars <- PrivateCars %>% rename(`2023` = `2023 Q1`)
PrivateCars <- PrivateCars %>% rename(`2022` = `2022 Q1`)
PrivateCars <- PrivateCars %>% rename(`2021` = `2021 Q1`)
PrivateCars <- PrivateCars %>% rename(`2020` = `2020 Q1`)
PrivateCars <- PrivateCars %>% rename(`2019` = `2019 Q1`)
PrivateCars <- PrivateCars %>% rename(`2018` = `2018 Q1`)
PrivateCars <- PrivateCars %>% rename(`2017` = `2017 Q1`)
PrivateCars <- PrivateCars %>% rename(`2016` = `2016 Q1`)
PrivateCars <- PrivateCars %>% rename(`2015` = `2015 Q1`)
PrivateCars <- PrivateCars %>% rename(`2014` = `2014 Q1`)
PrivateCars <- PrivateCars %>% rename(`2013` = `2013 Q1`)
PrivateCars <- PrivateCars %>% rename(`2012` = `2012 Q1`)
PrivateCars <- PrivateCars %>% rename(`2011` = `2011 Q1`)
PrivateCars <- PrivateCars %>% rename(`2010` = `2010 Q1`) ##Renames the columns to get rid of the Q1s from the titles
PrivateCars <- PrivateCars %>%
  pivot_longer(
    cols = c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025"),
    names_to = "Year",
    values_to = "Private cars licensed in each region in each year"
  ) ##Rearranges the data into the correct panel data format, ready for combination with the control variable data
PrivateCars <- PrivateCars[order(PrivateCars$`ONS Geography`), ] ##Sorts the data alphabetically by name of local/combined authority
PrivateCars$`Private cars licensed in each region in each year` <- as.numeric(PrivateCars$`Private cars licensed in each region in each year`) ##Transforms the car ownership data from characters into numbers
#View(PrivateCars) ## Lets us view the tidied private cars dataframe

##**COMBINING AND MATCHING BUS JOURNEY AND CAR OWNERSHIP DATA**
PrivateCars <- PrivateCars[order(PrivateCars$`ONS Code`), ] ##Sorts the private car data alphabetically by ONS Code
BusJourneys <- BusJourneys[order(BusJourneys$`Local Authority (LA) Code`), ] ##Sorts the bus journey data alphabetically by OLS code
BusJourneys <- BusJourneys %>%
  dplyr::filter(`LA or Region` != "Bournemouth" & `LA or Region` != "Poole") ##Filters out Bournemouth and Poole - there are no data for these entries and they are already accounted for in the 'Bournemouth, Chirstchuch and Poole' entry
##=>For some reason, the bus journeys dataset doesn't include any entries for the Isle of Scilly. So, I am going to add them to the dataset for completeness' sake. 
ONS_Code_Scilly <- c("E06000053", "E06000053", "E06000053", "E06000053", "E06000053", "E06000053", "E06000053", "E06000053", "E06000053", "E06000053", "E06000053", "E06000053", "E06000053", "E06000053", "E06000053", "E06000053")
LA_or_Region_Scilly <- c("Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly")
Year_Scilly <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025")
Bus_Patronage_Scilly <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) ##Creates four vectors each containing the elements needed for an Isle of Scilly dataframe to be added to the bus journeys data. 
BusJourneys_Scilly <- data.frame(ONS_Code_Scilly, LA_or_Region_Scilly, Year_Scilly, Bus_Patronage_Scilly) ##Combines these four vectors into a dataframe
colnames(BusJourneys_Scilly) <- c("Local Authority (LA) Code", "LA or Region", "Year", "Bus Journeys Per Capita Per Year") ##Renames the columns to match the BusJourneys dataframe
BusJourneys_list_Scilly <- list(BusJourneys, BusJourneys_Scilly) ##Creates a list of the overall bus journey dataframe and the Scilly bus journey dataframe
BusJourneys <- BusJourneys_list_Scilly %>% reduce(full_join) ##Combines the list into a completed dataframe so that the Isle of Scilly data is included into the BusJourneys data 
#--
#=> The bus journeys data includes the overarching Cumbria county data, while the car ownership data includes the separate combined authorities Cumberland, and Westmorland and Furness. So, in the private car ownership data we want to combine these two local authorities into one datapoint. 
ONS_Code_Cumbria <- c("E10000006", "E10000006", "E10000006", "E10000006", "E10000006", "E10000006", "E10000006", "E10000006", "E10000006", "E10000006", "E10000006", "E10000006", "E10000006", "E10000006", "E10000006", "E10000006")
LA_or_Region_Cumbria <- c("Cumbria", "Cumbria", "Cumbria", "Cumbria", "Cumbria", "Cumbria", "Cumbria", "Cumbria", "Cumbria", "Cumbria", "Cumbria", "Cumbria", "Cumbria", "Cumbria", "Cumbria", "Cumbria")
Year_Cumbria <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025") ##We first set up the first three rows of the Cumbria dataframe
PrivateCars_Cumberland <- PrivateCars %>%
  dplyr::filter(`ONS Geography`== "Cumberland") ##Filters the private cars dataframe for only the Cumberland information
Private_Car_Ownership_Cumberland = c(PrivateCars_Cumberland[ ,4]) ##Creates a vector which contains the private car ownership for Cumberland, by filtering for only the 4th column in the above 
Private_Car_Ownership_Cumberland <- unlist(Private_Car_Ownership_Cumberland, use.names = FALSE) ##Converts the list into a straight vector so we can perform numerical operations with it
PrivateCars_Westmorland <- PrivateCars %>%
  dplyr::filter(`ONS Geography`== "Westmorland and Furness") 
Private_Car_Ownership_Westmorland = c(PrivateCars_Westmorland[ ,4]) 
Private_Car_Ownership_Westmorland <- unlist(Private_Car_Ownership_Westmorland, use.names = FALSE) ##Does the same process for Westmorland
Private_Car_Ownership_Cumbria = Private_Car_Ownership_Cumberland + Private_Car_Ownership_Westmorland ##Adds the private car ownership numbers together for Cumberland and Westmorland 
PrivateCars_Cumbria <- data.frame(ONS_Code_Cumbria, LA_or_Region_Cumbria, Year_Cumbria, Private_Car_Ownership_Cumbria) ##Combines these four vectors into a dataframe
colnames(PrivateCars_Cumbria) <- c("ONS Code", "ONS Geography", "Year", "Private cars licensed in each region in each year") ##Renames the columns to match the PrivateCars dataframe
PrivateCars_list_Cumbria <- list(PrivateCars, PrivateCars_Cumbria) ##Creates a list of the overall private cars dataframe and the Cumbria private cars dataframe
PrivateCars <- PrivateCars_list_Cumbria %>% reduce(full_join) ##Combines the list into a completed dataframe so that the Cumbria data is included into the PrivateCars data 
PrivateCars <- PrivateCars %>%
  dplyr::filter(`ONS Geography` != "Cumberland" & `ONS Geography` != "Westmorland and Furness") ##Filters out Cumberland & Westmorland and Furness - these have now been replaced by Cumbria.
#- 

#=>Both datasets treat Northamptonshire funnily. The bus journeys dataset contains values for Northamptonshire up until 2021, and then from 2022-2025, it is split into North Northamptonshire and West Northamptonshire. The PrivateCars data, on the other hand, reports North and West Northamptonshire differently the entire time. We are going to combine into Northamptonshire for both datasets, as this is probably the most consistent way to deal with the problem. First, I will combine into Northamptonshire in the PrivateCars dataset. 
PC_ONS_Code_Northamptonshire <- c("E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021")
PC_ONS_Geography_Northamptonshire <- c("Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire")
PC_Year_Northamptonshire <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025") ##We first set up the first three rows of the PrivateCars Northamptonshire dataframe
PrivateCars_North_Northamptonshire <- PrivateCars %>%
  dplyr::filter(`ONS Geography`== "North Northamptonshire") ##Filters the private cars dataframe for only the North Northamptonshire information
Private_Car_Ownership_North_Northamptonshire = c(PrivateCars_North_Northamptonshire[ ,4]) ##Creates a vector which contains the private car ownership for North Northamptonshire, by filtering for only the 4th column in the above 
Private_Car_Ownership_North_Northamptonshire <- unlist(Private_Car_Ownership_North_Northamptonshire, use.names = FALSE) ##Converts the list into a straight vector so we can perform numerical operations with it
PrivateCars_West_Northamptonshire <- PrivateCars %>%
  dplyr::filter(`ONS Geography`== "West Northamptonshire") ##Filters the private cars dataframe for only the West Northamptonshire information
Private_Car_Ownership_West_Northamptonshire = c(PrivateCars_West_Northamptonshire[ ,4]) ##Creates a vector which contains the private car ownership for West Northamptonshire, by filtering for only the 4th column in the above 
Private_Car_Ownership_West_Northamptonshire <- unlist(Private_Car_Ownership_West_Northamptonshire, use.names = FALSE) ##Converts the list into a straight vector so we can perform numerical operations with it
Private_Car_Ownership_Northamptonshire = Private_Car_Ownership_North_Northamptonshire + Private_Car_Ownership_West_Northamptonshire ##Adds the private car ownership numbers together for North Northamptonshire and West Northamptonshire
PrivateCars_Northamptonshire <- data.frame(PC_ONS_Code_Northamptonshire, PC_ONS_Geography_Northamptonshire, PC_Year_Northamptonshire, Private_Car_Ownership_Northamptonshire) ##Combines these four vectors into a dataframe
colnames(PrivateCars_Northamptonshire) <- c("ONS Code", "ONS Geography", "Year", "Private cars licensed in each region in each year") ##Renames the columns to match the PrivateCars dataframe
PrivateCars_list_Northamptonshire <- list(PrivateCars, PrivateCars_Northamptonshire) ##Creates a list of the overall private cars dataframe and the Northamptonshire private cars dataframe
PrivateCars <- PrivateCars_list_Northamptonshire %>% reduce(full_join) ##Combines the list into a completed dataframe so that the Northamptonshire data is included into the PrivateCars data 
PrivateCars <- PrivateCars %>%
  dplyr::filter(`ONS Geography` != "North Northamptonshire" & `ONS Geography` != "West Northamptonshire") ##Filters out North Northamptonshire and West Northamptonshire - these have now been replaced by Northamptonshire.

#=> Now to combine West Northamptonshire and North Northamptonshire together for the bus journeys data. This will be slightly more complicated, as only the last four years of data are split into North Northamptonshire and West Northamptonshire. I want to combine these into a 'New' Northamptonshire datapoint, then combine the two. 
BusJourneys_OldNorthamptonshire <- BusJourneys %>%
  dplyr::filter(`LA or Region` == "Northamptonshire") ##Creating a dataframe with all the 'old' Northamptonshire datapoints, which we will later combine with the 'new' Northamptonshire datapoints.
BJ_LA_Code_Northamptonshire <- c("E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021")
BJ_LA_or_Region_Northamptonshire <- c("Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire")
BJ_Year_Northamptonshire <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025") ##We first set up the first three rows of the BusJourneys 'new'/combined Northamptonshire dataframe
BusJourneys_North_Northamptonshire <- BusJourneys %>%
  dplyr::filter(`LA or Region`== "North Northamptonshire") ##Filters the BusJourneys dataframe for only the North Northamptonshire information
Bus_Patronage_North_Northamptonshire = c(BusJourneys_North_Northamptonshire[ ,4]) ##Creates a vector which contains the bus journey information for North Northamptonshire, by filtering for only the 4th column in the above 
Bus_Patronage_North_Northamptonshire <- unlist(Bus_Patronage_North_Northamptonshire, use.names = FALSE) ##Converts the list into a straight vector so we can perform numerical operations with it
BusJourneys_West_Northamptonshire <- BusJourneys %>%
  dplyr::filter(`LA or Region`== "West Northamptonshire") ##Filters the BusJourneys dataframe for only the West Northamptonshire information
Bus_Patronage_West_Northamptonshire = c(BusJourneys_West_Northamptonshire[ ,4]) ##Creates a vector which contains the bus journey information for West Northamptonshire, by filtering for only the 4th column in the above 
Bus_Patronage_West_Northamptonshire <- unlist(Bus_Patronage_West_Northamptonshire, use.names = FALSE) ##Converts the list into a straight vector so we can perform numerical operations with it
Bus_Patronage_Northamptonshire = Bus_Patronage_North_Northamptonshire + Bus_Patronage_West_Northamptonshire ##Adds the bus journey numbers together for North Northamptonshire and West Northamptonshire
BusJourneys_NewNorthamptonshire <- data.frame(BJ_LA_Code_Northamptonshire, BJ_LA_or_Region_Northamptonshire, BJ_Year_Northamptonshire, Bus_Patronage_Northamptonshire) ##Combines these four vectors into a dataframe, to create the 'new'/combined Northamptonshire dataframe 
colnames(BusJourneys_NewNorthamptonshire) <- c("Local Authority (LA) Code", "LA or Region", "Year", "Bus Journeys Per Capita Per Year") ##Renames the columns to match the BusJourneys dataframe
BusJourneys_OldNorthamptonshire <- BusJourneys_OldNorthamptonshire[1:12, ] ##Filters for the part of the 'old' Northamptonshire dataframe which actually has values in it
BusJourneys_NewNorthamptonshire <- BusJourneys_NewNorthamptonshire[13:16, ] ##Filters for the part of the 'new'/combined Northamptonshire dataframe which actually has values in it
BusJourneys_list_TotalNorthamptonshire <- list(BusJourneys_OldNorthamptonshire, BusJourneys_NewNorthamptonshire) ##Creates a list of the 'old' Northamptonshire and 'new'/combined Northamptonshire dataframes
BusJourneys_TotalNorthamptonshire <- BusJourneys_list_TotalNorthamptonshire %>% reduce(full_join) ##Combines the list into dataframe so that we now have a dataframe with all the bus journey Northamptonshire information in it 
BusJourneys <- BusJourneys %>%
  dplyr::filter(`LA or Region` != "North Northamptonshire" & `LA or Region` != "West Northamptonshire" & `LA or Region` != "Northamptonshire") ##Filters North Northamptonshire, West Northamptonshire and Northamptonshire data out of the Bus Journeys data, prior to the new combined Northamptonshire data being added back in.
BusJourneys_list_Northamptonshire <- list(BusJourneys, BusJourneys_TotalNorthamptonshire) ##Creates a list of the overall bus journey data and the new totally combined Northamptonshire data
BusJourneys <- BusJourneys_list_Northamptonshire %>% reduce(full_join) ##Combines the list into one dataframe so that now all the Northamptonshire data is combined with the oritinal data

PrivateCars[PrivateCars=="County Durham"]<-"Durham" ##Renames each entry in the private cars dataframe containing 'County Durham' to 'Durham' to align it with the bus journeys data

##=> This bit combines the two dataframes to unify the data
BusJourneys <- BusJourneys[order(BusJourneys$`LA or Region`), ] ##Sorts the bus journey data alphabetically by region name
PrivateCars <- PrivateCars[order(PrivateCars$`ONS Geography`), ] ##Sorts the private car data alphabetically by region name
BusJourneys <- BusJourneys[1:1408, ] ##Resizes the bus journey dataframe to be the size we want (for now)
PrivateCars <- PrivateCars[1:1408, ] ##Resizes the car ownership dataframe to be the same size as the bus journey dataframe
BusJourneys_PrivateCars <- data.frame(BusJourneys, PrivateCars) ##Creates a new dataframe which is just the two main dataframes stitched together horizontally
BusJourneys_PrivateCars <- BusJourneys_PrivateCars[ , -c(5:7)] ##Removes redundant/repeated columns from this dataframe
#View(BusJourneys_PrivateCars) ##Lets us view this new stitched together dataframe

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
TramJourneys <- TramJourneys[ , c(1, 4:9)] ##Keeps only the columns of data we're interested in, i.e. gets rid of London, Scottish and summary data
TramJourneys <- TramJourneys[27:42, ] ##Keeps only the columns of data we're interested in, i.e. gets rid of London, Scottish and summary data
TramJourneys <- TramJourneys %>%
  pivot_longer(
    cols = c("Nottingham Express Transit", "West Midlands Metro [note 1][note 2][note 4]", "Sheffield Supertram", "Tyne and Wear Metro", "Manchester Metrolink [note 3]", "Blackpool Tramway"),
    names_to = "LA or Region",
    values_to = "Journeys on light rail/trams per year"
) ##Rearranges the data into the correct panel data format
TramJourneys <- TramJourneys[order(TramJourneys$`LA or Region`), ] ##Sorts the data alphabetically by name of local/combined authority
TramJourneys <- TramJourneys[ , c(2, 1, 3)] ##Reorders data into desired order
TramJourneys[TramJourneys=="Manchester Metrolink [note 3]"]<-"Greater Manchester CA"
TramJourneys[TramJourneys=="Nottingham Express Transit"]<-"Nottingham"
TramJourneys[TramJourneys=="Sheffield Supertram"]<-"South Yorkshire CA"
TramJourneys[TramJourneys=="Tyne and Wear Metro"]<-"Tyne and Wear CA"
TramJourneys[TramJourneys=="West Midlands Metro [note 1][note 2][note 4]"]<-"West Midlands CA" ##Aligns the location of the public transport with the local/combined authority names
TramJourneys$`Journeys on light rail/trams per year` <- as.numeric(TramJourneys$`Journeys on light rail/trams per year`) ##Transforms the tram journey data from characters into numbers

#=> Blackpool tram is split across Blackpool LA and Lancashire county with (ROUGHLY!) 75% of journeys in Blackpool LA and 25% in Lancashire county. So, we need to split it accordingly. 
TramJourneys_Blackpool <- TramJourneys %>%
  dplyr::filter(`LA or Region`== "Blackpool Tramway") ##Filters the TramJourneys dataframe for only the Blackpool Tramway information
TramJourneys_Blackpool[ , c(3)] <- 0.75*TramJourneys_Blackpool[ , c(3)] ##Multiplies the tram journey value by 0.75 as an approximation to the number of tram journeys taken place on the Blackpool Tramway in Blackpool LA
TramJourneys_Blackpool[TramJourneys_Blackpool=="Blackpool Tramway"]<-"Blackpool" ##Aligns the location of the public transport with Blackpool LA
TramJourneys_Lancashire <- TramJourneys %>%
  dplyr::filter(`LA or Region`== "Blackpool Tramway") ##Filters the TramJourneys dataframe for only the Blackpool Tramway information
TramJourneys_Lancashire[ , c(3)] <- 0.25*TramJourneys_Lancashire[ , c(3)] ##Multiplies the tram journey value by 0.25 as an approximation to the number of tram journeys taken place on the Blackpool Tramway in Lancashire County
TramJourneys_Lancashire[TramJourneys_Lancashire=="Blackpool Tramway"]<-"Lancashire" ##Aligns the location of the public transport with Blackpool LA
#=> Now I want to split up the TramJourneys dataframe into separate dataframes for each area
TramJourneys_Greater_Manchester <- TramJourneys %>%
  dplyr::filter(`LA or Region`== "Greater Manchester CA")
TramJourneys_Nottingham <- TramJourneys %>%
  dplyr::filter(`LA or Region`== "Nottingham")
TramJourneys_South_Yorkshire <- TramJourneys %>%
  dplyr::filter(`LA or Region`== "South Yorkshire CA")
TramJourneys_TyneWear <- TramJourneys %>%
  dplyr::filter(`LA or Region`== "Tyne and Wear CA")
TramJourneys_West_Midlands <- TramJourneys %>%
  dplyr::filter(`LA or Region`== "West Midlands CA") ##Creates 5 more dataframes split up as required

#=> I now want to prepare each dataframe for merging with the master dataframe. I start with Blackpool LA then move onto the rest of the regions
BusJourneys_PrivateCars_Blackpool <- BusJourneys_PrivateCars %>%
  dplyr::filter(`LA.or.Region` == "Blackpool") ##Creates a dataframe from the master dataframe, but only for Blackpool LA's entries
BusJourneys_PrivateCars_TramJourneys_Blackpool <- data.frame(BusJourneys_PrivateCars_Blackpool, TramJourneys_Blackpool) ##Now we have a dataframe for Blackpool LA with all the information so far
BusJourneys_PrivateCars_Greater_Manchester <- BusJourneys_PrivateCars %>%
  dplyr::filter(`LA.or.Region` == "Greater Manchester CA") ##Creates a dataframe from the master dataframe, but only for GM CA's entries
BusJourneys_PrivateCars_TramJourneys_Greater_Manchester <- data.frame(BusJourneys_PrivateCars_Greater_Manchester, TramJourneys_Greater_Manchester) ##Now we have a dataframe for Greater Manchester CA with all the information so far
BusJourneys_PrivateCars_Lancashire <- BusJourneys_PrivateCars %>%
  dplyr::filter(`LA.or.Region` == "Lancashire") ##Creates a dataframe from the master dataframe, but only for Lancashire county's entries
BusJourneys_PrivateCars_TramJourneys_Lancashire <- data.frame(BusJourneys_PrivateCars_Lancashire, TramJourneys_Lancashire) ##Now we have a dataframe for Lancashire county with all the information so far
BusJourneys_PrivateCars_Nottingham <- BusJourneys_PrivateCars %>%
  dplyr::filter(`LA.or.Region` == "Nottingham") ##Creates a dataframe from the master dataframe, but only for Nottingham's entries
BusJourneys_PrivateCars_TramJourneys_Nottingham <- data.frame(BusJourneys_PrivateCars_Nottingham, TramJourneys_Nottingham) ##Now we have a dataframe for Nottingham with all the information so far
BusJourneys_PrivateCars_South_Yorkshire <- BusJourneys_PrivateCars %>%
  dplyr::filter(`LA.or.Region` == "South Yorkshire CA") ##Creates a dataframe from the master dataframe, but only for South Yorkshire CA's entries
BusJourneys_PrivateCars_TramJourneys_South_Yorkshire <- data.frame(BusJourneys_PrivateCars_South_Yorkshire, TramJourneys_South_Yorkshire) ##Now we have a dataframe for South Yorkshire CA with all the information so far
BusJourneys_PrivateCars_TyneWear <- BusJourneys_PrivateCars %>%
  dplyr::filter(`LA.or.Region` == "Tyne and Wear CA") ##Creates a dataframe from the master dataframe, but only for Tyne and Wear CA's entries
BusJourneys_PrivateCars_TramJourneys_TyneWear <- data.frame(BusJourneys_PrivateCars_TyneWear, TramJourneys_TyneWear) ##Now we have a dataframe for Tyne and Wear CA with all the information so far
BusJourneys_PrivateCars_West_Midlands <- BusJourneys_PrivateCars %>%
  dplyr::filter(`LA.or.Region` == "West Midlands CA") ##Creates a dataframe from the master dataframe, but only for West Midlands CA's entries
BusJourneys_PrivateCars_TramJourneys_West_Midlands <- data.frame(BusJourneys_PrivateCars_West_Midlands, TramJourneys_West_Midlands) ##Now we have a dataframe for Tyne and Wear CA with all the information so far

#=> I now want to add columns to the BusJourneys_PrivateCars 'master' dataframe to align it with these new broken up dataframes, so they can be added in seamlessly. 
BusJourneys_PrivateCars_TramJourneys <- BusJourneys_PrivateCars %>%
  mutate(LA.or.Region1 = 0) ##Creates a new 'master' dataframe with a column of zeros named "LA.or.Region1" to align with the broken up tram dataframes
BusJourneys_PrivateCars_TramJourneys <- BusJourneys_PrivateCars_TramJourneys %>%
  mutate(Year.ending.March = "0")
BusJourneys_PrivateCars_TramJourneys <- BusJourneys_PrivateCars_TramJourneys %>%
  mutate(Journeys.on.light.rail.trams.per.year = 0) ##Does the same for the other two important columns
BusJourneys_PrivateCars_TramJourneys$`Journeys.on.light.rail.trams.per.year` <- as.numeric(BusJourneys_PrivateCars_TramJourneys$`Journeys.on.light.rail.trams.per.year`) ##Makes sure the zero column for the tram journey data is numerical and not strings

#=> Now to delete the entries in the master dataframe which are about to be overwritten by the new data, and add the new data in
BusJourneys_PrivateCars_TramJourneys <- BusJourneys_PrivateCars_TramJourneys %>%
  dplyr::filter(`LA.or.Region` != "Blackpool" & `LA.or.Region` != "Greater Manchester CA" & `LA.or.Region` != "Lancashire" & `LA.or.Region` != "Nottingham" & `LA.or.Region` != "South Yorkshire CA" & `LA.or.Region` != "Tyne and Wear CA" & `LA.or.Region` != "West Midlands CA") ##Filters out the regions we don't want in the dataframe as we're going to replace them
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

BusJourneys_PrivateCars_TramJourneys <- BusJourneys_PrivateCars_TramJourneys[ , c(1:5, 8)] ##Deletes the columns we don't want/need
BusJourneys_PrivateCars_TramJourneys <- BusJourneys_PrivateCars_TramJourneys[order(BusJourneys_PrivateCars_TramJourneys$`LA.or.Region`), ] ##Sorts the data alphabetically by name of local/combined authority
rownames(BusJourneys_PrivateCars_TramJourneys) <- 1:nrow(BusJourneys_PrivateCars_TramJourneys) ##Resets the row names
#View(BusJourneys_PrivateCars_TramJourneys)

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

##**CREATING THE TREATMENT INDICATOR**
Treatment <- as.vector(matrix(0, nrow=1408)) ##Creates a vector of zeros of length 1,408
for (i in 1:1408){
  if (BusJourneys_PrivateCars_TramJourneys_RealPay[i, c(2)]=="Greater Manchester CA" & (BusJourneys_PrivateCars_TramJourneys_RealPay[i, c(3)]=="2024" | BusJourneys_PrivateCars_TramJourneys_RealPay[i, c(3)]=="2025")){
    Treatment[i] <- 1
  }
} ##Sets the treatment indicator to be equal to 1 only for the treatment units (i.e. Greater Manchester after 2024)
BusJourneys_PrivateCars_TramJourneys_RealPay <- data.frame(BusJourneys_PrivateCars_TramJourneys_RealPay, Treatment) ##Stitches the treatment indicator vector onto the main dataset
View(BusJourneys_PrivateCars_TramJourneys_RealPay)

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
View(BusJourneys_PrivateCars_TramJourneys_RealPay)

##Next: Run the regressions
