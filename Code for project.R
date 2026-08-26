##install.packages("readODS")
##install.packages("tidyverse")
##install.packages("skimr")
##install.packages("dplyr")
##install.packages("stringr")

pacman::p_load(
  tidyverse,
  skimr,
  readODS,
  dplyr,
  stringr,
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

##COMBINING AND MATCHING BUS JOURNEY AND CAR OWNERSHIP DATA
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
##Now to combine West Northamptonshire and North Northamptonshire together for the bus journeys data. This will be slightly more complicated, as only the last four years of data are split into North Northamptonshire and West Northamptonshire. I want to combine these into a 'New' Northamptonshire datapoint, then combine the two. 
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
View(BusJourneys_PrivateCars) ##Lets us view this new stitched together dataframe

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

View(TramJourneys)

##NEXT NEED TO: CHECK THE ABOVE DATAFRAMES ARE RIGHT, ADD IN COLUMNS TO THE MASTER DATAFRAME AND SET THEM ALL TO ZERO FOR EVERY ENTRY, THEN REMOVE THE OLD ENTRIES AND ADD THE NEW ENTRIES IN VIA THE ABOVE DATAFRAMES