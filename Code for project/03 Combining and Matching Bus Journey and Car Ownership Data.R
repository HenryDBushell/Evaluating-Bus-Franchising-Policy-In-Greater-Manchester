source("Code for project/02 Wrangling the Car Ownership Data.R")

##**COMBINING AND MATCHING BUS JOURNEY AND CAR OWNERSHIP DATA**
PrivateCars <- PrivateCars[order(PrivateCars$`ONS Code`), ] ##Sorts the private car data alphabetically by ONS Code
BusJourneys <- BusJourneys[order(BusJourneys$`Local Authority (LA) Code`), ] ##Sorts the bus journey data alphabetically by OLS code
BusJourneys <- BusJourneys %>%
  filter(`LA or Region` != "Bournemouth" & `LA or Region` != "Poole") ##Filters out Bournemouth and Poole - there are no data for these entries and they are already accounted for in the 'Bournemouth, Chirstchuch and Poole' entry
##=>For some reason, the bus journeys dataset doesn't include any entries for the Isle of Scilly. So, I am going to add them to the dataset for completeness' sake. 
BusJourneys_Scilly <- data.frame(`Local Authority (LA) Code` = c("E06000053", "E06000053", "E06000053", "E06000053", "E06000053", "E06000053", "E06000053", "E06000053", "E06000053", "E06000053", "E06000053", "E06000053", "E06000053", "E06000053", "E06000053", "E06000053"), `LA or Region` = c("Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly", "Isles of Scilly"), Year = c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025"), `Bus Journeys Per Capita Per Year` = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)) ##Creates an Isle of Scilly dataframe to be added to the main Bus Journeys dataframe
colnames(BusJourneys_Scilly) <- c("Local Authority (LA) Code", "LA or Region", "Year", "Bus Journeys Per Capita Per Year") ##Renames the columns to match the BusJourneys dataframe
BusJourneys_list_Scilly <- list(BusJourneys, BusJourneys_Scilly) ##Creates a list of the overall bus journey dataframe and the Scilly bus journey dataframe
BusJourneys <- BusJourneys_list_Scilly %>% reduce(full_join) ##Combines the list into a completed dataframe so that the Isle of Scilly data is included into the BusJourneys data 
#--

#=> The bus journeys data includes the overarching Cumbria county data, while the car ownership data includes the separate combined authorities Cumberland, and Westmorland and Furness. So, in the private car ownership data we want to combine these two local authorities into one datapoint. 
Private_Car_Ownership_Cumberland <- PrivateCars %>%
  filter(`ONS Geography`== "Cumberland") %>% ##Filters the private cars dataframe for only the Cumberland information
  select(`Private cars licensed in each region in each year`) ##Selects only specifically the private car ownership for Cumberland
Private_Car_Ownership_Cumberland <- unlist(Private_Car_Ownership_Cumberland, use.names = FALSE) ##Converts the list into a straight vector so we can perform numerical operations with it
Private_Car_Ownership_Westmorland <- PrivateCars %>%
  filter(`ONS Geography`== "Westmorland and Furness") %>%
  select(`Private cars licensed in each region in each year`)
Private_Car_Ownership_Westmorland <- unlist(Private_Car_Ownership_Westmorland, use.names = FALSE) ##Does the same process for Westmorland
Private_Car_Ownership_Cumbria = Private_Car_Ownership_Cumberland + Private_Car_Ownership_Westmorland ##Adds the private car ownership numbers together for Cumberland and Westmorland 
PrivateCars_Cumbria <- data.frame(ONS_Code_Cumbria = c("E10000006", "E10000006", "E10000006", "E10000006", "E10000006", "E10000006", "E10000006", "E10000006", "E10000006", "E10000006", "E10000006", "E10000006", "E10000006", "E10000006", "E10000006", "E10000006"), LA_or_Region_Cumbria = c("Cumbria", "Cumbria", "Cumbria", "Cumbria", "Cumbria", "Cumbria", "Cumbria", "Cumbria", "Cumbria", "Cumbria", "Cumbria", "Cumbria", "Cumbria", "Cumbria", "Cumbria", "Cumbria"), Year_Cumbria = c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025"), Private_Car_Ownership_Cumbria) ##Combines these four vectors into a dataframe
colnames(PrivateCars_Cumbria) <- c("ONS Code", "ONS Geography", "Year", "Private cars licensed in each region in each year") ##Renames the columns to match the PrivateCars dataframe
PrivateCars_list_Cumbria <- list(PrivateCars, PrivateCars_Cumbria) ##Creates a list of the overall private cars dataframe and the Cumbria private cars dataframe
PrivateCars <- PrivateCars_list_Cumbria %>% reduce(full_join) ##Combines the list into a completed dataframe so that the Cumbria data is included into the PrivateCars data 
PrivateCars <- PrivateCars %>%
  filter(`ONS Geography` != "Cumberland" & `ONS Geography` != "Westmorland and Furness") ##Filters out Cumberland & Westmorland and Furness - these have now been replaced by Cumbria.
#- 

#=>Both datasets treat Northamptonshire funnily. The bus journeys dataset contains values for Northamptonshire up until 2021, and then from 2022-2025, it is split into North Northamptonshire and West Northamptonshire. The PrivateCars data, on the other hand, reports North and West Northamptonshire differently the entire time. We are going to combine into Northamptonshire for both datasets, as this is probably the most consistent way to deal with the problem. First, I will combine into Northamptonshire in the PrivateCars dataset. 
Private_Car_Ownership_North_Northamptonshire <- PrivateCars %>%
  filter(`ONS Geography`== "North Northamptonshire") %>% ##Filters the private cars dataframe for only the North Northamptonshire information
  select(`Private cars licensed in each region in each year`) ##Creates a vector which contains the private car ownership for North Northamptonshire, by filtering for only the relevant column
Private_Car_Ownership_North_Northamptonshire <- unlist(Private_Car_Ownership_North_Northamptonshire, use.names = FALSE) ##Converts the list into a straight vector so we can perform numerical operations with it
Private_Car_Ownership_West_Northamptonshire <- PrivateCars %>%
  filter(`ONS Geography`== "West Northamptonshire") %>% ##Filters the private cars dataframe for only the West Northamptonshire information
  select(`Private cars licensed in each region in each year`) ##Creates a vector which contains the private car ownership for West Northamptonshire, by filtering for only the relevant column
Private_Car_Ownership_West_Northamptonshire <- unlist(Private_Car_Ownership_West_Northamptonshire, use.names = FALSE) ##Converts the list into a straight vector so we can perform numerical operations with it
Private_Car_Ownership_Northamptonshire = Private_Car_Ownership_North_Northamptonshire + Private_Car_Ownership_West_Northamptonshire ##Adds the private car ownership numbers together for North Northamptonshire and West Northamptonshire
PrivateCars_Northamptonshire <- data.frame(PC_ONS_Code_Northamptonshire = c("E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021"), PC_ONS_Geography_Northamptonshire = c("Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire"), PC_Year_Northamptonshire = c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025"), Private_Car_Ownership_Northamptonshire) ##Creates a dataframe for Northamptonshire with the combined North/West Northamptonshire information
colnames(PrivateCars_Northamptonshire) <- c("ONS Code", "ONS Geography", "Year", "Private cars licensed in each region in each year") ##Renames the columns to match the PrivateCars dataframe
PrivateCars_list_Northamptonshire <- list(PrivateCars, PrivateCars_Northamptonshire) ##Creates a list of the overall private cars dataframe and the Northamptonshire private cars dataframe
PrivateCars <- PrivateCars_list_Northamptonshire %>% reduce(full_join) ##Combines the list into a completed dataframe so that the Northamptonshire data is included into the PrivateCars data 
PrivateCars <- PrivateCars %>%
  filter(`ONS Geography` != "North Northamptonshire" & `ONS Geography` != "West Northamptonshire") ##Filters out North Northamptonshire and West Northamptonshire - these have now been replaced by Northamptonshire.

#=> Now to combine West Northamptonshire and North Northamptonshire together for the bus journeys data. This will be slightly more complicated, as only the last four years of data are split into North Northamptonshire and West Northamptonshire. I want to combine these into a 'New' Northamptonshire datapoint, then combine the two. 
BusJourneys_OldNorthamptonshire <- BusJourneys %>%
  filter(`LA or Region` == "Northamptonshire") ##Creating a dataframe with all the 'old' Northamptonshire datapoints, which we will later combine with the 'new' Northamptonshire datapoints.
Bus_Patronage_North_Northamptonshire <- BusJourneys %>%
  filter(`LA or Region`== "North Northamptonshire") %>% ##Filters the BusJourneys dataframe for only the North Northamptonshire information
  select(`Bus Journeys Per Capita Per Year`) ##Creates a vector which contains the bus journey information for North Northamptonshire, by filtering for only the relevant column 
Bus_Patronage_North_Northamptonshire <- unlist(Bus_Patronage_North_Northamptonshire, use.names = FALSE) ##Converts the list into a straight vector so we can perform numerical operations with it
Bus_Patronage_West_Northamptonshire <- BusJourneys %>%
  filter(`LA or Region`== "West Northamptonshire") %>% ##Filters the BusJourneys dataframe for only the West Northamptonshire information
  select(`Bus Journeys Per Capita Per Year`) ##Creates a vector which contains the bus journey information for West Northamptonshire, by filtering for only the relevant column 
Bus_Patronage_West_Northamptonshire <- unlist(Bus_Patronage_West_Northamptonshire, use.names = FALSE) ##Converts the list into a straight vector so we can perform numerical operations with it
Bus_Patronage_Northamptonshire = Bus_Patronage_North_Northamptonshire + Bus_Patronage_West_Northamptonshire ##Adds the bus journey numbers together for North Northamptonshire and West Northamptonshire
BusJourneys_NewNorthamptonshire <- data.frame(BJ_LA_Code_Northamptonshire = c("E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021", "E10000021"), BJ_LA_or_Region_Northamptonshire = c("Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire", "Northamptonshire"), BJ_Year_Northamptonshire = c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025"), Bus_Patronage_Northamptonshire) ##Creates a dataframe with the Northamptonshire information
colnames(BusJourneys_NewNorthamptonshire) <- c("Local Authority (LA) Code", "LA or Region", "Year", "Bus Journeys Per Capita Per Year") ##Renames the columns to match the BusJourneys dataframe
BusJourneys_OldNorthamptonshire <- BusJourneys_OldNorthamptonshire %>% filter(Year == 2010:2021) ##Filters for the part of the 'old' Northamptonshire dataframe which actually has values in it
BusJourneys_NewNorthamptonshire <- BusJourneys_NewNorthamptonshire %>% filter(Year == 2022:2025) ##Filters for the part of the 'new'/combined Northamptonshire dataframe which actually has values in it
BusJourneys_list_TotalNorthamptonshire <- list(BusJourneys_OldNorthamptonshire, BusJourneys_NewNorthamptonshire) ##Creates a list of the 'old' Northamptonshire and 'new'/combined Northamptonshire dataframes
BusJourneys_TotalNorthamptonshire <- BusJourneys_list_TotalNorthamptonshire %>% reduce(full_join) ##Combines the list into dataframe so that we now have a dataframe with all the bus journey Northamptonshire information in it 
BusJourneys <- BusJourneys %>%
  filter(`LA or Region` != "North Northamptonshire" & `LA or Region` != "West Northamptonshire" & `LA or Region` != "Northamptonshire") ##Filters North Northamptonshire, West Northamptonshire and Northamptonshire data out of the Bus Journeys data, prior to the new combined Northamptonshire data being added back in.
BusJourneys_list_Northamptonshire <- list(BusJourneys, BusJourneys_TotalNorthamptonshire) ##Creates a list of the overall bus journey data and the new totally combined Northamptonshire data
BusJourneys <- BusJourneys_list_Northamptonshire %>% reduce(full_join) ##Combines the list into one dataframe so that now all the Northamptonshire data is combined with the oritinal data

PrivateCars[PrivateCars=="County Durham"]<-"Durham" ##Renames each entry in the private cars dataframe containing 'County Durham' to 'Durham' to align it with the bus journeys data

##=> This bit combines the two dataframes to unify the data
BusJourneys <- BusJourneys[order(BusJourneys$`LA or Region`), ] ##Sorts the bus journey data alphabetically by region name
PrivateCars <- PrivateCars[order(PrivateCars$`ONS Geography`), ] ##Sorts the private car data alphabetically by region name
BusJourneys <- BusJourneys[1:1408, ] ##Resizes the bus journey dataframe to be the size we want (for now)
PrivateCars <- PrivateCars[1:1408, ] ##Resizes the car ownership dataframe to be the same size as the bus journey dataframe
BusJourneys_PrivateCars <- data.frame(BusJourneys, PrivateCars) ##Creates a new dataframe which is just the two main dataframes stitched together horizontally
BusJourneys_PrivateCars <- BusJourneys_PrivateCars %>% select(-(ONS.Code | ONS.Geography | Year.1)) ##Removes redundant/repeated columns from this dataframe
#View(BusJourneys_PrivateCars) ##Lets us view this new stitched together dataframe