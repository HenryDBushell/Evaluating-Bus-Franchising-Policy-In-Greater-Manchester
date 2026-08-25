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
  stringr
)

##WRANGLING THE BUS JOURNEY DATA
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
View(BusJourneys) ##Lets us view the BusJourneys dataframe in an easy-to-read spreadsheet format

##WRANGLING THE CAR OWNERSHIP DATA
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
View(PrivateCars) ## Lets us view the tidied private cars dataframe

##COMBINING BUS JOURNEY AND CAR OWNERSHIP DATA
PrivateCars <- PrivateCars[1:1456, ] ##Resizes the car ownership dataframe to be the same size as the bus journey dataframe
BusJourneys_PrivateCars <- data.frame(BusJourneys, PrivateCars) ##Creates a new dataframe which is just the two main dataframes stitched together horizontally
View(BusJourneys_PrivateCars) ##Lets us view this new stitched together dataframe

###Need to sort out the Northamptonshire mix-up before moving on (also look for other mixups in the data)
