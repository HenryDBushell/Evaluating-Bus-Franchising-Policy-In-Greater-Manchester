##install.packages("readODS")
##install.packages("tidyverse")
##install.packages("skimr")
##install.packages("dplyr")
##install.packages("stringr")
##install.packages("readxl")
##install.packages("plm")
##install.packages("lmtest")
##install.packages("writexl")

pacman::p_load(
  tidyverse,
  skimr,
  readODS,
  dplyr,
  stringr,
  readxl,
  plm,
  lmtest,
  writexl
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

