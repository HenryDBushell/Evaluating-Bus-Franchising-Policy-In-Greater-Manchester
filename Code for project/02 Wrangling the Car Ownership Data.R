source("Code for project/01 Wrangling the Bus Journey Data.R")

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
PrivateCars <- PrivateCars %>% rename(`2025` = `2025 Q1`, `2024` = `2024 Q1`, `2023` = `2023 Q1`, `2022` = `2022 Q1`, `2021` = `2021 Q1`, `2020` = `2020 Q1`, `2019` = `2019 Q1`, `2018` = `2018 Q1`, `2017` = `2017 Q1`, `2016` = `2016 Q1`, `2015` = `2015 Q1`, `2014` = `2014 Q1`, `2013` = `2013 Q1`, `2012` = `2012 Q1`, `2011` = `2011 Q1`, `2010` = `2010 Q1`) ##Renames the columns to get rid of the Q1s from the titles
PrivateCars <- PrivateCars %>%
  pivot_longer(
    cols = c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025"),
    names_to = "Year",
    values_to = "Private cars licensed in each region in each year"
  ) ##Rearranges the data into the correct panel data format, ready for combination with the control variable data
PrivateCars <- PrivateCars[order(PrivateCars$`ONS Geography`), ] ##Sorts the data alphabetically by name of local/combined authority
PrivateCars$`Private cars licensed in each region in each year` <- as.numeric(PrivateCars$`Private cars licensed in each region in each year`) ##Transforms the car ownership data from characters into numbers
#View(PrivateCars) ## Lets us view the tidied private cars dataframe


