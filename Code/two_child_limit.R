if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools,np,lazyeval, hmisc,interp, lmtest,gt, modelsummary, dplyr,pdftools, tidyverse,rattle,glmnet,caret, rpart.plot, RcolorBrewer,rpart, tidyr, mice, stringr,randomForest,  curl, plm, readxl, zoo, stringr, patchwork,  sf, clubSandwich, modelsummary, sjPlot)


pdf_text <- pdftools::pdf_text(pdf = "~/Library/CloudStorage/OneDrive-Nexus365/Documents/Github/Github_new/two_child_limit/Data/Raw/FOI2024_193746 Tables (1).pdf")


# Step 2: Process the text to extract tables
# Combine all pages into one string
combined_text <- paste(pdf_text, collapse = " ")

# Step 3: Identify and split by year
year_positions <- str_locate_all(combined_text, "\\b(2018|2019|2020)\\b")[[1]]
years <- c("2018", "2019", "2020")

# Step 4: Process each year's data section by slicing the text based on start positions
# Initialize an empty tibble for all years
all_data <- tibble()

for (i in seq_along(years)) {
  # Determine start and end positions for each section
  start_pos <- year_positions[i, 1]
  end_pos <- if (i < length(years)) year_positions[i + 1, 1] - 1 else nchar(combined_text)
  
  # Extract current section and assign the correct year
  current_text <- str_sub(combined_text, start_pos, end_pos)
  current_year <- years[i]
  
  # Extract table data for the current section
  table_data <- str_extract_all(current_text, "\\b([A-Za-z\\s]+)\\s+(\\d+,?\\d*)\\b")[[1]]
  
  # Convert extracted text into a tibble (data frame) for cleaning
  year_df <- tibble(text = table_data) %>%
    separate(text, into = c("Location", "Child.Tax.Credit"), sep = "\\s+(?=\\d)", extra = "merge") %>%
    mutate(Child.Tax.Credit = as.numeric(str_remove_all(Child.Tax.Credit, ",")))
  
  # Add Year column to the data for this section
  year_df <- year_df %>%
    mutate(year = as.numeric(current_year))
  
  # Append to all_data
  all_data <- bind_rows(all_data, year_df)
}

# Step 5: Clean and finalize the data
# Identify Regions and Local Authorities, using cumulative approach
cleaned_data <- all_data %>%
  mutate(Region = ifelse(str_detect(Location, "(Total|Midlands|London|North|South|East|West|Scotland|Wales|Northern Ireland)"),
                         Location, NA),
         Location = Location %>%
           gsub('&', 'and', .) %>%
           gsub('[[:punct:] ]+', ' ', .) %>%
           gsub('[0-9]', '', .)%>%
           toupper() %>%
           gsub("CITY OF", "",.)%>%
           gsub("UA", "",.)%>%
           gsub("COUNTY OF", "",.)%>%
           gsub("ROYAL BOROUGH OF", "",.)%>%
           gsub("LEICESTER CITY", "LEICESTER",.)%>%
           gsub("UA", "",.)%>%
           gsub("DARWIN", "DARWEN", .)%>%
           gsub("COUNTY DURHAM", "DURHAM", .)%>%
           gsub("AND DARWEN", "WITH DARWEN", .)%>%
           gsub("NE SOM", "NORTH EAST SOM", .)%>%
           gsub("N E SOM", "NORTH EAST SOM", .)%>%
           str_trim()) %>%
  fill(Region) %>%
  filter(!str_detect(Location, "(Total|OFFICIAL)"))  # Remove unnecessary rows


####Universal Credit####

cleaned_uc <-rbind( read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Github/Github_new/two_child_limit/Data/Raw/2CP_2018_2023_Data_April2024_LAs.xlsx", sheet=3, skip=5)%>%
                      dplyr::rename(Location=...1,
                                    Total.Universal.Credit = `Number of households`,
                                    Universal.Credit = `Number of households not receiving a child element/amount for at least one child`,
                                    Proportion.Universal.Credit = `Proportion of households not receiving a child element/amount for at least one child`)%>%
                      dplyr::select(-...2)%>%
                      dplyr::mutate(year = 2018),
                    read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Github/Github_new/two_child_limit/Data/Raw/2CP_2018_2023_Data_April2024_LAs.xlsx", sheet=4, skip=5)%>%
                      dplyr::rename(Location=...1,
                                    Total.Universal.Credit = `Number of households`,
                                    Universal.Credit = `Number of households not receiving a child element/amount for at least one child`,
                                    Proportion.Universal.Credit = `Proportion of households not receiving a child element/amount for at least one child`)%>%
                      dplyr::select(-...2)%>%
                      dplyr::mutate(year = 2019),
                    read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Github/Github_new/two_child_limit/Data/Raw/2CP_2018_2023_Data_April2024_LAs.xlsx", sheet=5, skip=5)%>%
                      dplyr::rename(Location=...1,
                                    Total.Universal.Credit = `Number of households`,
                                    Universal.Credit = `Number of households not receiving a child element/amount for at least one child`,
                                    Proportion.Universal.Credit = `Proportion of households not receiving a child element/amount for at least one child`)%>%
                      dplyr::select(-...2)%>%
                      dplyr::mutate(year = 2020),
                    read_excel("~/Library/CloudStorage/OneDrive-Nexus365/Documents/Github/Github_new/two_child_limit/Data/Raw/2CP_2018_2023_Data_April2024_LAs.xlsx", sheet=6, skip=5)%>%
                      dplyr::rename(Location=...1,
                                    Total.Universal.Credit = `Number of households`,
                                    Universal.Credit = `Number of households not receiving a child element/amount for at least one child`,
                                    Proportion.Universal.Credit = `Proportion of households not receiving a child element/amount for at least one child`)%>%
                      dplyr::select(-...2)%>%
                      dplyr::mutate(year = 2021))%>%
  dplyr::mutate(Location = Location %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())

cleaned_data <- merge(cleaned_data, cleaned_uc, by=c("year", "Location"),all=T)

#### after####


y2021 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/two_child_limit/refs/heads/main/Data/Raw/data-tables-universal-credit-and-child-tax-credit-claimants-statistics-april-2021%20(3).csv"), skip=4)[c(2:5)]%>%
  dplyr::rename(Location=X.1,
                Universal.Credit.or.Child.Tax.Credits = Total)%>%
  dplyr::mutate(year=2021,
                Child.Tax.Credit = as.numeric(str_remove_all(Child.Tax.Credit, ",")),
                Universal.Credit = as.numeric(str_remove_all(Universal.Credit, ",")),
                Universal.Credit.or.Child.Tax.Credits = as.numeric(str_remove_all(Universal.Credit.or.Child.Tax.Credits, ",")))%>%
  dplyr::select(-Universal.Credit)



y2022 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/two_child_limit/refs/heads/main/Data/Raw/data-tables-universal-credit-and-child-tax-credit-claimants-statistics-april-2022%20(1).csv"), skip=4)[c(2:13)]%>%
  dplyr::rename(Location=X.1,
                Total.Both = X.2,
                Proportion.Both = X.3,
                Total.Universal.Credit = X.5,
                Proportion.Universal.Credit =X.6,
                Total.Child.Tax.Credit = X.8,
                Proportion.Child.Tax.Credit = X.9)%>%
  dplyr::mutate(year=2022,
                Child.Tax.Credit = as.numeric(str_remove_all(Child.Tax.Credit, ",")),
                Total.Universal.Credit = as.numeric(str_remove_all(Total.Universal.Credit, ",")),
                Proportion.Both = as.numeric(str_remove_all(Proportion.Both, "%")),
                Proportion.Universal.Credit = as.numeric(str_remove_all(Proportion.Universal.Credit, "%")),
                Proportion.Child.Tax.Credit = as.numeric(str_remove_all(Proportion.Child.Tax.Credit, "%")),
                Universal.Credit = as.numeric(str_remove_all(Universal.Credit, ",")),
                Universal.Credit.or.Child.Tax.Credits = as.numeric(str_remove_all(Universal.Credit.or.Child.Tax.Credits, ",")))%>%
  dplyr::filter(Location!="")%>%
  dplyr::select(-X.4, -X.7)


y2023 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/two_child_limit/refs/heads/main/Data/Raw/data-tables-universal-credit-and-child-tax-credit-claimants-statistics-april-2023.csv"), skip=4)[c(2:13)]%>%
  dplyr::rename(Location=X.1,
                Total.Both = X.2,
                Proportion.Both = X.3,
                Total.Universal.Credit = X.5,
                Proportion.Universal.Credit =X.6,
                Total.Child.Tax.Credit = X.8,
                Proportion.Child.Tax.Credit = X.9)%>%
  dplyr::mutate(year=2023,
                Child.Tax.Credit = as.numeric(str_remove_all(Child.Tax.Credit, ",")),
                Total.Universal.Credit = as.numeric(str_remove_all(Total.Universal.Credit, ",")),
                Universal.Credit = as.numeric(str_remove_all(Universal.Credit, ",")),
                Proportion.Both = as.numeric(str_remove_all(Proportion.Both, "%")),
                Proportion.Universal.Credit = as.numeric(str_remove_all(Proportion.Universal.Credit, "%")),
                Proportion.Child.Tax.Credit = as.numeric(str_remove_all(Proportion.Child.Tax.Credit, "%")),
                Universal.Credit.or.Child.Tax.Credits = as.numeric(str_remove_all(Universal.Credit.or.Child.Tax.Credits, ",")))%>%
  dplyr::filter(Location!="")%>%
  dplyr::select(-X.4, -X.7)

y2024 <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/two_child_limit/refs/heads/main/Data/Raw/data-tables-universal-credit-and-child-tax-credit-claimants-statistics-april-2024%20(1).csv"), skip=4)[c(2:13)]%>%
  dplyr::rename(Location=X.1,
                Total.Both = X.2,
                Proportion.Both = X.3,
                Total.Universal.Credit = X.5,
                Proportion.Universal.Credit =X.6,
                Total.Child.Tax.Credit = X.8,
                Proportion.Child.Tax.Credit = X.9)%>%
  dplyr::mutate(year=2024,
                Child.Tax.Credit = as.numeric(str_remove_all(Child.Tax.Credit, ",")),
                Proportion.Both = as.numeric(str_remove_all(Proportion.Both, "%")),
                Total.Universal.Credit = as.numeric(str_remove_all(Total.Universal.Credit, ",")),
                Proportion.Universal.Credit = as.numeric(str_remove_all(Proportion.Universal.Credit, "%")),
                Proportion.Child.Tax.Credit = as.numeric(str_remove_all(Proportion.Child.Tax.Credit, "%")),
                Universal.Credit = as.numeric(str_remove_all(Universal.Credit, ",")),
                Universal.Credit.or.Child.Tax.Credits = as.numeric(str_remove_all(Universal.Credit.or.Child.Tax.Credits, ",")))%>%
  dplyr::filter(Location!="")%>%
  dplyr::select(-X.4, -X.7)

lookup <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/two_child_limit/refs/heads/main/Data/Raw/Lower_Tier_Local_Authority_to_Upper_Tier_Local_Authority_(December_2022)_Lookup_in_England_and_Wales.csv"))%>%
  dplyr::mutate(Location = LTLA22NM %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim(),
                Local.authority = UTLA22NM %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())


####populations####

pop <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/two_child_limit/refs/heads/main/Data/Raw/myebtablesenglandwales20112023.csv"), skip=1)


under1 <- pop %>%
  dplyr::filter(age<1)%>%
  tidyr::pivot_longer(cols = c("population_2011",
                               "population_2012",
                               "population_2013",
                               "population_2014",
                               "population_2015",
                               "population_2016",
                               "population_2017",
                               "population_2018",
                               "population_2019",
                               "population_2020",
                               "population_2021",
                               "population_2022",
                               "population_2023"), names_to = "year", values_to = "under_1_population")%>%
  dplyr::select(-age, -sex, -ladcode23, -country)%>%
  dplyr::mutate(under_1_population = gsub(",", "", under_1_population))%>%
  dplyr::group_by(year, laname23)%>%
  dplyr::summarise(under_1_population = sum(as.numeric(under_1_population), na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(year = as.numeric(gsub("population_", "", year)),
                Location = laname23 %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())%>%
  dplyr::full_join(., lookup, by="Location")%>%
  dplyr::select(Local.authority, year, under_1_population)%>%
  dplyr::group_by(Local.authority, year)%>%
  dplyr::summarise(under_1_population = sum(under_1_population, na.rm=T))%>%
  dplyr::ungroup()



onetofour <- pop %>%
  dplyr::filter(age>0&age<5)%>%
  tidyr::pivot_longer(cols = c("population_2011",
                               "population_2012",
                               "population_2013",
                               "population_2014",
                               "population_2015",
                               "population_2016",
                               "population_2017",
                               "population_2018",
                               "population_2019",
                               "population_2020",
                               "population_2021",
                               "population_2022",
                               "population_2023"), names_to = "year", values_to = "under_5_population")%>%
  dplyr::select(-age, -sex, -ladcode23, -country)%>%
  dplyr::mutate(under_5_population = gsub(",", "", under_5_population))%>%
  dplyr::group_by(year, laname23)%>%
  dplyr::summarise(under_5_population = sum(as.numeric(under_5_population), na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(year = as.numeric(gsub("population_", "", year)),
                Location = laname23 %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())%>%
  dplyr::full_join(., lookup, by="Location")%>%
  dplyr::select(Local.authority, year, under_5_population)%>%
  dplyr::group_by(Local.authority, year)%>%
  dplyr::summarise(under_5_population = sum(under_5_population, na.rm=T))%>%
  dplyr::ungroup()



under1 <- pop %>%
  dplyr::filter(age<1)%>%
  tidyr::pivot_longer(cols = c("population_2011",
                               "population_2012",
                               "population_2013",
                               "population_2014",
                               "population_2015",
                               "population_2016",
                               "population_2017",
                               "population_2018",
                               "population_2019",
                               "population_2020",
                               "population_2021",
                               "population_2022",
                               "population_2023"), names_to = "year", values_to = "under_5_population")%>%
  dplyr::select(-age, -sex, -ladcode23, -country)%>%
  dplyr::mutate(under_5_population = gsub(",", "", under_5_population))%>%
  dplyr::group_by(year, laname23)%>%
  dplyr::summarise(under_5_population = sum(as.numeric(under_5_population), na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(year = as.numeric(gsub("population_", "", year)),
                Location = laname23 %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())%>%
  dplyr::full_join(., lookup, by="Location")%>%
  dplyr::select(Local.authority, year, under_5_population)%>%
  dplyr::group_by(Local.authority, year)%>%
  dplyr::summarise(under_5_population = sum(under_5_population, na.rm=T))%>%
  dplyr::ungroup()


under5 <- pop %>%
  dplyr::filter(age<5)%>%
  tidyr::pivot_longer(cols = c("population_2011",
                               "population_2012",
                               "population_2013",
                               "population_2014",
                               "population_2015",
                               "population_2016",
                               "population_2017",
                               "population_2018",
                               "population_2019",
                               "population_2020",
                               "population_2021",
                               "population_2022",
                               "population_2023"), names_to = "year", values_to = "under_5_population")%>%
  dplyr::select(-age, -sex, -ladcode23, -country)%>%
  dplyr::mutate(under_5_population = gsub(",", "", under_5_population))%>%
  dplyr::group_by(year, laname23)%>%
  dplyr::summarise(under_5_population = sum(as.numeric(under_5_population), na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(year = as.numeric(gsub("population_", "", year)),
                Location = laname23 %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())%>%
  dplyr::full_join(., lookup, by="Location")%>%
  dplyr::select(Local.authority, year, under_5_population)%>%
  dplyr::group_by(Local.authority, year)%>%
  dplyr::summarise(under_5_population = sum(under_5_population, na.rm=T))%>%
  dplyr::ungroup()
  
under19 <- pop %>%
  dplyr::filter(age<19)%>%
  tidyr::pivot_longer(cols = c("population_2011",
                               "population_2012",
                               "population_2013",
                               "population_2014",
                               "population_2015",
                               "population_2016",
                               "population_2017",
                               "population_2018",
                               "population_2019",
                               "population_2020",
                               "population_2021",
                               "population_2022",
                               "population_2023"), names_to = "year", values_to = "under_19_population")%>%
  dplyr::select(-age, -sex, -ladcode23, -country)%>%
  dplyr::mutate(under_19_population = gsub(",", "", under_19_population))%>%
  dplyr::group_by(year, laname23)%>%
  dplyr::summarise(under_19_population = sum(as.numeric(under_19_population), na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(year = as.numeric(gsub("population_", "", year)),
                Location = laname23 %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())%>%
  dplyr::full_join(., lookup, by="Location")%>%
  dplyr::select(Local.authority, year, under_19_population)%>%
  dplyr::group_by(Local.authority, year)%>%
  dplyr::summarise(under_19_population = sum(under_19_population, na.rm=T))%>%
  dplyr::ungroup()


over15_under19 <- pop %>%
  dplyr::filter(age>15&age<19)%>%
  tidyr::pivot_longer(cols = c("population_2011",
                               "population_2012",
                               "population_2013",
                               "population_2014",
                               "population_2015",
                               "population_2016",
                               "population_2017",
                               "population_2018",
                               "population_2019",
                               "population_2020",
                               "population_2021",
                               "population_2022",
                               "population_2023"), names_to = "year", values_to = "over_16_population")%>%
  dplyr::select(-age, -sex, -ladcode23, -country)%>%
  dplyr::mutate(over_16_population = gsub(",", "", over_16_population))%>%
  dplyr::group_by(year, laname23)%>%
  dplyr::summarise(over_16_population = sum(as.numeric(over_16_population), na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(year = as.numeric(gsub("population_", "", year)),
                Location = laname23 %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())%>%
  dplyr::full_join(., lookup, by="Location")%>%
  dplyr::select(Local.authority, year, over_16_population)%>%
  dplyr::group_by(Local.authority, year)%>%
  dplyr::summarise(over_16_population = sum(over_16_population, na.rm=T))%>%
  dplyr::ungroup()



affected_pop  <- pop %>%
  tidyr::pivot_longer(cols = c("population_2011",
                               "population_2012",
                               "population_2013",
                               "population_2014",
                               "population_2015",
                               "population_2016",
                               "population_2017",
                               "population_2018",
                               "population_2019",
                               "population_2020",
                               "population_2021",
                               "population_2022",
                               "population_2023"), names_to = "year", values_to = "population")%>%
  dplyr::select(-sex, -ladcode23, -country)%>%
  dplyr::mutate(population = gsub(",", "", population))%>%
  dplyr::group_by(year, laname23, age)%>%
  dplyr::summarise(population = sum(as.numeric(population), na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(year = as.numeric(gsub("population_", "", year)),
                Location = laname23 %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())%>%
  dplyr::full_join(., lookup, by="Location")%>%
  dplyr::select(Local.authority, year, population, age)%>%
  dplyr::group_by(Local.authority, year, age)%>%
  dplyr::summarise(population = sum(population, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::filter(year==2018&age<1|
                year==2019&age<2|
                year==2020&age<3|
                year==2021&age<4|
                year==2022&age<5|
                year==2023&age<6)%>%
  dplyr::select(-age)%>%
  dplyr::group_by(Local.authority, year)%>%
  dplyr::summarise(affected_population = sum(population, na.rm=T))%>%
  dplyr::ungroup()


final_data <- bind_rows(cleaned_data, y2021, y2022, y2023, y2024)%>%
  dplyr::select(Location, year, Total.Both, Universal.Credit.or.Child.Tax.Credits, Proportion.Both, 
                Total.Universal.Credit, Universal.Credit, Proportion.Universal.Credit,
                Total.Child.Tax.Credit, Child.Tax.Credit,  Proportion.Child.Tax.Credit)%>%
  dplyr::mutate(Location = Location %>%
                  gsub('&', 'and', .) %>%
                  gsub('[[:punct:] ]+', ' ', .) %>%
                  gsub('[0-9]', '', .)%>%
                  toupper() %>%
                  gsub("CITY OF", "",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("COUNTY OF", "",.)%>%
                  gsub("ROYAL BOROUGH OF", "",.)%>%
                  gsub("LEICESTER CITY", "LEICESTER",.)%>%
                  gsub("UA", "",.)%>%
                  gsub("DARWIN", "DARWEN", .)%>%
                  gsub("COUNTY DURHAM", "DURHAM", .)%>%
                  gsub("AND DARWEN", "WITH DARWEN", .)%>%
                  gsub("NE SOM", "NORTH EAST SOM", .)%>%
                  gsub("N E SOM", "NORTH EAST SOM", .)%>%
                  str_trim())%>%
  dplyr::full_join(., lookup, by="Location")%>%
  dplyr::select(Local.authority, year, Total.Both, Universal.Credit.or.Child.Tax.Credits, Proportion.Both, 
                Total.Universal.Credit, Universal.Credit, Proportion.Universal.Credit,
                Total.Child.Tax.Credit, Child.Tax.Credit,  Proportion.Child.Tax.Credit)%>%
  dplyr::group_by(Local.authority, year)%>%
  dplyr::summarise(
    Total.Both = mean(as.numeric(Total.Both), na.rm=T),
    Universal.Credit.or.Child.Tax.Credits = mean(as.numeric(Universal.Credit.or.Child.Tax.Credits), na.rm=T),
    Proportion.Both = mean(as.numeric(Proportion.Both), na.rm=T),
    Total.Universal.Credit = mean(as.numeric(Total.Universal.Credit), na.rm=T),
    Universal.Credit = mean(as.numeric(Universal.Credit), na.rm=T),
    Proportion.Universal.Credit = mean(as.numeric(Proportion.Universal.Credit), na.rm=T),
    Total.Child.Tax.Credit = mean(as.numeric(Total.Child.Tax.Credit), na.rm=T),
    Child.Tax.Credit = mean(as.numeric(Child.Tax.Credit), na.rm=T),
    Proportion.Child.Tax.Credit = mean(as.numeric(Proportion.Child.Tax.Credit), na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::full_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>% 
                     dplyr::filter(category=="child characteristic at 31st March",
                                   variable=="Total" &subcategory=="Age group") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   children_in_care = number)%>%
                     dplyr::select(Local.authority, year, children_in_care), by=c("Local.authority", "year"))%>%
  dplyr::full_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>% 
                     dplyr::filter(category=="child characteristic at 31st March",
                                   variable=="Under 1 year" &subcategory=="Age group") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   children_in_care_1_under = number)%>%
                     dplyr::select(Local.authority, year, children_in_care_1_under), by=c("Local.authority", "year"))%>%
  dplyr::full_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>% 
                     dplyr::filter(category=="child characteristic at 31st March",
                                   variable=="1 to 4 years" &subcategory=="Age group") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   children_in_care_1_4 = number)%>%
                     dplyr::select(Local.authority, year, children_in_care_1_4), by=c("Local.authority", "year"))%>%
  dplyr::full_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>% 
                     dplyr::filter(category=="child characteristic at 31st March",
                                   variable=="5 to 9 years" &subcategory=="Age group") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   children_in_care_5_9 = number)%>%
                     dplyr::select(Local.authority, year, children_in_care_5_9), by=c("Local.authority", "year"))%>%
  dplyr::full_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>% 
                     dplyr::filter(category=="child characteristic at 31st March",
                                   variable=="10 to 15 years" &subcategory=="Age group") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   children_in_care_10_15 = number)%>%
                     dplyr::select(Local.authority, year, children_in_care_10_15), by=c("Local.authority", "year"))%>%
  dplyr::full_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>% 
                     dplyr::filter(category=="child characteristic at 31st March",
                                   variable=="16 years and over" &subcategory=="Age group") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   children_in_care_16plus = number)%>%
                     dplyr::select(Local.authority, year, children_in_care_16plus), by=c("Local.authority", "year"))%>%
  dplyr::full_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>% 
                     dplyr::filter(category=="started during",
                                   variable=="Under 1 year" &subcategory=="Age group") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   started_under_1 = number)%>%
                     dplyr::select(Local.authority, year, started_under_1), by=c("Local.authority", "year"))%>%
  dplyr::full_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>% 
                     dplyr::filter(category=="started during",
                                   variable=="Total" &subcategory=="Age group") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   started_during = number)%>%
                     dplyr::select(Local.authority, year, started_during), by=c("Local.authority", "year"))%>%
  dplyr::full_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>% 
                     dplyr::filter(category=="started during",
                                   variable=="1 to 4 years" &subcategory=="Age group") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   started_1_to_4 = number)%>%
                     dplyr::select(Local.authority, year, started_1_to_4), by=c("Local.authority", "year"))%>%
  dplyr::full_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>% 
                     dplyr::filter(category=="started during",
                                   variable=="5 to 9 years" &subcategory=="Age group") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   started_5_to_9 = number)%>%
                     dplyr::select(Local.authority, year, started_5_to_9), by=c("Local.authority", "year"))%>%
  dplyr::full_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>% 
                     dplyr::filter(category=="started during",
                                   variable=="10 to 15 years" &subcategory=="Age group") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   started_10_to_15 = number)%>%
                     dplyr::select(Local.authority, year, started_10_to_15), by=c("Local.authority", "year"))%>%
    dplyr::full_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>% 
                     dplyr::filter(category=="started during",
                                   variable=="16 years and over" &subcategory=="Age group") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   started_over_16 = number)%>%
                     dplyr::select(Local.authority, year, started_over_16), by=c("Local.authority", "year"))%>%
  dplyr::full_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>% 
                     dplyr::filter(category=="ceased during",
                                   variable=="1 to 4 years" &subcategory=="Age on ceasing") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   ceased_1_to_4 = number)%>%
                     dplyr::select(Local.authority, year, ceased_1_to_4), by=c("Local.authority", "year"))%>%
  dplyr::full_join(., read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>% 
                     dplyr::filter(category=="ceased during",
                                   variable=="Under 1 year" &subcategory=="Age on ceasing") %>%
                     dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
                     dplyr::rename(Local.authority = LA_Name,
                                   ceased_under_1 = number)%>%
                     dplyr::select(Local.authority, year, ceased_under_1), by=c("Local.authority", "year"))%>%
  dplyr::filter(!is.na(Local.authority),
                !is.na(children_in_care),
                Local.authority!="DORSET",
                Local.authority!="LONDON")%>%
  dplyr::mutate(Child.Tax.Credit = Child.Tax.Credit%>% gsub(",","",.))%>%
  dplyr::mutate(Child.Tax.Credit_did = as.numeric(ifelse(year<2018, 0, Child.Tax.Credit)),
                post = ifelse(year<2018, "0", "1"),
                time = factor(year),
                children_in_care = as.numeric(children_in_care),
                started_under_5 = as.numeric(started_under_1)+as.numeric(started_1_to_4),
                ceased_under_5 = as.numeric(ceased_under_1)+as.numeric(ceased_1_to_4),
                total_in_care_under_5= as.numeric(children_in_care_1_under)+as.numeric(children_in_care_1_4))%>%
  dplyr::full_join(., under5, by=c("Local.authority", "year"))%>%
  dplyr::full_join(., under19, by=c("Local.authority", "year"))%>%
  dplyr::full_join(., over15_under19, by=c("Local.authority", "year"))%>%
  dplyr::full_join(., affected_pop, by=c("Local.authority", "year"))%>%
  dplyr::filter(!is.na(Local.authority),
                !is.na(year))%>%
  dplyr::mutate(percent_impacted_uc = Universal.Credit/affected_population*100,
                percent_impacted_ctc = as.numeric(Child.Tax.Credit)/affected_population*100,
                percent_impacted = (as.numeric(Child.Tax.Credit)+Universal.Credit)/affected_population*100,
                percent_impacted_did = as.numeric(ifelse(year<2018, 0, percent_impacted)))

# Sort data by Local.authority and year
final_data <- final_data[order(final_data$Local.authority, final_data$year), ]

# Manually calculate the lagged variable within each Local.authority group
final_data$lagged_UC <- ave(final_data$Universal.Credit, final_data$Local.authority, FUN = function(x) c(NA, head(x, -1)))
final_data$lagged_CTC <- ave(final_data$Child.Tax.Credit, final_data$Local.authority, FUN = function(x) c(NA, head(x, -1)))

# Manually calculate the lagged variable within each Local.authority group
final_data$lagged_UC_per <- ave(final_data$percent_impacted_uc, final_data$Local.authority, FUN = function(x) c(NA, head(x, -1)))
final_data$lagged_CTC_per<- ave(final_data$percent_impacted_ctc, final_data$Local.authority, FUN = function(x) c(NA, head(x, -1)))
final_data$lagged_both_per<- ave(final_data$percent_impacted, final_data$Local.authority, FUN = function(x) c(NA, head(x, -1)))



####analysis####

yes <- pdata.frame(final_data, index = c("Local.authority","year"))


####DID following code from https://mixtape-sessions.github.io/Frontiers-in-DID/Exercises/Exercise-2/exercise2a_sol.html####

#devtools::install_github("shommazumder/binscatteR")

library(ggplot2)
library(binscatteR)
library(np)


reg <- final_data%>%
  select(percent_impacted_did, Local.authority)%>%
  group_by(Local.authority)%>%
  dplyr::summarise(treatment_mean = mean(as.numeric(percent_impacted_did), na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::filter(treatment_mean!=0)%>%
  dplyr::select(Local.authority, treatment_mean)

reg2 <- final_data%>%
  dplyr::mutate(children_in_care_per_18 = children_in_care/under_19_population)%>%
  dplyr::filter(year==2018)%>%
  select(children_in_care_per_18, Local.authority)

reg3 <- final_data%>%
  dplyr::mutate(children_in_care_per_23 = children_in_care/under_19_population)%>%
  dplyr::filter(year==2023)%>%
  select(children_in_care_per_23, Local.authority)

did <- merge(reg,reg2, by="Local.authority")
did <- merge(did,reg3, by="Local.authority")

did$d_care = did$children_in_care_per_23-did$children_in_care_per_18

#' @param l a particular value of the treatment for which to compute weights
#' @param D an nx1 vector containing doses for all units
cont_twfe_weights <- function(l, D) {
  wt <- ( ( mean(D[D>=l]) - mean(D) ) * mean(1*(D>=l)) ) / var(D)
  wt
}

#' nonparametric estimates of att(d|d) and acrt(d|d)
#' @param dy the change in the outcome over time
#' @param dose the amount of the treatment
#' @return list( 
#'            local_effects - data frame containing the dose and estimates of 
#'              att(dose) and acrt(dose)
#'            att.overall - an estimate of the overall att
#'            acrt.overall - an estimate of the overall acrt
#'          )
cont_did <- function(dy, dose) {
  # choose bandwidth
  bw <- np::npregbw(formula=dy ~ dose,
                    regtype="ll",
                    bws=1.06,
                    bwscaling=TRUE,
                    bandwidth.compute=FALSE)
  # estimate att and acrt nonparametrically
  out <- np::npreg(bws=bw, gradients=TRUE, exdat=dose)
  
  # order from smallest to largest dose and drop untreated
  this_order <- order(dose)
  dose <- dose[this_order]
  dy <- dy[this_order]
  att.d <- out$mean[this_order]
  acrt.d <- out$grad[,1][this_order]
  att.d <- att.d[dose>0]
  acrt.d <- acrt.d[dose>0]
  att.overall <- mean(att.d)
  acrt.overall <- mean(acrt.d)
  
  return(list(local_effects=data.frame(dose=dose[dose>0],
                                       att.d=att.d,
                                       acrt.d=acrt.d),
              att.overall=att.overall,
              acrt.overall=acrt.overall))
}


dose <- did$treatment_mean
dy <- did$d_care

twfe <- lm(dy ~ dose)
summary(twfe)$coefficients


p <- ggplot(data.frame(dose=dose), aes(x=dose)) + 
  geom_histogram()
p


binnedout <- binscatter(data=did, x="treatment_mean", y="d_care")
binnedout





cont_res <- cont_did(dy, dose)
cont_res$att.overall

cont_res$acrt.overall


plot_df <- cont_res$local_effects

colnames(plot_df) <- c("dose", "att", "acrt")
ggplot(plot_df, aes(x=dose, att)) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  geom_line() +
  theme_bw()

ggplot(plot_df, aes(x=dose, acrt)) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  geom_line() +
  theme_bw()

did_model <- plm(
  as.numeric(percent_in_care) ~ Universal.Credit_mean * post + factor(time), # DiD interaction term with fixed effects
  data = reg,
  index = c("Local.authority", "time"),     # Panel structure (unit and time identifiers)
  model = "within"             # Fixed effects model
)

clustered_se <- coeftest(did_model, vcov = vcovHC(did_model, type = "HC1", cluster = "group"))
print(clustered_se)















####TWFE CIC#### 
FinalFE1 <- plm(as.numeric(started_under_1)~lagged_both_per+under_5_population,  data=yes, effect = "twoway", model = "within")
FinalFE2 <- plm(as.numeric(started_1_to_4)~lagged_both_per+under_5_population,  data=yes, effect = "twoway", model = "within")
FinalFE3 <- plm(as.numeric(started_5_to_9)~lagged_both_per+under_5_population,  data=yes, effect = "twoway", model = "within")
FinalFE4 <- plm(as.numeric(started_10_to_15)~lagged_both_per+under_5_population,  data=yes, effect = "twoway", model = "within")
FinalFE5 <- plm(as.numeric(started_during)~lagged_both_per+under_5_population,  data=yes, effect = "twoway", model = "within")



rows <- tribble(~term,          ~`ln. Treatable Mortality [95% ci]`,  ~`p-value`,  ~`ln. Treatable Mortality [95% ci]`,  ~`p-value`,  ~`ln. Treatable Mortality [95% ci]`,  ~`p-value`,  ~`ln. Treatable Mortality [95% ci]`,  ~`p-value`,  ~`ln. Treatable Mortality [95% ci]`,  ~`p-value`, 
                'LA Fixed Effects', 'Yes',  'Yes','Yes',  'Yes','Yes',  'Yes','Yes',  'Yes','Yes','Yes',
                'Time Fixed Effects', 'Yes',  'Yes','Yes','Yes','Yes','Yes',  'Yes','Yes','Yes','Yes',
                'Clustered Standard Errors', 'Yes',  'Yes','Yes','Yes','Yes','Yes',  'Yes','Yes','Yes','Yes')

cm <- c("lagged_both_per" = "Households affected by two-child limit (%)", 
        "under_5_population" = "Children under 5 (n)")


FinalFEsum1 <- as.list(modelsummary(FinalFE1, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
FinalFEsum2 <- as.list(modelsummary(FinalFE2, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
FinalFEsum3 <- as.list(modelsummary(FinalFE3, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
FinalFEsum4 <- as.list(modelsummary(FinalFE4, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))
FinalFEsum5 <- as.list(modelsummary(FinalFE5, output = "modelsummary_list", statistic = c("conf.int","p={p.value}")))

FinalFEsum1$tidy$p.value <- coef_test(FinalFE1, vcov = "CR2", cluster = yes$Local.authority, test = "Satterthwaite")$p
FinalFEsum1$tidy$std.error <- coef_test(FinalFE1, vcov = "CR2", cluster = yes$Local.authority, test = "Satterthwaite")$SE
FinalFEsum1$tidy$conf.low <- FinalFEsum1$tidy$estimate-(1.96*coef_test(FinalFE1, vcov = "CR2", cluster = yes$Local.authority, test = "Satterthwaite")$SE)
FinalFEsum1$tidy$conf.high <- FinalFEsum1$tidy$estimate+(1.96*coef_test(FinalFE1, vcov = "CR2", cluster = yes$Local.authority, test = "Satterthwaite")$SE)
FinalFEsum1$tidy$estimate <- FinalFEsum1$tidy$estimate

FinalFEsum2$tidy$p.value <- coef_test(FinalFE2, vcov = "CR2", cluster = yes$Local.authority, test = "Satterthwaite")$p
FinalFEsum2$tidy$std.error <- coef_test(FinalFE2, vcov = "CR2", cluster = yes$Local.authority, test = "Satterthwaite")$SE
FinalFEsum2$tidy$conf.low <- FinalFEsum2$tidy$estimate-(1.96*coef_test(FinalFE2, vcov = "CR2", cluster = yes$Local.authority, test = "Satterthwaite")$SE)
FinalFEsum2$tidy$conf.high <- FinalFEsum2$tidy$estimate+(1.96*coef_test(FinalFE2, vcov = "CR2", cluster = yes$Local.authority, test = "Satterthwaite")$SE)
FinalFEsum2$tidy$estimate <- FinalFEsum2$tidy$estimate

FinalFEsum3$tidy$p.value <- coef_test(FinalFE3, vcov = "CR2", cluster = yes$Local.authority, test = "Satterthwaite")$p
FinalFEsum3$tidy$std.error <- coef_test(FinalFE3, vcov = "CR2", cluster = yes$Local.authority, test = "Satterthwaite")$SE
FinalFEsum3$tidy$conf.low <- FinalFEsum3$tidy$estimate-(1.96*coef_test(FinalFE3, vcov = "CR2", cluster = yes$Local.authority, test = "Satterthwaite")$SE)
FinalFEsum3$tidy$conf.high <- FinalFEsum3$tidy$estimate+(1.96*coef_test(FinalFE3, vcov = "CR2", cluster = yes$Local.authority, test = "Satterthwaite")$SE)
FinalFEsum3$tidy$estimate <- FinalFEsum3$tidy$estimate

FinalFEsum4$tidy$p.value <- coef_test(FinalFE4, vcov = "CR2", cluster = yes$Local.authority, test = "Satterthwaite")$p
FinalFEsum4$tidy$std.error <- coef_test(FinalFE4, vcov = "CR2", cluster = yes$Local.authority, test = "Satterthwaite")$SE
FinalFEsum4$tidy$conf.low <- FinalFEsum4$tidy$estimate-(1.96*coef_test(FinalFE4, vcov = "CR2", cluster = yes$Local.authority, test = "Satterthwaite")$SE)
FinalFEsum4$tidy$conf.high <- FinalFEsum4$tidy$estimate+(1.96*coef_test(FinalFE4, vcov = "CR2", cluster = yes$Local.authority, test = "Satterthwaite")$SE)
FinalFEsum4$tidy$estimate <- FinalFEsum4$tidy$estimate

FinalFEsum5$tidy$p.value <- coef_test(FinalFE5, vcov = "CR2", cluster = yes$Local.authority, test = "Satterthwaite")$p
FinalFEsum5$tidy$std.error <- coef_test(FinalFE5, vcov = "CR2", cluster = yes$Local.authority, test = "Satterthwaite")$SE
FinalFEsum5$tidy$conf.low <- FinalFEsum5$tidy$estimate-(1.96*coef_test(FinalFE5, vcov = "CR2", cluster = yes$Local.authority, test = "Satterthwaite")$SE)
FinalFEsum5$tidy$conf.high <- FinalFEsum5$tidy$estimate+(1.96*coef_test(FinalFE5, vcov = "CR2", cluster = yes$Local.authority, test = "Satterthwaite")$SE)
FinalFEsum5$tidy$estimate <- FinalFEsum5$tidy$estimate


modelsummary(list("Under 1 [.95 ci]"=FinalFEsum1,"p-value"=FinalFEsum1,"Aged 1-4 [.95 ci]"=FinalFEsum2,"p-value"=FinalFEsum2,"Aged 5-9 [.95 ci]"=FinalFEsum3,"p-value"=FinalFEsum3,"Aged 10-16 [.95 ci]"=FinalFEsum4,"p-value"=FinalFEsum4,"All children in care [.95 ci]"=FinalFEsum5,"p-value"=FinalFEsum5),
             coef_omit = "Intercept|dept|year", add_rows = rows, 
             coef_map=cm,fmt = 4, estimate = c("{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value","{estimate} [{conf.low}, {conf.high}]", "p.value"), statistic = NULL,
             notes = list('Table reports results from multivariate longitudinal regression models.',
                          'Two-child limit impact has a one year lag.',
                          'Robust SEs are clustered at LA level and use a bias-reduced linearization estimator (CR2)'),
             output = "gt") #%>%
  # add_header_above(c(" ", "Fixed Effects" = 2, "First Differences" = 2, "Covariate Balancing (1)" = 2, "Covariate Balancing (2)" = 2, "Multi-Level Model" = 2))
  # tab_spanner(label = 'Fixed Effects', columns = 2:3) %>%
  # tab_spanner(label = 'First Differences', columns = 4:5) %>%
  # tab_spanner(label = 'Covariate Balancing (1)', columns = 6:7) %>%
  # tab_spanner(label = 'Covariate Balancing (2)', columns = 8:9) %>%
  # tab_spanner(label = 'Multi-Level Model', columns = 10:11)










summary(lm(as.numeric(started_under_5)~as.numeric(lagged_UC_per)*as.numeric(time), data = final_data))



ja <- (plm(as.numeric(total_in_care_under_5)~ as.numeric(lagged_both_per)+under_5_population, data=yes, effect = "twoway", model = "within"))

coeftest(ja, vcov = vcovHC(ja, type = "HC1", cluster = "group"))
coef_test(ja, vcov = "CR2", cluster = yes$Local.authority, test = "Satterthwaite")

did_model <- plm(
  children_in_care ~ Child.Tax.Credit * post + factor(time), # DiD interaction term with fixed effects
  data = final_data,
  index = c("Local.authority", "time"),     # Panel structure (unit and time identifiers)
  model = "within"             # Fixed effects model
)

# Print summary of the model
summary(did_model)

# Adjust for clustered standard errors
clustered_se <- coeftest(did_model, vcov = vcovHC(did_model, type = "HC1", cluster = "group"))
print(clustered_se)


####Diff-in-diff###


reg <- final_data%>%
  select(percent_impacted_did, Local.authority)%>%
  group_by(Local.authority)%>%
  dplyr::summarise(Universal.Credit_mean = mean(as.numeric(percent_impacted_did), na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::filter(Universal.Credit_mean!=0)%>%
  dplyr::select(Local.authority, Universal.Credit_mean)%>%
  dplyr::full_join(., final_data, by="Local.authority")
  

did_model <- plm(
  as.numeric(total_in_care_under_5) ~ Universal.Credit_mean * post + factor(time), # DiD interaction term with fixed effects
  data = reg,
  index = c("Local.authority", "time"),     # Panel structure (unit and time identifiers)
  model = "within"             # Fixed effects model
)

clustered_se <- coeftest(did_model, vcov = vcovHC(did_model, type = "HC1", cluster = "group"))
print(clustered_se)

summary(lm(as.numeric(started_over_16)~as.numeric(Universal.Credit_mean)*post, data = reg))


####figure####

two <- final_data%>%
  dplyr::filter(year>2017)%>%
  dplyr::mutate(percent_in_care = total_in_care_under_5/under_5_population*100)%>%
  select(percent_impacted, percent_in_care, Local.authority)%>%
  group_by(Local.authority)%>%
  dplyr::summarise(percent_impacted = mean(as.numeric(percent_impacted), na.rm=T),
                   percent_in_care = mean(as.numeric(percent_in_care), na.rm=T))%>%
  dplyr::ungroup()%>%
  ggplot(., aes(x=percent_impacted, y = percent_in_care))+
  geom_point()+
  theme_bw()+
  geom_smooth(method="lm")+
  labs(x="Percent of children born after 2017 impacted by two-child limit (%)",
       y= "Percent of children under 5 in care (%)",
       title="Two child limit impacts areas with lots of young children in care")

one <- final_data%>%
  dplyr::mutate(percent_in_care = total_in_care_under_5/under_5_population*100)%>%
  ggplot(., aes(x=year, y=as.numeric(percent_in_care)))+
  stat_summary(fun = mean)+
  geom_smooth(method="loess")+
  theme_bw()+
  labs(y="Percent of children under 5 in care (%)",
       title = "The number of young children in care started increasing in 2016")+
  scale_x_continuous(breaks=c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023))+
  geom_vline(xintercept = 2017.2, linetype = "dashed")+
  annotate("text", label = "Two-child limit introduced",
                     x = 2017, y = 0.58, size = 3, angle=90)


dfchange <- final_data %>%
  dplyr::mutate(percent_in_care = total_in_care_under_5/under_5_population*100)
  

dfchange17 <- dfchange[which(dfchange$year==2018),]
dfchange23 <- dfchange[which(dfchange$year==2023),]

dfchange17 <- dfchange17[c("Local.authority", "percent_impacted", "total_in_care_under_5")]
dfchange23 <- dfchange23[c("Local.authority", "percent_impacted", "total_in_care_under_5")]

names(dfchange17)[names(dfchange17)=="percent_impacted"] <- "percent_impacted_17"
names(dfchange17)[names(dfchange17)=="total_in_care_under_5"] <- "percent_in_care_17"

names(dfchange23)[names(dfchange23)=="percent_impacted"] <- "percent_impacted_23"
names(dfchange23)[names(dfchange23)=="total_in_care_under_5"] <- "percent_in_care_23"



dfchange <- merge(dfchange17, dfchange23, by="Local.authority", all=T)


dfchange$impactchange <- dfchange$percent_impacted_23-dfchange$percent_impacted_17
dfchange$carechange <- dfchange$percent_in_care_23-dfchange$percent_in_care_17

dfchange <- dfchange[complete.cases(dfchange),]

dfchange %>%
  full_join(., final_data, by=c("Local.authority"))%>%
  dplyr::filter(year==2023)%>%
  ggplot(.,aes(x=percent_impacted, y=carechange))+
  geom_point()+
  geom_smooth()



figure <- final_data%>%
  dplyr::filter(year>2017)%>%
  select(percent_impacted, Local.authority)%>%
  group_by(Local.authority)%>%
  dplyr::summarise(percent_impacted = mean(as.numeric(percent_impacted), na.rm=T))%>%
  dplyr::ungroup()


quantiles <- quantile(as.double(figure$percent_impacted), 
                      probs = seq(0, 1, length.out = 5), na.rm=T)


three <- figure %>%  dplyr::mutate(two_child_impact_quantiles =cut(percent_impacted, 
                                        breaks = quantiles, 
                                        labels = c(1,2,3,4),
                                        include.lowest = T) )%>%
  dplyr::select(Local.authority, two_child_impact_quantiles)%>%
  dplyr::full_join(., final_data, by="Local.authority")%>%
  dplyr::filter(!is.na(two_child_impact_quantiles))%>%
  dplyr::mutate(percent_in_care = total_in_care_under_5/under_5_population*100)%>%
  ggplot(., aes(x=as.numeric(year), y=as.numeric(percent_in_care),
                group = two_child_impact_quantiles,
                fill = two_child_impact_quantiles,
                colour = two_child_impact_quantiles))+
  geom_smooth(data = .%>%dplyr::filter(year<2018), method="lm")+
  geom_smooth(data = .%>%dplyr::filter(year>2016), method="lm")+
  geom_vline(xintercept = 2017)+
  theme_bw()+
  labs(x="Year", y="Children in care under 5 yrs old (%)",
       title = "Areas which were impacted most had largest increases in children in care since the reform",
       fill= "Quartile of two-child limit impact",
       colour= "Quartile of two-child limit impact",
       group= "Quartile of two-child limit impact"
  )+
  scale_x_continuous(breaks=c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022,2023))+
  theme(legend.position="bottom")
  
yes <- cowplot::plot_grid(one, two, three, ncol=1)

ggsave(plot=yes, filename="Library/CloudStorage/OneDrive-Nexus365/Documents/GitHub/Github_new/two_child_limit/Figures/descriptive.png", width=10, height=12, dpi=600)

#### check other ages you muppet ####

ages <- read.csv(curl("https://raw.githubusercontent.com/BenGoodair/childrens_social_care_data/main/Final_Data/outputs/dashboard_data.csv"))%>% 
  dplyr::filter(category=="child characteristic at 31st March",
                variable!="Total" &subcategory=="Age group",
                variable != "Total_during",
                variable != "Children who were only looked after exclusively under a series of short term placements") %>%
  dplyr::mutate(LA_Name = ifelse(LA_Name=="COUNTY DURHAM", "DURHAM", LA_Name))%>%
  dplyr::rename(Local.authority = LA_Name,
                pop = number)%>%
  dplyr::select(pop, year, variable)%>%
  dplyr::group_by(year, variable)%>%
  dplyr::summarise(pop = sum(as.numeric(pop), na.rm=T))%>%
  ungroup()

 ggplot(ages, aes(x=year, y = pop))+
  geom_area()+
  facet_grid(~variable)


figure <- final_data%>%
  dplyr::filter(year>2017)%>%
  select(Universal.Credit, Local.authority)%>%
  group_by(Local.authority)%>%
  dplyr::summarise(Universal.Credit = mean(as.numeric(Universal.Credit), na.rm=T))%>%
  dplyr::ungroup()


quantiles <- quantile(as.double(figure$Universal.Credit), 
                      probs = seq(0, 1, length.out = 4), na.rm=T)


figure %>%  dplyr::mutate(two_child_impact_quantiles =cut(Universal.Credit, 
                                                          breaks = quantiles, 
                                                          labels = c(1,2,3),
                                                          include.lowest = T) )%>%
  dplyr::select(Local.authority, two_child_impact_quantiles)%>%
  dplyr::full_join(., final_data, by="Local.authority")%>%
  dplyr::filter(!is.na(two_child_impact_quantiles))%>%
  ggplot(., aes(x=as.numeric(year), y=as.numeric(children_in_care),
                group = two_child_impact_quantiles,
                fill = two_child_impact_quantiles,
                colour = two_child_impact_quantiles))+
  geom_smooth(data = .%>%dplyr::filter(year<2018), method="lm")+
  geom_smooth(data = .%>%dplyr::filter(year>2016), method="lm")+
  geom_vline(xintercept = 2017)+
  theme_bw()+
  labs(x="Year", y="Children_in_care")








figure <- final_data%>%
  dplyr::filter(year>2017)%>%
  select(Proportion.Both, Local.authority)%>%
  group_by(Local.authority)%>%
  dplyr::summarise(Proportion.Both = mean(as.numeric(Proportion.Both), na.rm=T))%>%
  dplyr::ungroup()


quantiles <- quantile(as.double(figure$Proportion.Both), 
                      probs = seq(0, 1, length.out = 5), na.rm=T)

figure %>%  dplyr::mutate(two_child_impact_quantiles_percent =cut(Proportion.Both, 
                                                                 breaks = quantiles, 
                                                                 labels = c(1,2,3, 4),
                                                                 include.lowest = T) )%>%
  dplyr::select(Local.authority, two_child_impact_quantiles_percent)%>%
  dplyr::full_join(., final_data, by="Local.authority")%>%
  dplyr::filter(!is.na(two_child_impact_quantiles_percent))%>%
  ggplot(., aes(x=as.numeric(year), y=as.numeric(children_in_care),
                group = two_child_impact_quantiles_percent,
                fill = two_child_impact_quantiles_percent,
                colour = two_child_impact_quantiles_percent
                ))+
  geom_smooth(data = .%>%dplyr::filter(year<2018), method="lm")+
  geom_smooth(data = .%>%dplyr::filter(year>2016), method="lm")+
  geom_vline(xintercept = 2017)+
  theme_bw()+
  labs(x="Year", y="Children_in_care")


final_data%>%




