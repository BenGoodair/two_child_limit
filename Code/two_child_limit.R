if (!require("pacman")) install.packages("pacman")

pacman::p_load(devtools,lmtest, dplyr,pdftools, tidyverse,rattle,glmnet,caret, rpart.plot, RcolorBrewer,rpart, tidyr, mice, stringr,randomForest,  curl, plm, readxl, zoo, stringr, patchwork,  sf, clubSandwich, modelsummary, sjPlot)


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
  dplyr::filter(!is.na(Local.authority),
                !is.na(children_in_care),
                Local.authority!="DORSET",
                Local.authority!="LONDON")%>%
  dplyr::mutate(Child.Tax.Credit = Child.Tax.Credit%>% gsub(",","",.))%>%
  dplyr::mutate(Child.Tax.Credit = as.numeric(ifelse(year<2018, 0, Child.Tax.Credit)),
                post = ifelse(year<2018, "0", "1"),
                time = factor(year),
                children_in_care = as.numeric(children_in_care))


summary(lm(as.numeric(children_in_care)~as.numeric(Child.Tax.Credit)*time, data = final_data))

yes <- pdata.frame(final_data, index = c("Local.authority","year"))

summary(plm(as.numeric(children_in_care)~ as.numeric(Universal.Credit), data=yes, effect = "twoway", model = "within"))


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
  select(Universal.Credit, Local.authority)%>%
  group_by(Local.authority)%>%
  dplyr::summarise(Universal.Credit_mean = mean(as.numeric(Universal.Credit), na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::filter(Universal.Credit_mean!=0)%>%
  dplyr::select(Local.authority, Universal.Credit_mean)%>%
  dplyr::full_join(., final_data, by="Local.authority")
  

did_model <- plm(
  children_in_care ~ Universal.Credit_mean * post + factor(time), # DiD interaction term with fixed effects
  data = reg,
  index = c("Local.authority", "time"),     # Panel structure (unit and time identifiers)
  model = "within"             # Fixed effects model
)

clustered_se <- coeftest(did_model, vcov = vcovHC(did_model, type = "HC1", cluster = "group"))
print(clustered_se)

summary(lm(as.numeric(children_in_care)~as.numeric(Universal.Credit_mean)*post, data = reg))


####figure####


figure <- final_data%>%
  dplyr::filter(year>2017)%>%
  select(Child.Tax.Credit, Local.authority)%>%
  group_by(Local.authority)%>%
  dplyr::summarise(Child.Tax.Credit = mean(as.numeric(Child.Tax.Credit), na.rm=T))%>%
  dplyr::ungroup()


quantiles <- quantile(as.double(figure$Child.Tax.Credit), 
                      probs = seq(0, 1, length.out = 4), na.rm=T)


figure %>%  dplyr::mutate(two_child_impact_quantiles =cut(Child.Tax.Credit, 
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









