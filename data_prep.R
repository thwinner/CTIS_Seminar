library(readr)
library(tidyverse)
library(dplyr)

##########################################################################################################################
################ Read data & get one september dataaset
list_of_files <- list.files(path = "data_september", recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)
df <- read_csv(list_of_files, id = "file_name")
df$RecordedDate <- as.character(df$RecordedDate)

# 1st September has more variables (298) than the rest of September -> read them separately and put them together
first_sept <- read.csv("2021-09-01_full.csv")
df <- bind_rows(first_sept, df)

# Save september data as new file
library(data.table)
fwrite(df, "september_dt.csv")

#########################################################################################################
############# Read and prepare september data
dt_sept <- read.csv("september_dt.csv")

### Prepare Country codes
country <- read.csv("CTIS_survey_country_region_map_table_ver1.083021.csv")

# Assign UN-Geoscheme to countries (https://en.wikipedia.org/wiki/United_Nations_geoscheme_for_Europe) (without Russia as it's mostly in Asia)
country_eu <- country %>%
  mutate(europe_part = case_when(country_region %in% c("Belarus", "Bulgaria", "Czech Republic", "Hungary", "Poland", "Moldova", "Romania",
                                                       "Slovakia", "Ukraine") ~ "East", 
                                 country_region %in% c("Aland Islands", "Denmark", "Estonia", "Faroe Islands", "Finland", "Iceland", "Ireland", "Isle Of Man", "Latvia", "Lithuania", 
                                                       "Norway", "Svalbard and Jan Mayen", "Sweden", "United Kingdom") ~ "North",
                                 country_region %in% c("Albania", "Andorra", "Bosnia and Herzegovina", "Croatia", "Gibraltar", "Greece", "Italy", "Vatican City",
                                                       "Malta", "Montenegro", "Republic of North Macedonia", "Portugal", "San Marino", "Serbia", "Slovenia", "Spain") ~ "South",
                                 country_region %in% c("Austria", "Belgium", "France", "Germany", "Liechtenstein", "Luxembourg", "Monaco", "Netherlands", "Switzerland") ~ "West")) %>%
  filter(!is.na(europe_part))
country_eu <- country_eu[, c("country_region_numeric", "country_region", "europe_part")]
country_num <- country_eu$country_region_numeric

###### Data preparation
### Select only countries in the EU and important variables: 
# - B0: ever had COVID19
# - B7:  Have you been tested for COVID19 in last days
# - V1 & V2: Have you had a vaccination & how many?
# - V15a: Have you an appointment for vaccination
# - V3a: If you have the chance to get a vaccination, would you choose it?
# - V5c: Reasons that you only probably would choose to get a Covid vaccination
# - C0a: Past 24 hours have you done anything of the following
# - G1: How much do you worry about getting COVID19
# - H3: How many friends and family have gotten a COVID vaccination
# - I5: Where have you received news in the last 7 days? Local health workers, scientist, who, governement/officials,
#        politicians, journalists, friends & family, religious leaders, none
# - I6: How much do you trust news sources?
# - E3: gender
# - V11: are you pregnant
# - E4: age
# - E8: highest level of education
# - E2: area (city, town, village)


dt_sept_eu <- dt_sept %>% 
  filter(A2_2_1 %in% country_num) %>%
  dplyr::select(survey_region, A2_2_1, B0, B7, B8a, V1, V2, V15a, V3a, V5c_1, V5c_2, V5c_3, V5c_4, V5c_5, V5c_6, V5c_7, 
         V5c_8, V5c_9, V5c_10, C0a_1, C0a_2, C0a_3, C0a_4, C0a_5, C0a_6, C0a_7, G1, H3, I5_1, I5_2, I5_3, I5_4, I5_5, I5_6, I5_7, I5_8, I5_9, I6_1, I6_2, I6_3, 
         I6_4, I6_5, I6_6, I6_7, I6_8, E3, V11, E4, E8, E2) %>%
  mutate_all(na_if, -99) %>%
  mutate_all(na_if, -88) %>%
  mutate_all(na_if, -77)

# change Vaccination-Variable to a binary variable with 0 (No) and 1 (Yes)
dt_sept_eu$V1[dt_sept_eu$V1 == 3] <- NA
dt_sept_eu$V1[dt_sept_eu$V1 == 2] <- 0

# People who didn't want to say which gender they are: NA
dt_sept_eu$E3[dt_sept_eu$E3 == 4] <- NA


## assign the geoscheme of europe to september dataset
names(dt_sept_eu)[names(dt_sept_eu) == "A2_2_1"] <- "country_region_numeric"
dt_sept_eu <- merge(dt_sept_eu, country_eu, by = "country_region_numeric")

## group variables in (binary) variables for regression models:
# - one age variable with three groups and one with two
# - gender in male and female
# - education in school- and university education
# - vaccinationrate in friendsgroup in two groups: no or few friends are vaccinated and some, most and all
# - worried about COVID in yes and no
# - area in urban and rural
# - all trust variables in trust and no trust

dt_sept_eu <- dt_sept_eu %>%
  mutate(age_grouped = case_when(E4 %in% c(1, 2) ~ "1", E4 %in% c(3, 4, 5) ~ "2", E4 %in% c(6, 7) ~ "3")) %>%
  mutate(age_dummy_grouped = case_when(E4 %in% c(1, 2, 3, 4) ~ "0", E4 %in% c(5, 6, 7) ~ "1")) %>%
  mutate(gender_grouped = case_when(E3 %in% c(1) ~ "0", E3 %in% c(2) ~ "1", E3 %in% c(3) ~ NA_character_)) %>%
  mutate(education_grouped = case_when(E8 %in% c(1, 2, 3, 4, 5) ~ "0", E8 %in% c(6, 7) ~ "1")) %>%
  mutate(vacc_friends_grouped = case_when(H3 %in% c(1, 2) ~ "0", H3 %in% c(3, 4, 5) ~ "1")) %>%
  mutate(worry_grouped = case_when(G1 %in% c(3, 4) ~ "0", G1 %in% c(1, 2) ~ "1")) %>%
  mutate(area_grouped = case_when(E2 %in% c(1, 2) ~ "0", E2 %in% c(3) ~ "1")) %>%
  mutate(trust_loc_group = case_when(I6_1 %in% c(1) ~ "0", I6_1 %in% c(2, 3) ~ "1")) %>%
  mutate(trust_science_group = case_when(I6_2 %in% c(1) ~ "0", I6_2 %in% c(2, 3) ~ "1")) %>%
  mutate(trust_who_group = case_when(I6_3 %in% c(1) ~ "0", I6_3 %in% c(2, 3) ~ "1")) %>%
  mutate(trust_gov_group = case_when(I6_4 %in% c(1) ~ "0", I6_4 %in% c(2, 3) ~ "1")) %>%
  mutate(trust_pol_group = case_when(I6_5 %in% c(1) ~ "0", I6_5 %in% c(2, 3) ~ "1")) %>%
  mutate(trust_journalist_group = case_when(I6_6 %in% c(1) ~ "0", I6_6 %in% c(2, 3) ~ "1")) %>%
  mutate(trust_fam_group = case_when(I6_7 %in% c(1) ~ "0", I6_7 %in% c(2, 3) ~ "1")) %>%
  mutate(trust_religious_group = case_when(I6_8 %in% c(1) ~ "0", I6_8 %in% c(2, 3) ~ "1")) %>%
  mutate(B0 = case_when(B0 %in% c(1) ~ "1", I6_8 %in% c(2) ~ "0")) %>%
  mutate(V2 = case_when(V2 %in% c(1) ~ "0", V2 %in% c(2) ~ "1", V2 %in% c(3) ~ NA_character_))



## Rename variables
dt_sept_eu <- dt_sept_eu %>%
  rename( "cov_inf" =  B0, "test_cov" = B7, "vacc" = V1,"numb_vacc" = V2, 
          "appointment_vacc" = V15a, "worry_cov" = G1, "vacc_friends" = H3,
          "news_loc" = I5_1, "news_science" = I5_2, "news_who" = I5_3, "news_gov" = I5_4,
          "news_pol" = I5_5, "news_journalist" = I5_6, "news_fam" = I5_7, "news_religious" = I5_8,
          "news_none" = I5_9, "trust_loc" = I6_1, "trust_science" = I6_2, "trust_who" = I6_3, "trust_gov" = I6_4,
          "trust_pol" = I6_5, "trust_journalist" = I6_6, "trust_fam" = I6_7, "trust_religious" = I6_8,
          "gender" = E3, "pregnant" = V11, "age" = E4, "education" = E8, "area" = E2)

## Rename levels of factor variables
# levels(dt_eu$trust_loc) <- c("No trust", "Somewhat trust", "Trust")
# levels(dt_eu$trust_science) <- c("No trust", "Somewhat trust", "Trust")
# levels(dt_eu$trust_who) <- c("No trust", "Somewhat trust", "Trust")
# levels(dt_eu$trust_gov) <- c("No trust", "Somewhat trust", "Trust")
# levels(dt_eu$trust_pol) <- c("No trust", "Somewhat trust", "Trust")
# levels(dt_eu$trust_journalist) <- c("No trust", "Somewhat trust", "Trust")
# levels(dt_eu$trust_fam) <- c("No trust", "Somewhat trust", "Trust")
# levels(dt_eu$trust_fam) <- c("No trust", "Somewhat trust", "Trust")
# levels(dt_eu$area) <- c("City", "Town", "Village")
# levels(dt_eu$cov_inf) <- c("Yes", "No")
# levels(dt_eu$worry_cov) <- c("Very much", "Much", "A little", "Not at all")
# levels(dt_eu$vacc_friends) <- c("None", "Few", "Some", "Most", "All")
# dt_eu$vacc_friendsGroup <- factor(dt_eu$vacc_friendsGroup, levels = c("Few", "Some", "Most"))
# levels(dt_eu$gender) <- c("Male", "Female", "Other", "No answer")
# 

# Factorize categorical variables
dt_sept_eu$age_grouped <- as.factor(dt_sept_eu$age_grouped)
# levels(dt_eu$europe_part) <- c("East", "North", "South", "West")
dt_sept_eu$area <- as.factor(dt_sept_eu$area)
dt_sept_eu$trust_loc <- as.factor(dt_sept_eu$trust_loc)
dt_sept_eu$trust_science <- as.factor(dt_sept_eu$trust_science)
dt_sept_eu$trust_who <- as.factor(dt_sept_eu$trust_who)
dt_sept_eu$trust_gov <- as.factor(dt_sept_eu$trust_gov)
dt_sept_eu$trust_pol <- as.factor(dt_sept_eu$trust_pol)
dt_sept_eu$trust_journalist <- as.factor(dt_sept_eu$trust_journalist)
dt_sept_eu$trust_fam <- as.factor(dt_sept_eu$trust_fam)
dt_sept_eu$trust_religious <- as.factor(dt_sept_eu$trust_religious)

dt_sept_eu$trust_loc_group <- as.factor(dt_sept_eu$trust_loc_group)
dt_sept_eu$trust_science_group <- as.factor(dt_sept_eu$trust_science_group)
dt_sept_eu$trust_who_group <- as.factor(dt_sept_eu$trust_who_group)
dt_sept_eu$trust_gov_group <- as.factor(dt_sept_eu$trust_gov_group)
dt_sept_eu$trust_pol_group <- as.factor(dt_sept_eu$trust_pol_group)
dt_sept_eu$trust_journalist_group <- as.factor(dt_sept_eu$trust_journalist_group)
dt_sept_eu$trust_fam_group <- as.factor(dt_sept_eu$trust_fam_group)
dt_sept_eu$trust_religious_group <- as.factor(dt_sept_eu$trust_religious_group)
dt_sept_eu$age_dummy_grouped <- as.factor(dt_sept_eu$age_dummy_grouped)
dt_sept_eu$gender_grouped <- as.factor(dt_sept_eu$gender_grouped)
dt_sept_eu$area_grouped <- as.factor(dt_sept_eu$area_grouped)
dt_sept_eu$education_grouped <- as.factor(dt_sept_eu$education_grouped)
dt_sept_eu$cov_inf <- as.factor(dt_sept_eu$cov_inf)
dt_sept_eu$vacc_friends_grouped <- as.factor(dt_sept_eu$vacc_friends_grouped)

dt_sept_eu$news_loc <- as.factor(dt_sept_eu$news_loc)
dt_sept_eu$news_science <- as.factor(dt_sept_eu$news_science)
dt_sept_eu$news_who <- as.factor(dt_sept_eu$news_who)
dt_sept_eu$news_gov <- as.factor(dt_sept_eu$news_gov)
dt_sept_eu$news_pol <- as.factor(dt_sept_eu$news_pol)
dt_sept_eu$news_journalist <- as.factor(dt_sept_eu$news_journalist)
dt_sept_eu$news_fam <- as.factor(dt_sept_eu$news_fam)
dt_sept_eu$news_religious <- as.factor(dt_sept_eu$news_religious)
dt_sept_eu$news_none <- as.factor(dt_sept_eu$news_none)



