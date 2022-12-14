library(readr)
library(tidyverse)

### read data
list_of_files <- list.files(path = "data_september", recursive = TRUE, pattern = "\\.csv$", full.names = TRUE)
df <- readr::read_csv(list_of_files, id = "file_name")

### select only european countries and important variables: 
# - B0: ever had COVID19
# - B7:  Have you been tested for COVID19 in last days
# - V1 & V2: Have you had a vaccination & how many?
# - V15a: Have you an appointment for vaccination
# - V3a: If you have the chance to get a vaccination, would you choose it?
# - I5: Where have you received news in the last 7 days? Local health workers, scientist, who, governement/officials,
#        politicians, journalists, friends & family, religious leaders, none
# - I6: How much do you trust news sources?
# - E3: gender
# - V11: are you pregnant
# - E4: age
# - E8: highest level of education

df_eu <- df %>% 
  filter(survey_region == "EU") %>%
  select(B0, B7, B8a, V1, V2, V15a, V3a, I5_1, I5_2, I5_3, I5_4, I5_5, I5_6, I5_7, I5_8, I5_9, I6_1, I6_2, I6_3, 
         I6_4, I6_5, I6_6, I6_7, I6_8, E3, V11, E4, E8) %>%
  mutate_all(na_if, -99) %>%
  mutate_all(na_if, -88) %>%
  mutate_all(na_if, -77)








