# THIS CODE MASTERS THE TRAIT 2021 DATA FROM GOOGLE DRIVE

# install.packages("googlesheets4")

library("googlesheets4")
library(tidyverse)


gs4_deauth()

ss <- "https://docs.google.com/spreadsheets/d/1rmYfuVk5Fqx8r9D9S1Mhp3juU3PpL6R6O_OEl6iG7aA/edit#gid=0"
dat <- read_sheet(ss)


dat %>%
  select(-KUKIMEMO) %>% 
  mutate(species = toupper(species)) %>% 
  mutate(site = toupper(site)) %>% 
  mutate(w_weight = gsub(",",".",w_weight),
         d_weight = gsub(",",".",d_weight)) %>% 
  mutate(across(n_inds:d_weight, as.numeric)) %>% 
  mutate(LDMC = d_weight/w_weight) %>% 
  mutate(site = ifelse(grepl("K",site) | grepl("MAL",site) | grepl("RA",site) | grepl("AIL",site),
                       site, paste0("MI",site))) -> dat

sort(unique(dat$species))

dat <- dat %>% rename(abbr = species)

nam <- read_csv2("C:/Science/Summer2020/all_species_abbr_edited_2.csv", locale = readr::locale(encoding = "latin1")) %>% 
  rename(species = name) %>% 
  mutate(abbr = toupper(abbr))

dat <- left_join(dat, nam) %>% 
  relocate(site, species)

d <- read_csv("trait_data/All_traits_2020.csv") %>%
  mutate(site = ifelse(grepl("NONE",site) | grepl("K",site) | grepl("MAL",site) | grepl("RA",site) | grepl("AIL",site),
                       site, paste0("MI",site)))

d <- bind_rows(d %>% mutate(year = 2020), dat %>% mutate(year = 2021)) %>% 
  select(-abbr) %>% 
  relocate(year, .after = species)

sort(unique(d$species))

rev(sort(table(d$species)))

