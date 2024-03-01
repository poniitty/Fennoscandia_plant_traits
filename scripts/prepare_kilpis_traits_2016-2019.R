library("googlesheets4")
library(tidyverse)

# Community 2019

ss <- "https://docs.google.com/spreadsheets/d/1-OUSeHmoKxXTfDICF4hBSaFqbLdV4UB4cU3YG54UO_M/edit?pli=1#gid=0"
d <- read_sheet(ss, col_types = "c", col_names = FALSE)

d %>% mutate(...1 = gsub(" ","_",...1)) -> d

t(d) %>% as.data.frame() -> d

colnames(d) <- d[1,] %>% as.matrix %>% as.character() %>% make.unique(sep = "_MAKEUNIQUE")

d <- d[-1,]

d %>% filter(measurement == "coverage") %>% select(-measurement) -> cvr1

cvr1 %>% pivot_longer(cols = Equisetum_sylvaticum:ncol(.), names_to = "species", values_to = "cvr") %>% 
  pull(cvr) %>% unique()

cvr1 %>% 
  mutate(across(Equisetum_sylvaticum:ncol(.), ~gsub(",",".",.x))) %>% 
  mutate(across(Equisetum_sylvaticum:ncol(.), as.numeric)) %>% 
  rename(plot = Plot,
         setting = `Study_setting`,
         plot_type = `plot_size`) %>% 
  select(plot:Salix_polaris) %>% 
  pivot_longer(cols = Equisetum_sylvaticum:Salix_polaris,names_to = "species",values_to = "cvr") %>% 
  filter(cvr > 0) %>% 
  filter(!is.na(cvr)) -> cvr1

cvr1 <- cvr1 %>% 
  mutate(date = ymd(date)) %>% 
  mutate(species = gsub("_"," ", species))

####################################################
# Community 2019

ss <- "https://docs.google.com/spreadsheets/d/1A62lbh2N8hdw11dc-_6RgKPGwCafHwQaVLa2VZWdMsE/edit?pli=1#gid=0"
d <- read_sheet(ss, col_types = "c", col_names = FALSE)

d %>% mutate(...1 = gsub(" ","_",...1)) -> d

t(d) %>% as.data.frame() -> d

colnames(d) <- d[1,] %>% as.matrix %>% as.character() %>% make.unique(sep = "_MAKEUNIQUE")

d <- d[-1,]

d %>% filter(measurement == "coverage") %>% select(-measurement, -Kartoittaja) -> cvr2

cvr2 %>% pivot_longer(cols = Antennaria_alpina:ncol(.), names_to = "species", values_to = "cvr") %>% 
  pull(cvr) %>% unique()

cvr2 %>% 
  mutate(across(Antennaria_alpina:ncol(.), ~gsub(",",".",.x))) %>% 
  mutate(across(Antennaria_alpina:ncol(.), as.numeric)) %>% 
  rename(plot = Plot,
         setting = `Study_setting`,
         plot_type = `plot_size`) %>% 
  select(plot:Viola_palustris) %>% 
  pivot_longer(cols = Antennaria_alpina:Viola_palustris,names_to = "species",values_to = "cvr") %>% 
  filter(cvr > 0) %>% 
  filter(!is.na(cvr)) -> cvr2

cvr2 <- cvr2 %>% 
  mutate(date = dmy(date),
         plot = paste0("MI", plot)) %>% 
  mutate(species = gsub("_"," ", species))

###################################################################
# 2016 - 2017 woody plants mikkuna

ss <- "https://docs.google.com/spreadsheets/d/1d53-UZjPp_Qpi1q6cqwUAzfmN-U-pVr21U6WxL4xaQI/edit#gid=0"
d <- read_sheet(ss, col_types = "c", col_names = TRUE)

s17 <- as.character(c(388,11224,554,396,317,239,240,360,11225,747,11218,531,532,11217,422,11223,1186,903,862,11221,815,215,126,206,1043,11222,11219))

d <- d %>% 
  select(-sto_cov) %>% 
  pivot_longer(cols = BetnanC:ncol(.)) %>% 
  mutate(value = as.numeric(gsub(",",".",value))) %>% 
  filter(value > 0) %>% 
  mutate(species = substr(name, 1, 6),
         measurement = substr(name, 7, 7)) %>% 
  mutate(date = ymd(ifelse(site %in% s17, "2017-07-25", "2016-07-25"))) %>% 
  mutate(site = paste0("MI", site))

d %>% filter(measurement == "C") %>% rename(cvr = value) %>% select(-measurement, -name) -> cvr3
d %>% filter(measurement == "H") %>% rename(median_height = value) %>% select(-measurement, -name) -> medh3
d %>% filter(measurement == "M") %>% rename(max_height = value) %>% select(-measurement, -name) -> maxh3

cvr3 <- full_join(cvr3, medh3) %>% 
  full_join(., maxh3) %>% 
  mutate(plot = ifelse(plot == "C", "", plot),
         plot = paste0(site, plot)) %>% 
  mutate(setting = "KIL_WOO",
         plot_type = "b") %>% 
  select(plot, date, setting, plot_type, species, cvr, median_height, max_height) %>% 
  mutate(species = tolower(species))

# Fix names

nam <- read_csv2("env_data/all_species_abbr_edited_2.csv")

unique(cvr3$species)[!unique(cvr3$species) %in% nam$abbr]

cvr3 <- cvr3 %>% left_join(., nam, by = c("species" = "abbr")) %>% 
  select(-species) %>% rename(species = name) %>% 
  relocate(species, .after = plot_type)

#######################################################################
# 2019 wetland woodies

ss <- "https://docs.google.com/spreadsheets/d/1Bv6rKFHUg6wEN3xgvWiEXbY2OTHue_S8zHZsjzsHHaE/edit#gid=0"
d <- read_sheet(ss, col_types = "c", col_names = TRUE)

d <- d %>% 
  select(-sto_cov, -observer) %>% 
  pivot_longer(cols = BetnanC:ncol(.)) %>% 
  mutate(value = as.numeric(gsub(",",".",value))) %>% 
  filter(value > 0) %>% 
  mutate(species = substr(name, 1, 6),
         measurement = substr(name, 7, 7)) %>% 
  mutate(site = paste0("MI", site))

d %>% filter(measurement == "C") %>% rename(cvr = value) %>% select(-measurement, -name) -> cvr4
d %>% filter(measurement == "H") %>% rename(median_height = value) %>% select(-measurement, -name) -> medh4
d %>% filter(measurement == "M") %>% rename(max_height = value) %>% select(-measurement, -name) -> maxh4

cvr4 <- full_join(cvr4, medh4) %>% 
  full_join(., maxh4) %>% 
  mutate(plot = ifelse(plot == "C", "", plot),
         plot = paste0(site, plot)) %>% 
  mutate(setting = "KIL_WOO",
         plot_type = "b") %>% 
  select(plot, date, setting, plot_type, species, cvr, median_height, max_height) %>% 
  mutate(species = tolower(species))

# Fix names

nam <- read_csv2("env_data/all_species_abbr_edited_2.csv")

unique(cvr4$species)[!unique(cvr4$species) %in% nam$abbr]

cvr4 <- cvr4 %>% left_join(., nam, by = c("species" = "abbr")) %>% 
  select(-species) %>% rename(species = name) %>% 
  relocate(species, .after = plot_type) %>% 
  mutate(date = ymd(date))


######################################################
# Combine

all <- bind_rows(cvr1, 
                 cvr2, 
                 cvr3, 
                 cvr4) %>% 
  mutate(plot = toupper(plot)) %>% 
  mutate(species = gsub("_"," ",species))

unique(all$species)[!unique(all$species) %in% nam$name]

syns <- readxl::read_xlsx("env_data/synonyms.xlsx")

all <- all %>% 
  mutate(species = str_squish(species)) %>% 
  left_join(., syns) %>% 
  mutate(species = ifelse(!is.na(truename), truename, species)) %>% 
  select(-truename)

unique(all$species)[!unique(all$species) %in% nam$name]

all %>% 
  filter(is.na(species))

write_csv(all, "trait_data/kilpis_vascular_all_2016-2019.csv")
