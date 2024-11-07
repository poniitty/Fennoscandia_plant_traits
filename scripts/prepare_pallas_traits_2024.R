# THIS CODE MASTERS THE TRAIT 2022 DATA FROM GOOGLE DRIVE

# install.packages("googlesheets4")

library("googlesheets4")
library(tidyverse)
library(lubridate)

######################
# Leaf area

la <- read_csv("C:/Users/poniitty/OneDrive - University of Helsinki/Kesä2024/LA_AllLeaves_kilpis_2024.csv")

la <- la %>% 
  filter(!(mean_la > 0.1 & Area < 0.01))

laa <- la %>% 
  select(id, Area) %>% 
  group_by(id) %>% 
  summarise(n = n(),
            area_sum = sum(Area),
            area_sd = sd(Area))

# Other leaf traits

ss <- "https://docs.google.com/spreadsheets/d/1-VdHu4DI2OdRH475TNJaMYIVssCswODLbswZsKCQp8Q/edit?gid=0#gid=0"
d1 <- read_sheet(ss, col_types = "c") %>% 
  select(-KUKIMEMO,-date_prosessed) %>% 
  mutate(site = toupper(site),
         species = tolower(species)) %>% 
  filter(!is.na(species)) %>% 
  mutate(w_weight = gsub(",",".",w_weight),
         d_weight = gsub(",",".",d_weight)) %>% 
  mutate(date = dmy(date_sampled)) %>% 
  select(-date_sampled) %>% 
  filter(startsWith(site, "PAL"))

nam <- read_csv2("env_data/all_species_abbr_edited_2.csv")

unique(d1$species)[!unique(d1$species) %in% nam$abbr]
unique(d1$site)

d1 %>% filter(duplicated(d1 %>% select(site, species)))

d1 %>% filter(n_leaf == 0)
d1 %>% filter(n_inds == 0)
unique(d1$n_inds)
unique(d1$n_leaf)

table(d1$species) %>% sort

# d1 %>% 
#   filter(n_inds > 0) %>% 
#   mutate(n_leaf = ifelse(n_leaf == 0, NA, n_leaf)) -> d1

head(d1)
head(laa)

unique(d1$id)[!unique(d1$id) %in% laa$id]
unique(laa$id)[!unique(laa$id) %in% unique(d1$id)]

# Fix names

d1 <- d1 %>% left_join(., nam, by = c("species" = "abbr")) %>% 
  select(-species) %>% rename(species = name) %>% 
  relocate(species, .after = id) %>% 
  mutate(across(c(id, n_inds, n_leaf, w_weight, d_weight), as.numeric))

d4 <- left_join(d1, laa) %>% arrange(site, species)
d4 %>% filter(!complete.cases(d4 %>% select(-area_sd)))

d4 %>% filter(duplicated(d4 %>% select(site, species, date)))

d4 %>% filter(d_weight > w_weight)

d4 %>% mutate(leaf_area = area_sum/n_leaf,
              SLA = area_sum/d_weight,
              LDMC = d_weight/w_weight) -> d4

d4 %>% mutate(diff = n_leaf-n) %>% 
  arrange(desc(diff))
d4 %>% mutate(diff = n_leaf-n) %>% 
  arrange(diff)

summary(d4)

d4 %>% filter(LDMC > 0.7)
d4 %>% filter(LDMC < 0.05)
d4 %>% filter(species == "Vaccinium vitis-idaea") %>% summary

##########################

all <- d4 %>% 
  mutate(plot_type = "b") %>% 
  relocate(plot_type, .after = site) %>% 
  rename(n_leaf_image = n) %>%
  filter(!is.na(species))

unique(all$site)

all <- all %>% 
  select(-id)

unique(all$species)[!unique(all$species) %in% nam$name]

syns <- readxl::read_xlsx("env_data/synonyms.xlsx")

all <- all %>% 
  mutate(species = str_squish(species)) %>% 
  left_join(., syns) %>% 
  mutate(species = ifelse(!is.na(truename), truename, species)) %>% 
  select(-truename)

unique(all$species)[!unique(all$species) %in% nam$name]

write_csv(all, "trait_data/pallas_vascular_all_2024.csv")
