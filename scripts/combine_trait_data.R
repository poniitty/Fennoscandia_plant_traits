
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
  mutate(site = ifelse(!is.na(as.numeric(site)), paste0("MI",site), site)) %>% 
  filter(!is.na(species)) %>% 
  filter(!is.na(n_inds)) -> dat

sort(unique(dat$site))
sort(unique(dat$species))

dat %>% mutate(site = ifelse(site == "RAX1", "RA047", site),
               site = ifelse(site == "RAX2", "RA048", site),
               site = ifelse(site == "KOODIII", "RA051", site)) -> dat

dat %>% mutate(study_design = "NONE") %>% 
  mutate(study_design = ifelse(grepl("^AIL", site), "AIL", study_design),
         study_design = ifelse(grepl("^MAL", site), "MAL", study_design),
         study_design = ifelse(grepl("^RA", site), "RA", study_design),
         study_design = ifelse(grepl("^MI", site), "MI", study_design),
         study_design = ifelse(grepl("^BA", site) | grepl("^BB", site), "BN", study_design),
         study_design = ifelse(grepl("^[a-zA-Z][0-9]", site), "ITV", study_design)) -> dat
  
dat %>% filter(study_design == "NONE")

dat <- dat %>% rename(abbr = species)

nam <- read_csv2("C:/Science/Summer2020/all_species_abbr_edited_2.csv", locale = readr::locale(encoding = "latin1")) %>% 
  rename(species = name) %>% 
  mutate(abbr = toupper(abbr))

unique(dat$abbr)[which(!unique(dat$abbr) %in% nam$abbr)]

dat <- left_join(dat, nam) %>% 
  relocate(site, species)
dat %>% pull(species) %>% unique() %>% sort()

# Leaf area data

library(data.table)

d2 <- fread("C:/Users/OMISTAJA/OneDrive - University of Helsinki/Kesä2021/kilpisjarvi/leaves/LA_summarised_kilpis_2021.csv", encoding = "Latin-1") %>% 
  mutate(site = toupper(site)) %>%
  mutate(species = toupper(species)) %>% 
  as_tibble() %>% 
  mutate(species = ifelse(species == "¨PHYCAE","PHYCAE",species))

sort(unique(d2$species))
sort(unique(d2$site))
d2 %>% filter(site == "607")

d2 %>% arrange(dir, site, species) %>% 
  filter(!duplicated(d2 %>% select(site, species), fromLast = T)) -> d2


d2 %>% mutate(site = ifelse(!is.na(as.numeric(site)), paste0("MI",site), site)) %>% 
  mutate(species = ifelse(species == "¨PHYCAE", "PHYCAE", species),
              species = ifelse(species == "GEMDRY", "GYMDRY", species)) %>% 
  mutate(species = ifelse(site == "MI1057" & species == "SAUALP", "HIERAA", species)) %>% 
  mutate(site = ifelse(site == "RAX1", "RA047", site),
         site = ifelse(site == "RAX2", "RA048", site),
         site = ifelse(site == "KOO", "RA051", site)) %>% 
  rename(abbr = species) -> d2
d2 %>% filter(!complete.cases(.))

# Combine

d1 <- full_join(dat, d2)

d1 %>% filter(is.na(n_inds)) %>% select(site, abbr, id, dir)
d1 %>% filter(is.na(dir)) %>% select(site, abbr, id, dir)

d1 %>% filter(site == "F6") %>% select(site, abbr, id, dir)

d1 %>% mutate(leaf_area = sum_la/n_leaf,
              SLA = sum_la/d_weight) %>% 
  rename(total.leaf.area = sum_la) %>% 
  select(site, species, abbr, study_design, n_inds, n_leaf, w_weight, d_weight, total.leaf.area, sd_la_filt, n, n_filt, leaf_area, SLA, LDMC) -> d1

#################################################################################
# Kilpisjärvi 2020 traits

d <- read_csv("trait_data/All_traits_2020.csv") %>%
  mutate(site = ifelse(!is.na(as.numeric(site)), paste0("MI",site), site)) %>% 
  mutate(study_design = "NONE") %>% 
  mutate(study_design = ifelse(grepl("^AIL", site), "AIL", study_design),
         study_design = ifelse(grepl("^MAL", site), "MAL", study_design),
         study_design = ifelse(grepl("^RA", site), "RA", study_design),
         study_design = ifelse(grepl("^MI", site), "MI", study_design),
         study_design = ifelse(grepl("^K", site), "ROS", study_design)) %>% 
  mutate(species = ifelse(species == "Anthoxantum odoratum", "Anthoxanthum odoratum", species),
         species = ifelse(species == "Carex rotundifolia", "Carex rotundata", species),
         species = ifelse(species == "Triophorum cespitosum", "Trichophorum cespitosum", species),
         species = ifelse(species == "Antennaria dioca", "Antennaria dioica", species),
         species = ifelse(species == "Erigeron uniflora", "Erigeron uniflorus", species),
         species = ifelse(species == "Taraxacum sp", "Taraxacum", species),
         species = ifelse(species == "Pedicular lapponica", "Pedicularis lapponica", species),
         species = ifelse(species == "Cirsium helenoides", "Cirsium helenioides", species),
         species = ifelse(species == "Betula pubescens chzerepanovii", "Betula pubescens subsp. czerepanovii", species),
         species = ifelse(species == "Melanpyrum pratense", "Melampyrum pratense", species),
         species = ifelse(species == "Salix phyllicifolia", "Salix phylicifolia", species))

nam <- fread("C:/Users/OMISTAJA/OneDrive - University of Helsinki/R_Projects/Fenno2018/Pekalta/NamesFromDatabasesFINAL.csv") %>%
  select(OldName, final)

unique(d$species)[which(!unique(d$species) %in% unique(nam$OldName))]

left_join(d, nam, by = c("species" = "OldName")) %>% 
  mutate(final = ifelse(is.na(final), species, final)) %>% 
  select(-species) %>% 
  rename(species = final) %>% 
  relocate(site, species) -> d

####################################################################
# Combine years

unique(d$species)[which(!unique(d$species) %in% unique(d1$species))]
unique(d1$species)[which(!unique(d1$species) %in% unique(d$species))]

d <- bind_rows(d %>% mutate(year = 2020), d1 %>% mutate(year = 2021)) %>% 
  relocate(year, .after = species) %>% 
  relocate(study_design, .after = site) %>% 
  relocate(abbr, .after = species)

sort(unique(d$site))

write_csv(d, "trait_data/Kilpis_combined_leaf_traits.csv")

# write_csv(d %>% filter(study_design == "ITV"), "trait_data/ITV_leaf_traits.csv")

# 2021 LEAF AREA FOR INDIVIDUAL LEAVES


d2 <- fread("C:/Users/OMISTAJA/OneDrive - University of Helsinki/Kesä2021/kilpisjarvi/leaves/LA_AllLeaves_kilpis_2021.csv", encoding = "Latin-1") %>% 
  mutate(site = toupper(site)) %>%
  mutate(species = toupper(species)) %>% 
  as_tibble() %>% 
  mutate(species = ifelse(species == "¨PHYCAE","PHYCAE",species))

sort(unique(d2$species))
sort(unique(d2$site))
d2 %>% filter(site == "607")

d2 %>% arrange(dir, site, species) %>% 
  select(-c(n:sd_la)) -> d2


d2 %>% mutate(site = ifelse(!is.na(as.numeric(site)), paste0("MI",site), site)) %>% 
  mutate(species = ifelse(species == "¨PHYCAE", "PHYCAE", species),
         species = ifelse(species == "GEMDRY", "GYMDRY", species)) %>% 
  mutate(species = ifelse(site == "MI1057" & species == "SAUALP", "HIERAA", species)) %>% 
  mutate(site = ifelse(site == "RAX1", "RA047", site),
         site = ifelse(site == "RAX2", "RA048", site),
         site = ifelse(site == "KOO", "RA051", site)) %>% 
  rename(abbr = species) -> d2

nam <- read_csv2("C:/Science/Summer2020/all_species_abbr_edited_2.csv", locale = readr::locale(encoding = "latin1")) %>% 
  rename(species = name) %>% 
  mutate(abbr = toupper(abbr))

unique(d2$abbr)[which(!unique(d2$abbr) %in% nam$abbr)]

d2 <- left_join(d2, nam) %>% 
  relocate(site, species) %>% 
  select(-id, -dir)

unique(d2$species) %>% sort()

nam <- fread("C:/Users/OMISTAJA/OneDrive - University of Helsinki/R_Projects/Fenno2018/Pekalta/NamesFromDatabasesFINAL.csv") %>%
  select(OldName, final)

unique(d2$species)[which(!unique(d2$species) %in% unique(nam$OldName))]

left_join(d2, nam, by = c("species" = "OldName")) %>% 
  mutate(final = ifelse(is.na(final), species, final)) %>% 
  select(-species) %>% 
  rename(species = final) %>% 
  relocate(site, species) -> d2

d2 %>% group_by(site, species) %>% mutate(leaf_id = row_number()) -> d2

write_csv(d2, "trait_data/Kilpis_all_leaf_areas.csv")


############################################################################################
# Värriö 2021 traits
############################################################################################

ss <- "https://docs.google.com/spreadsheets/d/1q8_wiw6igMWSMEA4dPdcPJn7SWfrLtjRoJgWC0ZBIcY/edit#gid=0"
dat <- read_sheet(ss)

add_zeros <- function(x){
  if(nchar(x) == 1){
    return(as.character(paste0("00",x)))
  }
  if(nchar(x) == 2){
    return(as.character(paste0("0",x)))
  }
  if(nchar(x) > 2){
    return(as.character(x))
  }
}

dat %>%
  mutate(species = toupper(species)) %>% 
  mutate(site = toupper(site)) %>% 
  mutate(w_weight = gsub(",",".",w_weight),
         d_weight = gsub(",",".",d_weight)) %>% 
  mutate(across(n_inds:d_weight, as.numeric)) %>% 
  mutate(LDMC = d_weight/w_weight) %>% 
  mutate(site = unlist(lapply(site, function(x) { add_zeros(x) }))) %>% 
  mutate(site = ifelse(!is.na(as.numeric(site)), paste0("VAR",site), site)) -> dat

sort(unique(dat$site))
sort(unique(dat$species))

dat <- dat %>% rename(abbr = species)

nam <- read_csv2("C:/Science/Summer2020/all_species_abbr_edited_2.csv", locale = readr::locale(encoding = "latin1")) %>% 
  rename(species = name) %>% 
  mutate(abbr = toupper(abbr))

unique(dat$abbr)[which(!unique(dat$abbr) %in% nam$abbr)]

dat <- left_join(dat, nam) %>% 
  relocate(site, species)

d <- bind_rows(d, dat %>% mutate(year = 2021)) %>% 
  select(-abbr) %>% 
  relocate(year, .after = species)

# Vindelfjällen

ss <- "https://docs.google.com/spreadsheets/d/1kP5xcUE_PafB_X7nXxqMn70fyu-mG93X7RfKkLS3dQI/edit#gid=0"
dat <- read_sheet(ss)

dat %>%
  mutate(species = toupper(species)) %>% 
  mutate(site = toupper(site)) %>% 
  mutate(w_weight = gsub(",",".",w_weight),
         d_weight = gsub(",",".",d_weight)) %>% 
  mutate(across(n_inds:d_weight, as.numeric)) %>% 
  mutate(LDMC = d_weight/w_weight) %>%  
  mutate(site = ifelse(!is.na(site), paste0("VIN_",site), site)) -> dat

sort(unique(dat$site))
sort(unique(dat$species))

dat <- dat %>% rename(abbr = species)

nam <- read_csv2("C:/Science/Summer2020/all_species_abbr_edited_2.csv", locale = readr::locale(encoding = "latin1")) %>% 
  rename(species = name) %>% 
  mutate(abbr = toupper(abbr))

unique(dat$abbr)[which(!unique(dat$abbr) %in% nam$abbr)]

dat <- left_join(dat, nam) %>% 
  relocate(site, species)

d <- bind_rows(d, dat %>% mutate(year = 2021)) %>% 
  select(-abbr) %>% 
  relocate(year, .after = species)



sort(unique(d$species))

rev(sort(table(d$species)))

d %>% filter(LDMC > 0.6)

d %>% group_by(species) %>% 
  summarise(mean = mean(LDMC, na.rm = T),
            sd = sd(LDMC, na.rm = T),
            n = n()) %>% 
  # filter(n > 3) %>%
  arrange(species) %>% as.data.frame()

write_csv(d, "trait_data/combined_traits.csv")

dd <- read_csv("trait_data/combined_traits.csv")

dd %>% 
  filter(!is.na(SLA)) %>% 
  pull(species) %>% 
  unique() %>% 
  sort()

dd %>% 
  filter(grepl("096", site))
