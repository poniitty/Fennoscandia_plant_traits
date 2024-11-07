# THIS CODE MASTERS THE TRAIT 2024 DATA FROM GOOGLE DRIVE

# install.packages("googlesheets4")

library("googlesheets4")
library(tidyverse)
library(lubridate)

gs4_deauth()
gs4_auth()

add_zeros <- function(x, area){
  xx <- unlist(lapply(x, function(x){
    if(nchar(x) == 1){
      return(as.character(paste0(area,"00",x)))
    }
    if(nchar(x) == 2){
      return(as.character(paste0(area,"0",x)))
    }
    if(nchar(x) > 2){
      return(as.character(paste0(area,x)))
    }
  }))
  return(xx)
}

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
  filter(startsWith(site, "OUL"))

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
d4 %>% filter(LDMC < 0.06)
d4 %>% filter(species == "Vaccinium vitis-idaea") %>% summary


############################################################################
# Height data

ss <- "https://docs.google.com/spreadsheets/d/1wG-IMb46fMwjlpkGVgWaGxPozUKFZ13YFEO5kEHZyxU/edit?gid=0#gid=0"
d <- read_sheet(ss, col_types = "c", col_names = FALSE) %>% 
  slice(c(1:5, 14:237))

d <- d[,-2]

d %>% mutate(...1 = gsub(" ","_",...1)) -> d
d %>% filter(!is.na(...1)) -> d

t(d) %>% as.data.frame() -> d

colnames(d) <- d[1,] %>% as.matrix %>% as.character() %>% make.unique(sep = "_MAKEUNIQUE")

d <- d[-1,]
d <- d %>% select(Plot:Ribes_spicatum_MAKEUNIQUE1) %>% 
  select(-c("Dicranum_scoparium","Hylocomnium_splendens","Pleurotzium_schreberi"))
d <- d %>% 
  mutate(across(everything(), ~gsub(",",".",.x)))

d <- d %>% 
  filter(!is.na(Plot)) %>% 
  mutate(Plot = add_zeros(Plot, "OUL"))

d %>% filter(measurement == "cov") %>% select(-measurement) -> cvr
d %>% filter(measurement == "medhei") %>% select(-measurement) %>% 
  filter(plot_size != "c") -> medh
d %>% filter(measurement == "maxhei") %>% select(-measurement) %>% 
  filter(plot_size != "c") -> maxh
d %>% filter(measurement == "rep") %>% select(-measurement) %>% 
  filter(plot_size == "c") -> flow

cvr %>% pivot_longer(cols = Betula_pubescens:ncol(.), names_to = "species", values_to = "cvr") %>% 
  pull(cvr) %>% unique()

medh %>% pivot_longer(cols = Betula_pubescens:ncol(.), names_to = "species", values_to = "medh") %>% 
  pull(medh) %>% unique()

medh %>% pivot_longer(cols = Betula_pubescens:ncol(.), names_to = "species", values_to = "medh") %>% 
  filter(medh == "")

medh %>% pivot_longer(cols = Betula_pubescens:ncol(.), names_to = "species", values_to = "medh") %>% 
  filter(!is.na(medh)) %>% 
  pull(species) %>% unique()

maxh %>% pivot_longer(cols = Betula_pubescens:ncol(.), names_to = "species", values_to = "maxh") %>% 
  pull(maxh) %>% unique()

maxh %>% pivot_longer(cols = Betula_pubescens:ncol(.), names_to = "species", values_to = "maxh") %>% 
  filter(maxh == "")

flow %>% pivot_longer(cols = Betula_pubescens:ncol(.), names_to = "species", values_to = "flow") %>% 
  pull(flow) %>% unique()

flow %>% pivot_longer(cols = Betula_pubescens:ncol(.), names_to = "species", values_to = "flow") %>% 
  filter(flow == "")
flow %>% pivot_longer(cols = Betula_pubescens:ncol(.), names_to = "species", values_to = "flow") %>% 
  filter(flow == "-")
flow <- flow %>% 
  mutate(across(everything(), ~gsub("-",NA,.x)))

flow %>% pivot_longer(cols = Betula_pubescens:ncol(.), names_to = "species", values_to = "flow") %>% 
  filter(!is.na(flow)) %>% 
  pull(species) %>% unique()


# If everything seems ok
medh %>% pivot_longer(cols = Betula_pubescens:ncol(.), names_to = "species", values_to = "median_height") %>% 
  filter(!is.na(median_height)) %>% 
  mutate(median_height = as.numeric(gsub(",",".",median_height))) -> medh

maxh %>% pivot_longer(cols = Betula_pubescens:ncol(.), names_to = "species", values_to = "max_height") %>% 
  filter(!is.na(max_height)) %>% 
  mutate(max_height = as.numeric(gsub(",",".",max_height))) -> maxh

flow %>% pivot_longer(cols = Betula_pubescens:ncol(.), names_to = "species", values_to = "reproduction") %>% 
  filter(!is.na(reproduction)) %>% 
  mutate(reproduction = as.numeric(gsub(",",".",reproduction))) -> flow

medh %>% filter(duplicated(medh %>% select(Plot, Study_setting, plot_size, species)))

full_join(medh, maxh) %>% 
  mutate(median_height = as.numeric(median_height),
         max_height = as.numeric(max_height)) %>% 
  filter(!complete.cases(.))

full_join(medh, maxh) %>% 
  mutate(median_height = as.numeric(median_height),
         max_height = as.numeric(max_height)) %>% 
  filter(median_height > max_height)

full_join(medh, maxh) %>% 
  mutate(median_height = as.numeric(median_height),
         max_height = as.numeric(max_height)) %>% 
  mutate(reldiff = max_height/median_height) %>% 
  arrange(desc(reldiff)) %>% as.data.frame()

full_join(medh, maxh) %>% 
  mutate(median_height = as.numeric(median_height),
         max_height = as.numeric(max_height)) %>% 
  group_by(species) %>% 
  arrange(desc(max_height)) %>% 
  slice_head(n = 1) %>% as.data.frame()


# Final filterings

flow %>% mutate(reproduction = as.numeric(reproduction)) %>% 
  filter(complete.cases(.)) %>% 
  rename(plot = Plot,
         setting = `Study_setting`) %>% 
  select(-plot_size) -> flow

full_join(medh, maxh) %>% 
  mutate(median_height = as.numeric(median_height),
         max_height = as.numeric(max_height)) %>% 
  filter(complete.cases(.)) %>% 
  rename(plot = Plot,
         setting = `Study_setting`,
         plot_type = `plot_size`) -> heights

cvr %>% 
  mutate(across(Betula_pubescens:ncol(.), ~gsub(",",".",.x))) %>% 
  mutate(across(Betula_pubescens:ncol(.), as.numeric)) %>% 
  rename(plot = Plot,
         setting = `Study_setting`,
         plot_type = `plot_size`) %>% 
  select(plot:Ribes_spicatum_MAKEUNIQUE1) %>% 
  pivot_longer(cols = Betula_pubescens:Ribes_spicatum_MAKEUNIQUE1,names_to = "species",values_to = "cvr") %>% 
  filter(cvr > 0) %>% 
  filter(!is.na(cvr)) -> cvr

all <- cvr %>% 
  full_join(., heights) %>% 
  full_join(., flow) %>% 
  mutate(plot = toupper(plot)) %>% 
  mutate(species = gsub("MAKEUNIQUE","",species)) %>% 
  mutate(species = gsub("[[:digit:]]","",species)) %>% 
  mutate(species = gsub("_"," ",species))

unique(all$species)[!unique(all$species) %in% nam$name]

syns <- readxl::read_xlsx("env_data/synonyms.xlsx")

all <- all %>% 
  mutate(species = str_squish(species)) %>% 
  left_join(., syns) %>% 
  mutate(species = ifelse(!is.na(truename), truename, species)) %>% 
  select(-truename)

unique(all$species)[!unique(all$species) %in% nam$name]

right_join(all %>% rename(site = plot) %>% rename(date2 = date) %>% mutate(date2 = dmy(date2)),
           d4) %>% 
  filter(is.na(cvr)) %>% as.data.frame()
right_join(all %>% rename(site = plot) %>% rename(date2 = date) %>% mutate(date2 = dmy(date2)),
           d4) %>% 
  filter(is.na(n_inds)) %>% as.data.frame()

full_join(all %>% rename(site = plot) %>% rename(date2 = date) %>% mutate(date2 = dmy(date2)) %>% filter(plot_type == "b"),
          d4) %>% filter(cvr > 1) %>% 
  select(-reproduction) %>% 
  filter(!complete.cases(.)) %>%
  group_by(site) %>% 
  count() %>% arrange(desc(n)) %>% as.data.frame()

full_join(all %>% rename(site = plot) %>% rename(date2 = date) %>% mutate(date2 = dmy(date2)) %>% filter(plot_type == "b"),
          d4) %>% filter(cvr > 1) %>% filter(site == "OUL041")

unique(all$plot)[!unique(all$plot) %in% d4$site]
unique(d4$site)[!unique(d4$site) %in% all$plot]

allall <- full_join(all %>% rename(site = plot) %>% mutate(date = dmy(date)),
                    d4 %>% rename(date2 = date)) %>% 
  mutate(date = as_date(ifelse(is.na(date), date2, date))) %>% 
  relocate(date, .after = plot_type) %>% select(-date2) %>% 
  rename(n_leaf_image = n) %>%
  filter(!is.na(species))

unique(allall$site)

allall <- allall %>% 
  select(-id)

write_csv(allall, "trait_data/oulanka_vascular_all_2024.csv")
