# THIS CODE MASTERS THE TRAIT 2022 DATA FROM GOOGLE DRIVE

# install.packages("googlesheets4")

library("googlesheets4")
library(tidyverse)
library(lubridate)

gs4_deauth()

# Leaf area

la <- read_csv("C:/Users/poniitty/OneDrive - University of Helsinki/Kesä2022/leaf_scans/LA_AllLeaves_kilpis_2022.csv") %>% 
  mutate(site = ifelse(site == "SM1112","SM11/12",site)) %>% 
  mutate(site = ifelse(site == "SM78","SM7/8",site)) %>% 
  mutate(site = ifelse(site == "SM910","SM9/10",site)) %>% 
  mutate(species = ifelse(species == "psetra","psestr",species)) %>% 
  mutate(species = ifelse(species == "carcvag","carvag",species)) %>% 
  mutate(species = ifelse(species == "caraparal","carparal",species)) %>% 
  mutate(site = ifelse(site == "EXT" & species == "cargla" & dir == "LA-2022_07_11","EXT1",site)) %>% 
  mutate(site = ifelse(site == "EXT" & species == "carpar" & dir == "LA-2022_07_11","EXT1",site)) %>% 
  mutate(site = ifelse(site == "EXT" & species == "melpra" & dir == "LA-2022_07_11","EXT1",site)) %>% 
  mutate(site = ifelse(site == "EXT" & species == "ribrub" & dir == "LA-2022_07_26","EXT1",site)) %>% 
  mutate(species = ifelse(id == "L10_vacmyr_0001.jpg.txt","vaculi",species)) %>% 
  mutate(species = ifelse(site == "1069" & species == "empnig","phycae",species)) %>% 
  mutate(species = ifelse(site == "11227" & species == "carros","carrot",species))

uus <- la %>% filter(dir == "LA-UUSIX")
laaa <- la %>% filter(dir != "LA-UUSIX")
la <- la %>% filter(dir != "LA-UUSIX")

for(i in unique(uus$id)){
  
  if((la %>% filter(id == i) %>% nrow) == 0){
    
    if(mean(uus %>% filter(id == i) %>% pull(mean_la)) > 0.1){
      temp <- uus %>% filter(id == i) %>% filter(Area > 0.01)
    } else {
      temp <- uus %>% filter(id == i)
    }
    
    dirid <- la %>% filter(site == temp$site[1]) %>% group_by(dir) %>% count %>% arrange(desc(n)) %>% slice_head(n = 1) %>% pull(dir)
    
    la <- bind_rows(la,
                    temp %>% mutate(dir = dirid))
    
  } else {
    
    if(mean(uus %>% filter(id == i) %>% pull(mean_la)) > 0.1){
      temp <- uus %>% filter(id == i) %>% filter(Area > 0.01)
    } else {
      temp <- uus %>% filter(id == i)
    }
    la <- la %>% filter(id != i)
    dirid <- laaa %>% filter(site == temp$site[1] & species == temp$species[1]) %>% group_by(dir) %>% count %>% arrange(desc(n)) %>% slice_head(n = 1) %>% pull(dir)
    
    la <- bind_rows(la,
                    temp %>% mutate(dir = dirid))
  }
  
}

la$date <- ymd(as.numeric(gsub("\\D", "", gsub("-2$","",gsub("-1$","",la$dir)))))

unique(la$site) %>% sort

la %>% 
  mutate(site = toupper(site)) %>%
  mutate(species = tolower(as.character(species))) %>% 
  mutate(site = ifelse(!is.na(as.numeric(site)), paste0("MI",site), site)) %>% 
  mutate(species = gsub("[[:digit:]]","",species)) -> la

la <- la %>% 
  mutate(species = ifelse(species == "athdis" & dir == "LA-2022_08_14", "dryfil", species))

unique(la$site) %>% sort
unique(la$species) %>% sort

laa <- la %>% 
  select(site, species, Area, date) %>% 
  group_by(site, species) %>% 
  summarise(n = n(),
            area_sum = sum(Area),
            date = min(date))

# Other leaf traits

ss <- "https://docs.google.com/spreadsheets/d/1eV_iIHRyKQAPIgEX_4YWRZXbPBGAp8AmAxy3KnYR8LY/edit#gid=0"
d1 <- read_sheet(ss, col_types = "c") %>% 
  select(-KUKIMEMO,-date_prosessed) %>% 
  mutate(site = toupper(site),
         species = tolower(species)) %>% 
  filter(!is.na(species)) %>% 
  mutate(w_weight = gsub(",",".",w_weight),
         d_weight = gsub(",",".",d_weight)) %>% 
  mutate(date = dmy(date_sampled)) %>% 
  select(-date_sampled)

nam <- read_csv2("env_data/all_species_abbr_edited_2.csv")

unique(d1$species)[!unique(d1$species) %in% nam$abbr]

head(d1)

d1 %>% filter(duplicated(d1 %>% select(site, species)))
d1 %>% group_by(site, species) %>%
  summarise(date = min(date),
            n_inds = sum(as.numeric(n_inds), na.rm = T),
            n_leaf = sum(as.numeric(n_leaf), na.rm = T),
            w_weight = mean(as.numeric(w_weight), na.rm = T),
            d_weight = mean(as.numeric(d_weight), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(site = ifelse(!is.na(as.numeric(site)), paste0("MI",site), site)) -> d1

d1 %>% filter(n_leaf == 0)
d1 %>% filter(n_inds == 0)
unique(d1$n_inds)

d1 %>% 
  filter(n_inds > 0) %>% 
  mutate(n_leaf = ifelse(n_leaf == 0, NA, n_leaf)) -> d1

head(d1)
head(laa)

unique(d1$site)[!unique(d1$site) %in% laa$site]
unique(laa$site)[!unique(laa$site) %in% unique(d1$site)]

# Fix names

laa <- laa %>% left_join(., nam, by = c("species" = "abbr")) %>% 
  select(-species) %>% rename(species = name) %>% 
  mutate(species = ifelse(species == "Anthoxanthum odoratum", "Anthoxanthum nipponicum", species))
d1 <- d1 %>% left_join(., nam, by = c("species" = "abbr")) %>% 
  select(-species) %>% rename(species = name) %>% 
  mutate(species = ifelse(species == "Anthoxanthum odoratum", "Anthoxanthum nipponicum", species))


unique(d1$species)[!unique(d1$species) %in% laa$species]
unique(laa$species)[!unique(laa$species) %in% unique(d1$species)]

d4 <- full_join(d1, laa %>% rename(date2 = date)) %>% arrange(site, species)
d4 %>% filter(!complete.cases(.))

d4 %>% filter(duplicated(d4 %>% select(site, species)))

d4 %>% filter(d_weight > w_weight)

d4 %>% mutate(leaf_area = area_sum/n_leaf,
              SLA = area_sum/d_weight,
              LDMC = d_weight/w_weight) -> d4

d4 %>% mutate(diff = n_leaf-n) %>% 
  arrange(desc(diff))
d4 %>% mutate(diff = n_leaf-n) %>% 
  arrange(diff)

d4 <- d4 %>% 
  mutate(species = ifelse(species == "Carex bigelowii" & site == "RA032", "Carex vaginata", species)) %>% 
  mutate(species = ifelse(species == "Carex vaginata" & site == "RA036", "Carex bigelowii", species)) %>% 
  mutate(species = ifelse(species == "Festuca vivipara" & site == "RA055", "Festuca ovina", species)) %>% 
  mutate(species = ifelse(species == "Salix herbacea" & site == "RA057", "Salix polaris", species)) %>% 
  mutate(species = ifelse(species == "Melampyrum sylvaticum" & site == "RA070", "Melampyrum pratense", species)) %>% 
  mutate(species = ifelse(species == "Melampyrum pratense" & site == "RA071", "Melampyrum sylvaticum", species)) %>% 
  mutate(species = ifelse(species == "Saussurea alpina" & site == "RA077", "Hieracium subalpinum", species)) %>% 
  mutate(species = ifelse(species == "Viola rupestris subsp. relicta", "Viola rupestris", species)) %>% 
  mutate(species = ifelse(species == "Carex bigelowii" & site == "RA091", "Carex vaginata", species)) %>% 
  mutate(species = ifelse(species == "Salix herbacea" & site == "RA101", "Salix polaris", species))

summary(d4)

############################################################################
# Height data

d <- readxl::read_xlsx("C:/Users/poniitty/OneDrive - Jyväskylän yliopisto/Kesä2023/earlier_data/Kilpis2022/vegplots.xlsx", col_names = F) %>% 
  slice(c(1:4, 13:1000))

d %>% mutate(...1 = gsub(" ","_",...1)) -> d

t(d) %>% as.data.frame() -> d

colnames(d) <- d[1,] %>% as.matrix %>% as.character() %>% make.unique(sep = "_MAKEUNIQUE")

d <- d[-1,]

d %>% replace(., is.na(.), "0") -> d

tear1 <- function(x){ unlist(lapply(x, function(x) strsplit(x, "/")[[1]][1]))}
tear2 <- function(x){ unlist(lapply(x, function(x) strsplit(x, "/")[[1]][2]))}
tear3 <- function(x){ unlist(lapply(x, function(x) strsplit(x, "/")[[1]][3]))}

d %>% mutate(across(Betula_nana:ncol(d), tear1)) -> cvr
d %>% mutate(across(Betula_nana:ncol(d), tear2)) %>% 
  filter(plot_size != "c") -> medh
d %>% mutate(across(Betula_nana:ncol(d), tear3)) %>% 
  filter(plot_size != "c") -> maxh
d %>% mutate(across(Betula_nana:ncol(d), tear2)) %>% 
  filter(plot_size == "c") -> flow

cvr %>% pivot_longer(cols = Betula_nana:ncol(cvr), names_to = "species", values_to = "cvr") %>% 
  pull(cvr) %>% unique()

medh %>% pivot_longer(cols = Betula_nana:ncol(medh), names_to = "species", values_to = "medh") %>% 
  pull(medh) %>% unique()

medh %>% pivot_longer(cols = Betula_nana:ncol(medh), names_to = "species", values_to = "medh") %>% 
  filter(medh == "")

medh %>% pivot_longer(cols = Betula_nana:ncol(medh), names_to = "species", values_to = "medh") %>% 
  filter(!is.na(medh)) %>% #filter(species == "Cladonia_mitis")
  pull(species) %>% unique()

maxh %>% pivot_longer(cols = Betula_nana:ncol(maxh), names_to = "species", values_to = "maxh") %>% 
  pull(maxh) %>% unique()

maxh %>% pivot_longer(cols = Betula_nana:ncol(maxh), names_to = "species", values_to = "maxh") %>% 
  filter(maxh == "")

flow %>% pivot_longer(cols = Betula_nana:ncol(flow), names_to = "species", values_to = "flow") %>% 
  pull(flow) %>% unique()

flow %>% pivot_longer(cols = Betula_nana:ncol(flow), names_to = "species", values_to = "flow") %>% 
  filter(flow == "")

flow %>% pivot_longer(cols = Betula_nana:ncol(flow), names_to = "species", values_to = "flow") %>% 
  filter(!is.na(flow)) %>% 
  pull(species) %>% unique()


# If everything seems ok

medh %>% pivot_longer(cols = Betula_nana:ncol(medh), names_to = "species", values_to = "median_height") %>% 
  filter(!is.na(median_height)) %>% 
  mutate(median_height = as.numeric(gsub(",",".",median_height))) -> medh

maxh %>% pivot_longer(cols = Betula_nana:ncol(maxh), names_to = "species", values_to = "max_height") %>% 
  filter(!is.na(max_height)) %>% 
  mutate(max_height = as.numeric(gsub(",",".",max_height))) -> maxh

flow %>% pivot_longer(cols = Betula_nana:ncol(flow), names_to = "species", values_to = "reproduction") %>% 
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
  filter(reproduction != 0) %>% 
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
  mutate(across(Betula_nana:ncol(cvr), ~gsub(",",".",.x))) %>% 
  mutate(across(Betula_nana:ncol(cvr), as.numeric)) %>% 
  rename(plot = Plot,
         setting = `Study_setting`,
         plot_type = `plot_size`) -> cvr

all <- full_join(cvr %>% select(plot:Silene_suecica) %>% 
                   select(-c(Cladonia_borealis,Cladonia_mitis,Cladonia_pleurota,
                             Dicranum_fuscescens,Dicranum_scoparium,Flavocetraria_cucullata)) %>% 
                   pivot_longer(cols = Betula_nana:Silene_suecica,names_to = "species",values_to = "cvr") %>% 
                   filter(cvr > 0),
                 heights) %>% 
  full_join(., flow) %>% 
  mutate(plot = toupper(plot)) %>% 
  mutate(species = gsub("MAKEUNIQUE","",species)) %>% 
  mutate(species = gsub("[[:digit:]]","",species)) %>% 
  mutate(species = gsub("_"," ",species))

right_join(all %>% rename(site = plot) %>% rename(date2 = date) %>% mutate(date2 = ymd(date2)),
           d4) %>% 
  filter(startsWith(site,"RA") | startsWith(site,"MAL") | startsWith(site,"AIL")) %>% 
  filter(is.na(cvr)) %>% as.data.frame()
right_join(all %>% rename(site = plot) %>% rename(date2 = date) %>% mutate(date2 = ymd(date2)),
           d4) %>% 
  filter(startsWith(site,"RA") | startsWith(site,"MAL") | startsWith(site,"AIL")) %>% 
  filter(is.na(n_inds)) %>% as.data.frame()


full_join(all %>% rename(site = plot) %>% rename(date2 = date) %>% mutate(date2 = ymd(date2)) %>% filter(plot_type == "b"),
          d4 %>% select(-date2)) %>% filter(cvr > 1) %>% 
  select(-reproduction) %>% 
  filter(!complete.cases(.)) %>% 
  group_by(site) %>% 
  count() %>% arrange(desc(n)) %>% as.data.frame()

unique(all$plot)[!unique(all$plot) %in% d4$site]
unique(d4$site)[!unique(d4$site) %in% all$plot]

allall <- full_join(all %>% rename(site = plot) %>% mutate(date = ymd(date)),
                    d4 %>% select(-date2) %>% rename(date2 = date)) %>% 
  mutate(date = as_date(ifelse(is.na(date), date2, date))) %>% 
  relocate(date, .after = plot_type) %>% select(-date2) %>% 
  rename(n_leaf_image = n)

write_csv(allall, "trait_data/kilpis_vascular_all_2022.csv")

