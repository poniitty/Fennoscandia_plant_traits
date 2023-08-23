# THIS CODE MASTERS THE TRAIT 2021 DATA FROM GOOGLE DRIVE

# install.packages("googlesheets4")

library("googlesheets4")
library(tidyverse)
library(lubridate)

gs4_deauth()

# Leaf area

la <- read_csv("C:/Users/poniitty/OneDrive - University of Helsinki/Kesä2021/kilpisjarvi/leaves/LA_AllLeaves_kilpis_2021.csv")

uus <- la %>% filter(dir == "LA-uusix")
la <- la %>% filter(dir != "LA-uusix")

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
    dirid <- la %>% filter(site == temp$site[1]) %>% group_by(dir) %>% count %>% arrange(desc(n)) %>% slice_head(n = 1) %>% pull(dir)
    
    la <- bind_rows(la,
                    temp %>% mutate(dir = dirid))
  }
  
}

la$date <- ymd(as.numeric(gsub("\\D", "", gsub("-2$","",gsub("-1$","",la$dir)))))

unique(la$site) %>% sort
la %>% 
  filter(site == "RAX1") %>% pull(species) %>% unique

la %>% 
  mutate(site = toupper(site)) %>% 
  mutate(species = ifelse(grepl("phycae", species), "phycae", species)) %>% 
  mutate(species = tolower(as.character(species))) %>% 
  mutate(species = ifelse(species == "gemdry", "gymdry", species)) %>% 
  mutate(species = ifelse(species == "saualp" & site == "1057", "hiesub", species)) %>% 
  mutate(site = ifelse(site == "KOO", "RA051", site)) %>% 
  mutate(site = ifelse(site == "RAX1", "RA047", site)) %>% 
  mutate(site = ifelse(site == "RAX2", "RA048", site)) %>% 
  mutate(site = ifelse(!is.na(as.numeric(site)), paste0("MI",site), site)) -> la

unique(la$site) %>% sort
unique(la$species) %>% sort

laa <- la %>% 
  select(site, species, Area, date) %>% 
  group_by(site, species) %>% 
  summarise(n = n(),
            area_sum = sum(Area),
            date = min(date))

# Other leaf traits

ss <- "https://docs.google.com/spreadsheets/d/1rmYfuVk5Fqx8r9D9S1Mhp3juU3PpL6R6O_OEl6iG7aA/edit#gid=0"
d1 <- read_sheet(ss) %>% 
  select(-KUKIMEMO) %>% 
  mutate(site = toupper(site),
         species = tolower(species)) %>% 
  filter(!is.na(species)) %>% 
  mutate(w_weight = gsub(",",".",w_weight),
         d_weight = gsub(",",".",d_weight))

nam <- read_csv2("env_data/all_species_abbr_edited_2.csv")

unique(d1$species)[!unique(d1$species) %in% nam$abbr]

head(d1)

d1 %>% filter(duplicated(d1 %>% select(site, species)))
d1 %>% group_by(site, species) %>%
  summarise(n_inds = sum(as.numeric(n_inds), na.rm = T),
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

d4 <- full_join(d1, laa) %>% arrange(site, species)
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

d <- readxl::read_xlsx("C:/Users/poniitty/OneDrive - Jyväskylän yliopisto/Kesä2023/earlier_data/Kilpis2021/vegplots.xlsx", col_names = F) %>% 
  slice(c(1:3, 12:1000))

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
  filter(flow == 8)

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

right_join(all %>% rename(site = plot),
           d4) %>% 
  filter(startsWith(site,"RA") | startsWith(site,"MAL") | startsWith(site,"AIL")) %>% 
  filter(is.na(cvr)) %>% as.data.frame()

full_join(all %>% rename(site = plot) %>% filter(plot_type == "b"),
          d4) %>% filter(cvr > 1) %>% 
  select(-reproduction) %>% 
  filter(!complete.cases(.)) %>% 
  group_by(site) %>% 
  count() %>% arrange(desc(n))

unique(all$plot)[!unique(all$plot) %in% d4$site]

allall <- full_join(full_join(all %>% rename(site = plot),
                              d4 %>% group_by(site) %>% summarise(date2 = min(date))),
                    d4) %>% 
  mutate(date = as_date(ifelse(is.na(date), date2, date))) %>% 
  relocate(date, .after = plot_type) %>% select(-date2) %>% 
  rename(n_leaf_image = n) %>%
  filter(!is.na(species))

# ITV grid heights

d <- read_csv("C:/Users/poniitty/OneDrive - Jyväskylän yliopisto/Kesä2023/earlier_data/Kilpis2021/ITVgrid_heights.csv")

d <- left_join(d %>% rename(abbr = species) %>% mutate(abbr = tolower(abbr)),
          nam) %>% 
  group_by(name, plot) %>% 
  summarise(median_height2 = round(mean(height, na.rm = T),1),
            max_height2 = max(height, na.rm = T)) %>% 
  rename(species = name,
         site = plot)

allall <- left_join(allall, d %>% mutate(setting2 = "itvgrids", plot_type2 = "b")) %>% 
  mutate(median_height = ifelse(is.na(median_height), median_height2, median_height),
         max_height = ifelse(is.na(max_height), max_height2, max_height),
         setting = ifelse(is.na(setting), setting2, setting),
         plot_type = ifelse(is.na(plot_type), plot_type2, plot_type)) %>% 
  select(-ends_with("2"))

write_csv(allall, "trait_data/kilpis_vascular_all_2021.csv")
