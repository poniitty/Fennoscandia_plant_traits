# THIS CODE MASTERS THE TRAIT 2022 DATA FROM GOOGLE DRIVE

# install.packages("googlesheets4")

library("googlesheets4")
library(tidyverse)
library(lubridate)

######################
# Leaf area

la <- read_csv("C:/Users/poniitty/OneDrive - University of Helsinki/Kesa2023/LA_AllLeaves_kilpis_2023.csv")

la <- la %>% 
  filter(!(mean_la > 0.1 & Area < 0.01))

laa <- la %>% 
  select(id, Area) %>% 
  group_by(id) %>% 
  summarise(n = n(),
            area_sum = sum(Area),
            area_sd = sd(Area))

# Other leaf traits

ss <- "https://docs.google.com/spreadsheets/d/1zTN5fh6CTrTUNQz4xHDLpVVO_HqX0mTUtBoPTUCIlWo/edit#gid=0"
d1 <- read_sheet(ss, col_types = "c") %>% 
  select(-KUKIMEMO,-date_prosessed) %>% 
  mutate(site = toupper(site),
         species = tolower(species)) %>% 
  filter(!is.na(species)) %>% 
  mutate(w_weight = gsub(",",".",w_weight),
         d_weight = gsub(",",".",d_weight)) %>% 
  mutate(date = dmy(date_sampled)) %>% 
  select(-date_sampled) %>% 
  filter(startsWith(site, "VIN"))

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

############################################################################
# Height data

ss <- "https://docs.google.com/spreadsheets/d/1ZAJ2KNE72bqegVvnMvl8V7nbTuhI91npNwIX-iABRgI/edit#gid=0"
d <- read_sheet(ss, col_types = "c", col_names = FALSE) %>% 
  slice(c(1:4, 13:1000))

d %>% mutate(...1 = gsub(" ","_",...1)) -> d

t(d) %>% as.data.frame() -> d

colnames(d) <- d[1,] %>% as.matrix %>% as.character() %>% make.unique(sep = "_MAKEUNIQUE")

d <- d[-1,]

tear1 <- function(x){ unlist(lapply(x, function(x) strsplit(x, "/")[[1]][1]))}
tear2 <- function(x){ unlist(lapply(x, function(x) strsplit(x, "/")[[1]][2]))}
tear3 <- function(x){ unlist(lapply(x, function(x) strsplit(x, "/")[[1]][3]))}

d %>% mutate(across(Agrostis_mertensii:ncol(d), tear1)) -> cvr
d %>% mutate(across(Agrostis_mertensii:ncol(d), tear2)) %>% 
  filter(plot_size != "c") -> medh
d %>% mutate(across(Agrostis_mertensii:ncol(d), tear3)) %>% 
  filter(plot_size != "c") -> maxh
d %>% mutate(across(Agrostis_mertensii:ncol(d), tear2)) %>% 
  filter(plot_size == "c") -> flow

cvr %>% pivot_longer(cols = Agrostis_mertensii:ncol(.), names_to = "species", values_to = "cvr") %>% 
  pull(cvr) %>% unique()

medh %>% pivot_longer(cols = Agrostis_mertensii:ncol(.), names_to = "species", values_to = "medh") %>% 
  pull(medh) %>% unique()

medh %>% pivot_longer(cols = Agrostis_mertensii:ncol(.), names_to = "species", values_to = "medh") %>% 
  filter(medh == "")

medh %>% pivot_longer(cols = Agrostis_mertensii:ncol(.), names_to = "species", values_to = "medh") %>% 
  filter(!is.na(medh)) %>% 
  pull(species) %>% unique()

maxh %>% pivot_longer(cols = Agrostis_mertensii:ncol(.), names_to = "species", values_to = "maxh") %>% 
  pull(maxh) %>% unique()

maxh %>% pivot_longer(cols = Agrostis_mertensii:ncol(.), names_to = "species", values_to = "maxh") %>% 
  filter(maxh == "")

flow %>% pivot_longer(cols = Agrostis_mertensii:ncol(.), names_to = "species", values_to = "flow") %>% 
  pull(flow) %>% unique()

flow %>% pivot_longer(cols = Agrostis_mertensii:ncol(.), names_to = "species", values_to = "flow") %>% 
  filter(flow == "")

flow %>% pivot_longer(cols = Agrostis_mertensii:ncol(.), names_to = "species", values_to = "flow") %>% 
  filter(!is.na(flow)) %>% 
  pull(species) %>% unique()


# If everything seems ok
medh %>% pivot_longer(cols = Agrostis_mertensii:ncol(.), names_to = "species", values_to = "median_height") %>% 
  filter(!is.na(median_height)) %>% 
  mutate(median_height = as.numeric(gsub(",",".",median_height))) -> medh

maxh %>% pivot_longer(cols = Agrostis_mertensii:ncol(.), names_to = "species", values_to = "max_height") %>% 
  filter(!is.na(max_height)) %>% 
  mutate(max_height = as.numeric(gsub(",",".",max_height))) -> maxh

flow %>% pivot_longer(cols = Agrostis_mertensii:ncol(.), names_to = "species", values_to = "reproduction") %>% 
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
  mutate(across(Agrostis_mertensii:ncol(.), ~gsub(",",".",.x))) %>% 
  mutate(across(Agrostis_mertensii:ncol(.), as.numeric)) %>% 
  rename(plot = Plot,
         setting = `Study_setting`,
         plot_type = `plot_size`) %>% 
  select(plot:Viola_palustris) %>% 
  pivot_longer(cols = Agrostis_mertensii:Viola_palustris,names_to = "species",values_to = "cvr") %>% 
  filter(cvr > 0) %>% 
  filter(!is.na(cvr)) -> cvr

all <- cvr %>% 
  full_join(., heights) %>% 
  full_join(., flow) %>% 
  mutate(plot = toupper(plot)) %>% 
  mutate(species = gsub("MAKEUNIQUE","",species)) %>% 
  mutate(species = gsub("[[:digit:]]","",species)) %>% 
  mutate(species = gsub("_"," ",species))

all <- all %>% 
  mutate(species = str_squish(species))
d4 <- d4 %>% 
  mutate(species = str_squish(species))

right_join(all %>% rename(site = plot) %>% rename(date2 = date) %>% mutate(date2 = ymd(date2)),
           d4) %>% 
  filter(is.na(cvr)) %>% as.data.frame()
right_join(all %>% rename(site = plot) %>% rename(date2 = date) %>% mutate(date2 = ymd(date2)),
           d4) %>% 
  filter(is.na(n_inds)) %>% as.data.frame()

allall <- full_join(all %>% rename(site = plot) %>% mutate(date = ymd(date)),
                    d4 %>% rename(date2 = date)) %>% 
  mutate(date = as_date(ifelse(is.na(date), date2, date))) %>% 
  relocate(date, .after = plot_type) %>% select(-date2) %>% 
  rename(n_leaf_image = n) %>%
  filter(!is.na(species))

unique(allall$site)

allall <- allall %>%
  select(-id)

#################################################################################
# 2021 data

# Leaf area
la <- read_csv("C:/Users/poniitty/OneDrive - University of Helsinki/Kesä2021/Vindelfjallen/leaves/LA_AllLeaves_varrio_2021.csv")

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

la <- la %>% 
  mutate(species = ifelse(startsWith(species, "hein"), "desalp", species)) %>% 
  mutate(site = toupper(site)) %>% 
  mutate(species = tolower(as.character(species))) %>% 
  mutate(site = ifelse(site == "VF01", "R10", site),
         site = ifelse(site == "VF02", "R11", site),
         site = ifelse(site == "VF03", "R12", site),
         site = ifelse(site == "VF04", "R8", site),
         site = ifelse(site == "VF05", "R9", site)) %>% 
  mutate(site = ifelse(site == "JOKU", "6-50", site),
         site = ifelse(site == "JOKU2", "R6", site),
         site = ifelse(site == "JOKU3", "12-10", site))

unique(la$site) %>% sort
unique(la$species) %>% sort

laa <- la %>% 
  select(site, species, Area, date) %>% 
  group_by(site, species) %>% 
  summarise(n = n(),
            area_sum = sum(Area),
            date = min(date))

# Other leaf traits

ss <- "https://docs.google.com/spreadsheets/d/1kP5xcUE_PafB_X7nXxqMn70fyu-mG93X7RfKkLS3dQI/edit#gid=0"
d1 <- read_sheet(ss) %>% 
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

summary(d4)

############################################################################
# Height data

ss <- "https://docs.google.com/spreadsheets/d/1dshoFEOK9CReTJ-bDn0FhMul6MVxiFYDDSyyIswMGLo/edit#gid=0"
d <- read_sheet(ss, col_types = "c", col_names = FALSE) %>% 
  slice(c(1:4, 13:1000))

d %>% mutate(...1 = gsub(" ","_",...1)) -> d

t(d) %>% as.data.frame() -> d

colnames(d) <- d[1,] %>% as.matrix %>% as.character() %>% make.unique(sep = "_MAKEUNIQUE")

d <- d[-1,]

tear1 <- function(x){ unlist(lapply(x, function(x) strsplit(x, "/")[[1]][1]))}
tear2 <- function(x){ unlist(lapply(x, function(x) strsplit(x, "/")[[1]][2]))}
tear3 <- function(x){ unlist(lapply(x, function(x) strsplit(x, "/")[[1]][3]))}

d %>% mutate(across(Agrostis_mertensii:ncol(d), tear1)) -> cvr
d %>% mutate(across(Agrostis_mertensii:ncol(d), tear2)) %>% 
  filter(plot_size != "c") -> medh
d %>% mutate(across(Agrostis_mertensii:ncol(d), tear3)) %>% 
  filter(plot_size != "c") -> maxh

cvr %>% pivot_longer(cols = Agrostis_mertensii:ncol(.), names_to = "species", values_to = "cvr") %>% 
  pull(cvr) %>% unique()

medh %>% pivot_longer(cols = Agrostis_mertensii:ncol(.), names_to = "species", values_to = "medh") %>% 
  pull(medh) %>% unique()

medh %>% pivot_longer(cols = Agrostis_mertensii:ncol(.), names_to = "species", values_to = "medh") %>% 
  filter(medh == "")

medh %>% pivot_longer(cols = Agrostis_mertensii:ncol(.), names_to = "species", values_to = "medh") %>% 
  filter(!is.na(medh)) %>% 
  pull(species) %>% unique()

maxh %>% pivot_longer(cols = Agrostis_mertensii:ncol(.), names_to = "species", values_to = "maxh") %>% 
  pull(maxh) %>% unique()

maxh %>% pivot_longer(cols = Agrostis_mertensii:ncol(.), names_to = "species", values_to = "maxh") %>% 
  filter(maxh == "")

# If everything seems ok
medh %>% pivot_longer(cols = Agrostis_mertensii:ncol(.), names_to = "species", values_to = "median_height") %>% 
  filter(!is.na(median_height)) %>% 
  mutate(median_height = as.numeric(gsub(",",".",median_height))) -> medh

maxh %>% pivot_longer(cols = Agrostis_mertensii:ncol(.), names_to = "species", values_to = "max_height") %>% 
  filter(!is.na(max_height)) %>% 
  mutate(max_height = as.numeric(gsub(",",".",max_height))) -> maxh

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

full_join(medh, maxh) %>% 
  mutate(median_height = as.numeric(median_height),
         max_height = as.numeric(max_height)) %>% 
  filter(complete.cases(.)) %>% 
  rename(plot = Plot,
         setting = `Study_setting`,
         plot_type = `plot_size`) -> heights

cvr %>% 
  mutate(across(Agrostis_mertensii:ncol(.), ~gsub(",",".",.x))) %>% 
  mutate(across(Agrostis_mertensii:ncol(.), as.numeric)) %>% 
  rename(plot = Plot,
         setting = `Study_setting`,
         plot_type = `plot_size`) %>% 
  select(plot:Viola_palustris) %>% 
  pivot_longer(cols = Agrostis_mertensii:Viola_palustris,names_to = "species",values_to = "cvr") %>% 
  filter(cvr > 0) %>% 
  filter(!is.na(cvr)) -> cvr

all <- cvr %>% 
  full_join(., heights) %>% 
  mutate(plot = toupper(plot)) %>% 
  mutate(species = gsub("MAKEUNIQUE","",species)) %>% 
  mutate(species = gsub("[[:digit:]]","",species)) %>% 
  mutate(species = gsub("_"," ",species))

all <- all %>% 
  mutate(species = str_squish(species))
d4 <- d4 %>% 
  mutate(species = str_squish(species))

right_join(all %>% rename(site = plot) %>% rename(date2 = date) %>% mutate(date2 = ymd(date2)),
           d4) %>% 
  filter(is.na(cvr)) %>% as.data.frame()
right_join(all %>% rename(site = plot) %>% rename(date2 = date) %>% mutate(date2 = ymd(date2)),
           d4) %>% 
  filter(is.na(n_inds)) %>% as.data.frame()

allall2 <- full_join(all %>% rename(site = plot) %>% mutate(date = ymd(date)),
                    d4 %>% rename(date2 = date)) %>% 
  mutate(date = as_date(ifelse(is.na(date), date2, date))) %>% 
  relocate(date, .after = plot_type) %>% select(-date2) %>% 
  rename(n_leaf_image = n) %>%
  filter(!is.na(species))

unique(allall2$site)

################################################################################
# Unify site names

library(sf)

p <- st_read("C:/Users/poniitty/OneDrive - University of Helsinki/Kesä2021/Vindelfjallen/VIN_logger_locations.gpkg") %>%
  arrange(site) %>% 
  rowid_to_column("site2") %>% 
  mutate(site = ifelse(is.na(site), paste0("VIN0",site2), site)) %>% 
  select(-site2)
tail(p)
p2 <- st_read("C:/Users/poniitty/OneDrive - University of Helsinki/Kesa2023/vindeln/vindeln_20230805-081925.gpx") %>% 
  select(name) %>% 
  mutate(name = paste0("VIN",name)) %>% 
  filter(name %in% unique(allall$site))

dd <- st_is_within_distance(p2, p, dist = 0.01)
p2$site <- p$site[unlist(dd)]

p <- left_join(p,
               p2 %>% st_drop_geometry())

p3 <- readxl::read_xlsx("C:/Users/poniitty/OneDrive - University of Helsinki/Kesä2021/Vindelfjallen/Transect placements PN.xlsx") %>% 
  select(1:7) %>% 
  setNames(c("transect","plot","X","Y","soil","marks","logger")) %>% 
  select(-soil,-marks) %>% 
  filter(is.na(logger)) %>% 
  mutate(site = paste0(transect,"-",plot)) %>% 
  mutate(site = gsub("Random-","R",site)) %>% 
  filter(site %in% unique(allall2$site)) %>% 
  filter(!site %in% p$site2021)

p3 <- p3 %>% 
  st_as_sf(coords = c("X","Y"), crs = 4326) %>% 
  rename(tomst_id = logger,
         site2021 = site) %>% 
  select(tomst_id, site2021)

p <- bind_rows(p, 
               p3 %>% rename(geom = geometry)) %>%
  arrange(site) %>% 
  rowid_to_column("site2") %>% 
  mutate(site = ifelse(is.na(site), paste0("VIN0",site2), site)) %>% 
  select(-site2)
tail(p)

allall <- left_join(allall,
          p %>% rename(site2 = site) %>% select(name, site2) %>% st_drop_geometry(), 
          by = join_by(site == name)) %>% 
  relocate(site2, .after = site) %>% 
  select(-site) %>% 
  rename(site = site2)

allall2 <- left_join(allall2,
                    p %>% rename(site2 = site) %>% select(site2021, site2) %>% st_drop_geometry(), 
                    by = join_by(site == site2021)) %>% 
  relocate(site2, .after = site) %>% 
  select(-site) %>% 
  rename(site = site2)

dall <- bind_rows(allall, 
                  allall2 %>% 
                    mutate(setting = ifelse(is.na(setting), "Vindelfjallen", setting),
                           plot_type = ifelse(is.na(plot_type), "b", plot_type)))

tail(dall)

write_csv(dall, "trait_data/vindelfjallen_vascular_all.csv")
st_write(p, "env_data/GIS/VIN_coordinates_all.gpkg")
