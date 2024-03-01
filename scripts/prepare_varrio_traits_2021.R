# THIS CODE MASTERS THE VÄRRIÖ TRAIT 2021 DATA FROM GOOGLE DRIVE

# install.packages("googlesheets4")

library("googlesheets4")
library(tidyverse)
library(lubridate)

gs4_deauth()

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


# Leaf area

la <- read_csv("C:/Users/poniitty/OneDrive - University of Helsinki/Kesä2021/varrio/leaves/LA_AllLeaves_varrio_2021.csv")

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
  filter(!site %in% c("liscor.jpg.txt","monuni.jpg.txt")) %>% 
  mutate(site = toupper(site)) %>% 
  mutate(species = ifelse(endsWith(species, "phycae"), "phycae", species)) %>% 
  mutate(species = tolower(as.character(species))) %>% 
  mutate(species = ifelse(species == "gemdry", "gymdry", species)) -> la

unique(la$site) %>% sort
unique(la$species) %>% sort

laa <- la %>% 
  select(site, species, Area, date) %>% 
  group_by(site, species) %>% 
  summarise(n = n(),
            area_sum = sum(Area),
            date = min(date))

# Other leaf traits

ss <- "https://docs.google.com/spreadsheets/d/1q8_wiw6igMWSMEA4dPdcPJn7SWfrLtjRoJgWC0ZBIcY/edit#gid=0"
d1 <- read_sheet(ss) %>% 
  mutate(site = toupper(site),
         species = str_squish(tolower(species))) %>% 
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
  mutate(site = ifelse(!is.na(as.numeric(site)), paste0(add_zeros(site,"VAR")), site)) -> d1

d1 %>% filter(n_leaf == 0)
d1 %>% filter(n_inds == 0)
unique(d1$n_inds)
unique(d1$site)

d1 %>% 
  filter(n_inds > 0) %>% 
  mutate(n_leaf = ifelse(n_leaf == 0, NA, n_leaf)) -> d1

head(d1)
head(laa)

unique(d1$site)[!unique(d1$site) %in% laa$site]
unique(laa$site)[!unique(laa$site) %in% unique(d1$site)]

# Fix names

laa <- laa %>% left_join(., nam, by = c("species" = "abbr")) %>% 
  select(-species) %>% rename(species = name)
d1 <- d1 %>% left_join(., nam, by = c("species" = "abbr")) %>% 
  select(-species) %>% rename(species = name)

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
  mutate(species = ifelse(species == "Equisetum pratense" & site == "VAR030", "Equisetum palustre", species))

summary(d4)

############################################################################
# Height data

d <- readxl::read_xlsx("C:/Users/poniitty/OneDrive - Jyväskylän yliopisto/Kesä2023/earlier_data/Värriö2020/vegplots.xlsx", col_names = F) %>% 
  slice(c(1:3, 13:1000))

d %>% mutate(...1 = gsub(" ","_",...1)) -> d

t(d) %>% as.data.frame() -> d

colnames(d) <- d[1,] %>% as.matrix %>% as.character() %>% make.unique(sep = "_MAKEUNIQUE")

d <- d[-1,]

d %>% replace(., is.na(.), "0") -> d
d <- d %>% 
  select(Plot:Pinvil)

tear1 <- function(x){ unlist(lapply(x, function(x) strsplit(x, "/")[[1]][1]))}
tear2 <- function(x){ unlist(lapply(x, function(x) strsplit(x, "/")[[1]][2]))}
tear3 <- function(x){ unlist(lapply(x, function(x) strsplit(x, "/")[[1]][3]))}

d %>% mutate(across(Agrmer:ncol(d), tear1)) -> cvr
d %>% mutate(across(Agrmer:ncol(d), tear2)) %>% 
  filter(plot_size != "c") -> medh
d %>% mutate(across(Agrmer:ncol(d), tear3)) %>% 
  filter(plot_size != "c") -> maxh
d %>% mutate(across(Agrmer:ncol(d), tear2)) %>% 
  filter(plot_size == "c") -> flow

cvr %>% pivot_longer(cols = Agrmer:ncol(cvr), names_to = "species", values_to = "cvr") %>% 
  pull(cvr) %>% unique()

medh %>% pivot_longer(cols = Agrmer:ncol(medh), names_to = "species", values_to = "medh") %>% 
  pull(medh) %>% unique()

medh %>% pivot_longer(cols = Agrmer:ncol(medh), names_to = "species", values_to = "medh") %>% 
  filter(medh == "")

medh %>% pivot_longer(cols = Agrmer:ncol(medh), names_to = "species", values_to = "medh") %>% 
  filter(!is.na(medh)) %>% #filter(species == "Cladonia_mitis")
  pull(species) %>% unique()

maxh %>% pivot_longer(cols = Agrmer:ncol(maxh), names_to = "species", values_to = "maxh") %>% 
  pull(maxh) %>% unique()

maxh %>% pivot_longer(cols = Agrmer:ncol(maxh), names_to = "species", values_to = "maxh") %>% 
  filter(maxh == "")

flow %>% pivot_longer(cols = Agrmer:ncol(flow), names_to = "species", values_to = "flow") %>% 
  pull(flow) %>% unique()

flow %>% pivot_longer(cols = Agrmer:ncol(flow), names_to = "species", values_to = "flow") %>% 
  filter(!is.na(flow)) %>% 
  pull(species) %>% unique()


# If everything seems ok

medh %>% pivot_longer(cols = Agrmer:ncol(medh), names_to = "species", values_to = "median_height") %>% 
  filter(!is.na(median_height)) %>% 
  mutate(median_height = as.numeric(gsub(",",".",median_height))) -> medh

maxh %>% pivot_longer(cols = Agrmer:ncol(maxh), names_to = "species", values_to = "max_height") %>% 
  filter(!is.na(max_height)) %>% 
  mutate(max_height = as.numeric(gsub(",",".",max_height))) -> maxh

flow %>% pivot_longer(cols = Agrmer:ncol(flow), names_to = "species", values_to = "reproduction") %>% 
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
  mutate(across(Agrmer:ncol(cvr), ~gsub(",",".",.x))) %>% 
  mutate(across(Agrmer:ncol(cvr), as.numeric)) %>% 
  rename(plot = Plot,
         setting = `Study_setting`,
         plot_type = `plot_size`) -> cvr

all <- full_join(cvr %>% select(plot:Pinvil) %>% 
                   pivot_longer(cols = Agrmer:Pinvil, names_to = "species",values_to = "cvr") %>% 
                   filter(cvr > 0),
                 heights) %>% 
  full_join(., flow) %>%
  mutate(plot = toupper(plot)) %>% 
  mutate(species = gsub("MAKEUNIQUE","",species)) %>% 
  mutate(species = gsub("[[:digit:]]","",species)) %>% 
  mutate(species = gsub("_"," ",species))

all <- all %>% 
  mutate(species = tolower(species)) %>% 
  left_join(., nam, by = c("species" = "abbr")) %>% 
  select(-species) %>% rename(species = name) %>% 
  relocate(species, .after = plot_type)


right_join(all %>% rename(site = plot),
           d4) %>% 
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

unique(allall$species)[!unique(allall$species) %in% nam$name]

syns <- readxl::read_xlsx("env_data/synonyms.xlsx")

allall <- allall %>% 
  mutate(species = str_squish(species)) %>% 
  left_join(., syns) %>% 
  mutate(species = ifelse(!is.na(truename), truename, species)) %>% 
  select(-truename)

unique(allall$species)[!unique(allall$species) %in% nam$name]

write_csv(allall, "trait_data/varrio_vascular_all_2021.csv")
