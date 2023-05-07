library(tidyverse)
library(lubridate)

# Leaf area
la <- read_csv("C:/Users/OMISTAJA/OneDrive - University of Helsinki/Kesä2020/LA_AllLeaves_kilpis_2020.csv")

uus <- la %>% filter(dir == "LA-UUSIX")
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
    dirid <- la %>% filter(site == temp$site[1]) %>% group_by(dir) %>% count %>% arrange(desc(n)) %>% slice_head(n = 1) %>% pull(dir)
    
    la <- bind_rows(la,
                    temp %>% mutate(dir = dirid))
  }
  
}

la$date <- ymd(as.numeric(gsub("\\D", "", la$dir)))

la %>% filter(!grepl("BOTLUN",id)) %>% 
  filter(!grepl("016_hieraa_0001",id)) %>% 
  filter(!grepl("K18_empnig_0001",id)) %>% 
  mutate(species = ifelse(grepl("Jul26_29",id), "silaca", species),
         site = ifelse(grepl("Jul26_29",id), "013", site)) %>% 
  mutate(site = ifelse(grepl("MAL061_salher_0001",id), "MAL091", site)) %>% 
  mutate(site = ifelse(grepl("MAL061_gnasup",id), "MAL091", site)) %>% 
  mutate(site = ifelse(grepl("MAL061_callap",id), "MAL091", site)) %>% 
  mutate(site = ifelse(grepl("MAL061_castet",id) & dir == "LA-RA2020_08_10", "MAL091", site)) %>% 
  mutate(site = ifelse(grepl("MAL061_luzcon",id) & dir == "LA-RA2020_08_10", "MAL091", site)) %>% 
  mutate(species = ifelse(grepl("1043_carex",id), "caraqu", species)) %>% 
  mutate(species = ifelse(species == "pyrgra","pyrrot",species)) %>% 
  mutate(species = ifelse(species == "phycom", "phycae", species)) %>% 
  mutate(species = ifelse(species == "carequ", "caraqu", species)) %>% 
  mutate(site = ifelse(grepl("MAL043_crycri",id), "MAL042", site)) %>% 
  mutate(site = ifelse(grepl("MAL043_rumace",id), "MAL042", site)) %>% 
  mutate(site = ifelse(grepl("MAL043_antodo",id), "MAL042", site)) %>% 
  mutate(site = ifelse(grepl("MAL043_gersyl",id), "MAL042", site)) %>% 
  mutate(site = ifelse(grepl("MAL043_ranacr",id), "MAL042", site)) %>% 
  mutate(site = ifelse(grepl("MAL043_desfle",id), "MAL042", site)) %>% 
  mutate(site = ifelse(grepl("MAL043_vacmyr.jpg",id), "MAL042", site)) %>% 
  mutate(site = ifelse(grepl("025_poaalp",id), "026", site)) %>% 
  mutate(site = ifelse(grepl("025_saxniv",id), "026", site)) %>% 
  mutate(site = ifelse(grepl("025_silaca",id), "026", site)) %>% 
  mutate(species = ifelse(grepl("MAL030_poaalp",id), "corsue", species)) %>% 
  mutate(site = ifelse(startsWith(site,"0"), paste0("RA",site), site)) %>% 
  mutate(site = ifelse(!is.na(as.numeric(site)), paste0("MI",site), site)) %>%
  mutate(species = gsub(".JPG.txt","",species)) %>% 
  mutate(species = gsub("[[:digit:]]","",species)) -> la
la <- la %>% mutate(species = ifelse(species == "pyrgra","pyrrot",species))
la %>% filter(site == "MAL042")
la %>% filter(grepl("botlun", species))
unique(la$site)
unique(la$species) %>% sort

laa <- la %>% 
  select(site, species, Area, date) %>% 
  group_by(site, species) %>% 
  summarise(n = n(),
            area_sum = sum(Area),
            date = min(date))

# Other traits
d1 <- read_csv2("C:/Users/OMISTAJA/OneDrive - University of Helsinki/Kesä2020/traits2020.csv") %>% 
  mutate(species = ifelse(species == "cricry","crycri",species)) %>% 
  mutate(species = ifelse(species == "viobof","viobif",species)) %>% 
  mutate(species = ifelse(species == "empher","empnig", species)) %>% 
  filter(species != "empnig2") %>% 
  mutate(species = gsub("[[:digit:]]","",species)) %>% 
  select(-"...7",-"...8",-seed_mass,-seed_n)
d2 <- read_csv2("C:/Users/OMISTAJA/OneDrive - University of Helsinki/Kesä2020/julialta.csv") %>% 
  mutate(species = paste(genus, species, sep = " ")) %>% select(-genus,-memo,-date) %>% 
  mutate(species = ifelse(species == "Solidago viraugea","Solidago virgaurea",species)) %>% 
  mutate(species = ifelse(species == "Deschampsia flexuosa","Avenella flexuosa",species)) %>% 
  mutate(species = ifelse(species == "Taraxacum sp","Taraxacum",species)) %>% 
  mutate(species = ifelse(species == "Trientalis europaea","Lysimachia europaea",species)) %>% 
  mutate(species = ifelse(species == "Antennaria dioca","Antennaria dioica",species)) %>% 
  mutate(species = ifelse(species == "Hierachium sp","Hieracium alpinum",species)) %>% 
  mutate(species = ifelse(species == "Pyrola grandifolia","Pyrola rotundifolia",species)) %>% 
  mutate(species = ifelse(species == "Pedicular lapponica","Pedicularis lapponica",species)) %>% 
  mutate(species = ifelse(species == "Anthoxantum odoratum","Anthoxanthum nipponicum",species)) %>% 
  mutate(species = ifelse(species == "Erigeron uniflora","Erigeron uniflorus",species)) %>% 
  mutate(species = ifelse(species == "Cassiope hypnoides","Harrimanella hypnoides",species)) %>% 
  mutate(species = ifelse(species == "Empetrum herbacea","Empetrum nigrum",species)) %>% 
  mutate(species = ifelse(species == "Carex rotundifolia","Carex rotundata",species)) %>% 
  mutate(species = ifelse(species == "Gnaphalium supinum","Omalotheca supina",species)) %>% 
  mutate(species = ifelse(species == "Triophorum cespitosum","Trichophorum cespitosum",species)) %>% 
  mutate(species = ifelse(species == "Betula nana" & site == "1043","Salix herbacea",species)) %>% 
  select(site, species, individual_nr, leaves_nr_abs, wet_mass_g, dry_mass_g) %>% 
  rename(n_inds = individual_nr,
         n_leaf = leaves_nr_abs,
         w_weight = wet_mass_g,
         d_weight = dry_mass_g)
nam <- read_csv2("C:/Science/Summer2020/all_species_abbr_edited_2.csv")

unique(d2$species)[!unique(d2$species) %in% nam$name]
unique(d1$species)[!unique(d1$species) %in% nam$abbr]

head(d1)
head(d2)

d2 %>% left_join(., nam, by = c("species" = "name")) %>% 
  distinct(site, species, d_weight, .keep_all = T) %>% 
  select(-species) %>% rename(species = abbr) -> d2

d3 <- full_join(d1,d2)
d3 %>% filter(duplicated(d3 %>% select(site, species)))
d3 %>% group_by(site, species) %>%
  summarise(n_inds = sum(n_inds, na.rm = T),
            n_leaf = sum(n_leaf, na.rm = T),
            w_weight = mean(as.numeric(w_weight), na.rm = T),
            d_weight = mean(as.numeric(d_weight), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(site = ifelse(!is.na(as.numeric(site)), paste0("MI",site), site)) -> d3

head(d3)
head(laa)

unique(d3$site)[!unique(d3$site) %in% laa$site]
unique(laa$site)[!unique(laa$site) %in% unique(d3$site)]

# Fix names

laa <- laa %>% left_join(., nam, by = c("species" = "abbr")) %>% 
  select(-species) %>% rename(species = name) %>% 
  mutate(species = ifelse(species == "Anthoxanthum odoratum", "Anthoxanthum nipponicum", species))
d3 <- d3 %>% left_join(., nam, by = c("species" = "abbr")) %>% 
  select(-species) %>% rename(species = name) %>% 
  mutate(species = ifelse(species == "Anthoxanthum odoratum", "Anthoxanthum nipponicum", species))

unique(d3$species)[!unique(d3$species) %in% laa$species]
unique(laa$species)[!unique(laa$species) %in% unique(d3$species)]

d4 <- full_join(d3, laa) %>% arrange(site, species)
d4 %>% filter(!complete.cases(.))

d4 %>% filter(duplicated(d4 %>% select(site, species)))

d4 %>% filter(d_weight > w_weight)
d4 %>% mutate(d_weight = ifelse(site == "MI343" & species == "Betula nana",
                                d_weight/10, d_weight)) %>% 
  mutate(d_weight = ifelse(site == "MI529" & species == "Saussurea alpina",
                                d_weight/10, d_weight)) %>% 
  mutate(d_weight = ifelse(site == "MAL082" & species == "Carex bigelowii",
                                d_weight*10, d_weight)) %>% 
  mutate(d_weight = ifelse(site == "MI422" & species == "Eriophorum angustifolium",
                                d_weight*10, d_weight)) -> d4

d4 %>% mutate(leaf_area = area_sum/n_leaf,
              SLA = area_sum/d_weight,
              LDMC = d_weight/w_weight) -> d4

d4 %>% mutate(diff = n_leaf-n) %>% 
  arrange(desc(diff))
d4 %>% mutate(diff = n_leaf-n) %>% 
  arrange(diff)

d4 <- d4 %>% 
  mutate(species = ifelse(species == "Salix herbacea" & site == "K16", "Salix polaris", species)) %>% 
  mutate(species = ifelse(species == "Nardus stricta" & site == "MAL053", "Juncus trifidus", species)) %>% 
  mutate(species = ifelse(species == "Salix herbacea" & site == "MI11204", "Salix polaris", species)) %>% 
  mutate(species = ifelse(species == "Salix herbacea" & site == "MI11219", "Salix polaris", species)) %>% 
  mutate(species = ifelse(species == "Salix herbacea" & site == "MI367", "Salix polaris", species)) %>% 
  mutate(species = ifelse(species == "Carex vaginata" & site == "MI685", "Carex bigelowii", species)) %>% 
  mutate(species = ifelse(species == "Carex vaginata" & site == "MI697", "Carex bigelowii", species)) %>% 
  mutate(species = ifelse(species == "Hieracium alpinum" & site == "MI259", "Saussurea alpina", species)) %>% 
  mutate(site = ifelse(site == "MAL092", "MAL052",site)) %>% 
  mutate(site = ifelse(site == "RA018", "RA019",site))

summary(d4)

############################################################################
# Height data

d <- readxl::read_xlsx("C:/Users/OMISTAJA/OneDrive - University of Helsinki/Kesä2020/vegetation2020.xlsx", col_names = F) %>% 
  slice(c(1:3, 12:1000))

d %>% mutate(...1 = gsub(" ","_",...1)) -> d

t(d) %>% as.data.frame() -> d

colnames(d) <- d[1,] %>% as.matrix %>% as.character()

d %>% filter(Plot != "Plot") -> d

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
  filter(maxh == "0.05")

flow %>% pivot_longer(cols = Betula_nana:ncol(flow), names_to = "species", values_to = "flow") %>% 
  pull(flow) %>% unique()


flow %>% pivot_longer(cols = Betula_nana:ncol(flow), names_to = "species", values_to = "flow") %>% 
  filter(!is.na(flow)) %>% 
  pull(species) %>% unique()


# If everything seems ok

medh %>% pivot_longer(cols = Betula_nana:ncol(medh), names_to = "species", values_to = "median_height") %>% 
  filter(!is.na(median_height)) -> medh

maxh %>% pivot_longer(cols = Betula_nana:ncol(maxh), names_to = "species", values_to = "max_height") %>% 
  filter(!is.na(max_height)) -> maxh

flow %>% pivot_longer(cols = Betula_nana:ncol(flow), names_to = "species", values_to = "reproduction") %>% 
  filter(!is.na(reproduction)) -> flow

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

cvr %>% mutate(across(Betula_nana:ncol(cvr), as.numeric)) %>% 
  rename(plot = Plot,
         setting = `Study_setting`,
         plot_type = `plot_size`) -> cvr

all <- full_join(cvr %>% select(plot:Viscaria_alpina) %>% 
            select(-c(Cladonia_borealis,Cladonia_mitis,Cladonia_pleurota,
                      Dicranum_fuscescens,Dicranum_scoparium,Flavocetraria_cucullata)) %>% 
            pivot_longer(cols = Betula_nana:Viscaria_alpina,names_to = "species",values_to = "cvr") %>% 
            filter(cvr > 0),
          heights) %>% 
  full_join(., flow) %>% 
  mutate(plot = toupper(plot)) %>% 
  mutate(species = gsub("_"," ",species))

####################################################################
# PREPROCESS JULIAS DATA

d <- read_csv2("C:/Users/OMISTAJA/OneDrive - University of Helsinki/Kesä2020/Kemppinen_vegetation_2020.csv", col_names = F)

d %>% mutate(X1 = gsub(" ","_",X1)) -> d

t(d) %>% as.data.frame() -> d
colnames(d) <- d %>% slice(1) %>% as.character()
d %>% filter(site != "site") %>% 
  select(-photos) -> d

d %>% filter(observation == "coverage") %>% 
  select(-date,-observation) %>% 
  mutate(setting = "mikkuna",
         plot_type = "b") %>% 
  rename(plot = site) %>% 
  relocate(plot, setting, plot_type) -> cvr

cvr %>% replace(., is.na(.), "0") %>% 
  mutate(across(Andromeda_polifolia:ncol(.), as.numeric)) -> cvr

d %>% filter(observation == "med_height") %>% 
  select(-date,-observation) %>% 
  mutate(setting = "mikkuna",
         plot_type = "b") %>% 
  rename(plot = site) %>% 
  relocate(plot, setting, plot_type) %>% 
  pivot_longer(cols = Andromeda_polifolia:ncol(.), names_to = "species", values_to = "median_height") %>% 
  filter(!is.na(median_height)) -> medh

d %>% filter(observation == "max_height") %>% 
  select(-date,-observation) %>% 
  mutate(setting = "mikkuna",
         plot_type = "b") %>% 
  rename(plot = site) %>% 
  relocate(plot, setting, plot_type) %>% 
  pivot_longer(cols = Andromeda_polifolia:ncol(.), names_to = "species", values_to = "max_height") %>% 
  filter(!is.na(max_height)) -> maxh

full_join(medh, maxh) %>% 
  filter(!complete.cases(.))

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
  filter(complete.cases(.)) -> heights


all2 <- full_join(cvr %>% 
                   pivot_longer(cols = Andromeda_polifolia:Ranunculus_nivalis,names_to = "species",values_to = "cvr") %>% 
                   filter(cvr > 0),
                 heights) %>% 
  mutate(plot = paste0("MI",toupper(plot))) %>% 
  mutate(species = gsub("_"," ",species))

all <- bind_rows(all, all2)

all <- all %>% 
  mutate(species = ifelse(species == "Cassiope hypnoides", "Harrimanella hypnoides", species)) %>% 
  mutate(species = ifelse(species == "Arctostaphylos alpina", "Arctous alpina", species)) %>% 
  mutate(species = ifelse(species == "Loiseleuria procumbens", "Kalmia procumbens", species)) %>% 
  mutate(species = ifelse(species == "Anthoxanthum odoratum", "Anthoxanthum nipponicum", species)) %>% 
  mutate(species = ifelse(species == "Deschampsia flexuosa", "Avenella flexuosa", species)) %>% 
  mutate(species = ifelse(species == "Taraxacum sp.", "Taraxacum", species)) %>% 
  mutate(species = ifelse(species == "Gnaphalium supinum", "Omalotheca supina", species)) %>% 
  mutate(species = ifelse(species == "Gnaphalium norvegicum", "Omalotheca norvegica", species)) %>% 
  mutate(species = ifelse(species == "Trientalis europaea", "Lysimachia europaea", species)) %>% 
  mutate(species = ifelse(species == "Lycopodium annotinum", "Spinulum annotinum", species)) %>% 
  mutate(species = ifelse(species == "Euphrasia frigida", "Euphrasia wettsteinii", species)) %>% 
  mutate(species = ifelse(species == "Betula pubescens subsp. czerepanovii", "Betula pubescens", species)) %>% 
  mutate(species = ifelse(species == "Minuartia biflora", "Cherleria biflora", species)) %>% 
  mutate(species = ifelse(species == "Saxifraga nivalis", "Micranthes nivalis", species)) %>% 
  mutate(species = ifelse(species == "Hierochloe hirta", "Anthoxanthum nitens", species))
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
  rename(n_leaf_image = n)

write_csv(allall, "trait_data/kilpis_vascular_all_2020.csv")

