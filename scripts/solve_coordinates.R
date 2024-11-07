library(tidyverse)
library(sf)

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
# p <- st_read("env_data/GIS/site_coordinates.gpkg") %>% 
#   st_transform(4326)
# crds <- st_coordinates(p) %>% as.data.frame()
# p$Lon <- crds$X
# p$Lat <- crds$Y
# st_write(p, "env_data/GIS/site_coordinates.gpkg", append = FALSE)
###########################################
# Värriö

p <- st_read("env_data/GIS/site_coordinates.gpkg") %>% 
  select(site, Lon, Lat) %>% 
  filter(startsWith(site, "VAR")) %>% 
  st_drop_geometry()
d <- read_csv("trait_data/varrio_vascular_all_2021.csv")

d %>% filter(!site %in% p$site)

d1 <- left_join(d, p) %>% 
  relocate(Lat, Lon, .after = species) %>% 
  mutate(site = ifelse(site == "NONE", "EXT", site),
         setting = "VAR") %>% 
  mutate(reproduction = ifelse(reproduction == 1, 0, reproduction))

############################################
# OULANKA

p <- st_read("env_data/GIS/site_coordinates.gpkg") %>% 
  select(site, Lon, Lat) %>% 
  filter(startsWith(site, "OUL")) %>% 
  st_drop_geometry()
d <- bind_rows(read_csv("trait_data/oulanka_vascular_all_2023.csv"),
               read_csv("trait_data/oulanka_vascular_all_2024.csv"))

d %>% filter(!site %in% p$site)

d2 <- left_join(d, p) %>% 
  relocate(Lat, Lon, .after = species) %>% 
  mutate(site = ifelse(site == "NONE", "EXT", site),
         setting = "OUL")
d2 %>% filter(reproduction == 0)

############################################
# RASTIGAISA

p <- st_read("env_data/GIS/site_coordinates.gpkg") %>% 
  select(site, Lon, Lat) %>% 
  filter(startsWith(site, "RAS")) %>% 
  st_drop_geometry()

d <- read_csv("trait_data/rastigaisa_vascular_all_2023.csv") %>% 
  mutate(site = add_zeros(parse_number(site), "RAS"))

d %>% filter(!site %in% p$site)

d3 <- left_join(d, p) %>% 
  relocate(Lat, Lon, .after = species) %>% 
  mutate(site = ifelse(site == "NONE", "EXT", site),
         setting = "RAS")

############################################
# PALLAS

p <- st_read("env_data/GIS/site_coordinates.gpkg") %>% 
  select(site, Lon, Lat) %>% 
  filter(startsWith(site, "PAL")) %>% 
  st_drop_geometry()

d <- read_csv("trait_data/pallas_vascular_all_2024.csv")

d %>% filter(!site %in% p$site)

d8 <- left_join(d, p) %>% 
  relocate(Lat, Lon, .after = species) %>% 
  mutate(site = ifelse(site == "NONE", "EXT", site),
         setting = "PAL")

############################################
# Mattavarri

p <- st_read("env_data/GIS/site_coordinates.gpkg") %>% 
  select(site, Lon, Lat) %>% 
  filter(startsWith(site, "MAT")) %>% 
  st_drop_geometry()

d <- read_csv("trait_data/mattavarri_vascular_all_2023.csv") %>% 
  mutate(site = add_zeros(parse_number(site), "MAT"))

d %>% filter(!site %in% p$site)

d4 <- left_join(d, p) %>% 
  relocate(Lat, Lon, .after = species) %>% 
  mutate(site = ifelse(site == "NONE", "EXT", site),
         setting = "MAT")
d4 %>% filter(reproduction == 0)

############################################
# VINDELFJÄLLEN

p <- st_read("env_data/GIS/VIN_coordinates_all.gpkg") %>% 
  st_transform(4326)
crds <- st_coordinates(p) %>% as.data.frame()
p$Lon <- crds$X
p$Lat <- crds$Y
p <- p %>% 
  select(site, Lon, Lat) %>% 
  st_drop_geometry()

d <- read_csv("trait_data/vindelfjallen_vascular_all.csv")

d %>% filter(!site %in% p$site)

d5 <- left_join(d, p) %>% 
  relocate(Lat, Lon, .after = species) %>% 
  mutate(site = ifelse(site == "NONE", "EXT", site),
         setting = "VIN")
d5 %>% filter(reproduction == 0)

############################################
p <- st_read("env_data/GIS/site_coordinates.gpkg") %>% 
  select(site) %>% 
  st_transform(crs = 4326)

p <- bind_rows(p,
               st_read("C:/Users/poniitty/OneDrive - Jyväskylän yliopisto/Kesä2023/plot_coords/all_tomsts_2022.gpkg") %>% 
                 mutate(id = gsub("_","",id)) %>% 
                 rename(site = id) %>% 
                 st_transform(crs = 4326) %>% 
                 select(site) %>% 
                 filter(!site %in% p$site),
               st_read("C:/Users/poniitty/OneDrive - Jyväskylän yliopisto/Kesä2023/plot_coords/ITV_grids.gpkg") %>% 
                 rename(site = Comment) %>% 
                 mutate(site = paste0(str_sub(site, 1, 1), parse_number(site))) %>% 
                 st_transform(crs = 4326) %>% 
                 select(site) %>% 
                 st_zm(drop = TRUE, what = "ZM") %>% 
                 filter(!site %in% p$site),
               st_read("C:/Users/poniitty/OneDrive - Jyväskylän yliopisto/Kesä2023/plot_coords/roslin_plots.gpkg") %>% 
                 st_transform(crs = 4326) %>% 
                 filter(!site %in% p$site),
               st_read("C:/Users/poniitty/OneDrive - Jyväskylän yliopisto/Kesä2023/plot_coords/bugnet_plots.gpkg") %>% 
                 st_transform(crs = 4326) %>% 
                 distinct(site, .keep_all = T) %>% 
                 filter(!site %in% p$site),
               read_csv("C:/Users/poniitty/OneDrive - University of Helsinki/Kesa2023/All_site_points_2023.csv") %>% 
                 filter(startsWith(site, "gua")) %>% 
                 mutate(site = gsub("gua","KUA", site)) %>% 
                 st_as_sf(coords = c("X","Y"), crs = 32634) %>% 
                 st_transform(4326) %>% 
                 rename(geom = geometry) %>% 
                 select(site),
               read_csv("C:/Users/poniitty/OneDrive - University of Helsinki/Kesä2024/All_trait_points_2024.csv") %>% 
                 filter(startsWith(site, "RA3") | startsWith(site, "EXT17")) %>% 
                 st_as_sf(coords = c("X","Y"), crs = 32634) %>% 
                 st_transform(4326) %>% 
                 rename(geom = geometry) %>% 
                 select(site) %>% 
                 mutate(site = gsub("-","_",site)) %>% 
                 mutate(site = ifelse(site == "EXT1707_3","EXT170724_3",site))) %>% 
  mutate(site = toupper(site))

p <- bind_rows(p,
               read_delim("C:/Users/poniitty/OneDrive - University of Helsinki/lumi2023/all_snowpoints.txt", delim = ";") %>% 
                 st_as_sf(coords = c("X","Y"), crs = 32634) %>% 
                 st_transform(4326) %>% 
                 rename(geom = geometry) %>% 
                 mutate(id = gsub("_","",id)) %>% 
                 rename(site = id) %>% 
                 filter(startsWith(site,"MI")) %>% 
                 filter(!site %in% p$site))

p <- bind_rows(p,
               st_read("C:/Users/poniitty/OneDrive - Jyväskylän yliopisto/Kesä2023/plot_coords/rest_ra_plots.gpkg") %>% 
                 st_transform(crs = 4326) %>% 
                 select(site) %>% 
                 filter(!site %in% p$site))

p <- bind_rows(p,
               lapply(c("A","B","C","D","E","F"), function(x){
                 p %>% 
                   filter(startsWith(site, "AE") | startsWith(site, "SE")) %>% 
                   mutate(site = paste0(site, x))
               }) %>% 
                 bind_rows())

p <- bind_rows(p,
               tibble(site = "MI526",
                      X = 492708.8557,
                      Y = 7660483.267) %>% 
                 st_as_sf(coords = c("X","Y"), crs = 32634) %>% 
                 st_transform(4326) %>% 
                 rename(geom = geometry))


p <- bind_rows(p,
               lapply(c("N","E","S","W"), function(x){
                 pt <- p %>% 
                   filter(startsWith(site, "MI")) %>% 
                   mutate(site = paste0(site, x)) %>% 
                   st_transform(32634)
                 pt <- bind_cols(pt %>% st_drop_geometry(),
                                 pt %>% st_coordinates() %>% as.data.frame())
                 if(x == "N"){
                   pt$Y <- pt$Y + 5
                 }
                 if(x == "E"){
                   pt$X <- pt$X + 5
                 }
                 if(x == "S"){
                   pt$Y <- pt$Y - 5
                 }
                 if(x == "W"){
                   pt$X <- pt$X - 5
                 }
                 
                 pt <- pt %>% 
                   st_as_sf(coords = c("X","Y"), crs = 32634) %>% 
                   st_transform(4326) %>% 
                   rename(geom = geometry)
                 
                 return(pt)
                 
               }) %>% 
                 bind_rows())

p <- p %>% 
  distinct(site, .keep_all = T)

d <- bind_rows(read_csv("trait_data/kilpis_vascular_all_2016-2019.csv") %>% 
                 rename(site = plot),
               read_csv("trait_data/kilpis_vascular_all_2020.csv") %>% 
                 mutate(date = as_date(ifelse(is.na(date),as_date("2020-07-23"),date))),
               read_csv("trait_data/kilpis_vascular_all_2021.csv") %>% 
                 mutate(date = as_date(ifelse(is.na(date),as_date("2021-07-22"),date))),
               read_csv("trait_data/kilpis_vascular_all_2022.csv"),
               read_csv("trait_data/kilpis_vascular_all_2023.csv"),
               read_csv("trait_data/kilpis_vascular_all_2024.csv")) %>% 
  mutate(site = toupper(site)) %>% 
  mutate(site = unlist(lapply(site, function(x) str_split(x,"-")[[1]][1]))) %>% 
  mutate(site = ifelse(site == "NONE","EXT",site)) %>% 
  filter(!is.na(species)) %>% 
  filter(!startsWith(site, "SM"),
         !startsWith(site, "W"))

unique(d$site[which(!d$site %in% p$site)])

# Combine non-EXT traits

d6 <- d %>% 
  filter((!startsWith(site, "EXT")) | startsWith(site, "EXT17"))
unique(d6$site[which(!d6$site %in% p$site)])

d6 <- left_join(d6,
                bind_cols(p %>% st_drop_geometry(),
                          p %>% st_coordinates() %>% as.data.frame())) %>% 
  rename(Lat = Y, Lon = X) %>% 
  relocate(Lat, Lon, .after = species)

# Combine EXT traits
d7 <- d %>% filter(startsWith(site, "EXT")) %>% 
  filter(!startsWith(site, "EXT17")) %>% 
  mutate(site = ifelse(date == "2024-07-23" & species == "Eriophorum scheuchzeri" & w_weight < 0.16, "EXT2", site)) %>% 
  mutate(site = ifelse(date == "2024-07-23" & species == "Saxifraga cernua" & w_weight < 0.1, "EXT2", site)) %>% 
  mutate(site = ifelse(date == "2024-07-21" & species == "Silene dioica" & w_weight < 0.7, "EXT2", site)) %>% 
  mutate(site = ifelse(date == "2024-07-31" & species == "Petasites frigidus" & w_weight < 9, "EXT2", site))

pp1 <- bind_rows(read_csv("C:/Users/poniitty/OneDrive - Jyväskylän yliopisto/Kesä2023/earlier_data/species_points/species_points_2020_only.csv") %>% 
                  filter(endsWith(Comment, " N") | grepl(" NAYTE ", Comment)) %>% 
                  st_as_sf(coords = c("X","Y"), crs = 3067) %>% 
                  st_transform(crs = 4326) %>% 
                  rename(geom = geometry) %>% 
                  mutate(site = "EXT") %>% 
                  select(site, species),
                read_csv("C:/Users/poniitty/OneDrive - Jyväskylän yliopisto/Kesä2023/earlier_data/species_points/species_points_2021.csv") %>%
                  filter(endsWith(Comment, " N")) %>% 
                  st_as_sf(coords = c("X","Y"), crs = 32634) %>% 
                  st_transform(crs = 4326) %>% 
                  rename(geom = geometry) %>% 
                  mutate(site = "EXT") %>% 
                  select(site, species))

pp2 <- read_csv("C:/Users/poniitty/OneDrive - University of Helsinki/Kesa2023/All_species_points_2023.csv") %>% 
  filter(endsWith(Comment, " N") | grepl(" NAYTE ", Comment) | grepl("trait", Comment) | grepl("EXT", Comment) | grepl("TRAIT", Comment)) %>% 
  st_as_sf(coords = c("X","Y"), crs = 32634) %>% 
  st_transform(crs = 4326) %>% 
  rename(geom = geometry) %>% 
  filter(!species %in% c("trait","TRAIT")) %>% 
  mutate(species = ifelse(species == "lavsib", "lacalp", species)) %>% 
  mutate(species = ifelse(species == "JATTI", "verlob", species)) %>% 
  mutate(species = ifelse(species == "STELLA", "stenem", species)) %>% 
  mutate(species = tolower(gsub("EXT-","",species))) %>% 
  mutate(site = "EXT")

nam <- read_csv2("env_data/all_species_abbr_edited_2.csv")

unique(pp2$species)[!unique(pp2$species) %in% nam$abbr]

pp2 <- pp2 %>% left_join(., nam, by = c("species" = "abbr")) %>% 
  select(-species) %>% rename(species = name) %>% 
  select(site, species)

pp3 <- read_csv("C:/Users/poniitty/OneDrive - University of Helsinki/Kesä2024/All_trait_points_2024.csv") %>% 
  st_as_sf(coords = c("X","Y"), crs = 32634) %>% 
  st_transform(4326) %>% 
  rename(geom = geometry) %>% 
  rename(date = GPS_Date) %>% 
  filter(!(site == "EXT-SAXCER" & Z > 693.5 & date == "2024-07-23")) %>% 
  select(site, date, Z) %>%
  filter(!startsWith(site, "EXT17")) %>% 
  filter(!startsWith(site, "RA3")) %>% 
  mutate(site = gsub("EXT-","",site)) %>% 
  mutate(site = gsub(" TRT2","",site)) %>% 
  mutate(site = gsub(" TRT","",site)) %>% 
  mutate(site = tolower(gsub(" TRAIT","",site))) %>% 
  rename(species = site) %>% 
  mutate(site = "EXT") %>% 
  mutate(species = ifelse(species == "akankaali", "erystr", species)) %>% 
  mutate(species = ifelse(species == "antealp", "antalp", species)) %>% 
  mutate(species = ifelse(species == "cetalp", "ceralp", species)) %>% 
  mutate(species = ifelse(species == "saniainen", "dryexp", species)) %>% 
  mutate(species = ifelse(species == "saniainen2", "dryfil", species)) %>% 
  mutate(species = ifelse(species == "hentos", "tripal", species)) %>% 
  mutate(species = ifelse(species == "psastr", "psestr", species)) %>% 
  mutate(species = ifelse(species == "erivag" & date == "2024-07-23", "erisch", species)) %>% 
  mutate(species = ifelse(species == "ranhyp" & date == "2024-07-25", "ranrep", species)) %>% 
  mutate(species = ifelse(species == "galium" & date == "2024-07-21", "galuli", species)) %>% 
  mutate(site = ifelse(species == "erisch" & date == "2024-07-23" & Z > 650, "EXT2", site)) %>% 
  mutate(site = ifelse(species == "saxcer" & date == "2024-07-23" & Z > 690, "EXT2", site)) %>% 
  mutate(site = ifelse(species == "sildio" & date == "2024-07-21" & Z < 500, "EXT2", site)) %>% 
  mutate(site = ifelse(species == "petfri" & date == "2024-07-31" & Z < 700, "EXT2", site))

unique(pp3$species)[!unique(pp3$species) %in% nam$abbr]

pp3 <- pp3 %>% left_join(., nam, by = c("species" = "abbr")) %>% 
  select(-species) %>% rename(species = name) %>% 
  select(site, date, species)

pp <- read.table("C:/Users/poniitty/OneDrive - Jyväskylän yliopisto/Kesä2023/earlier_data/species_points/species_points_2022.csv", sep = ";", dec = ".", header = T) %>% 
  filter(grepl("trait", Comment) | grepl("EXT", Comment) | grepl("TRAIT", Comment)) %>% 
  st_as_sf(coords = c("X","Y"), crs = 32634) %>% 
  st_transform(crs = 4326) %>% 
  rename(geom = geometry) %>% 
  mutate(site = "EXT") %>% 
  select(site, Comment, species, GPS_Date, GPS_Time) %>% 
  filter(GPS_Date != "15.7.2022") %>% 
  filter(!(species == "Micranthes stellaris" & GPS_Date == "1.8.2022")) %>% 
  mutate(site = ifelse(species == "Carex glacialis" & GPS_Date == "10.7.2022","EXT1",site)) %>% 
  mutate(site = ifelse(species == "Carex parallela" & GPS_Date == "10.7.2022","EXT1",site)) %>% 
  mutate(species = ifelse(species == "Draba norvegica" & GPS_Date == "22.7.2022","Draba daurica",species)) %>% 
  mutate(species = ifelse(species == "Woodsia glabella" & GPS_Date == "12.8.2022","Woodsia alpina",species)) %>% 
  mutate(site = ifelse(species == "Ribes spicatum" & GPS_Date == "26.7.2022","EXT1",site)) %>% 
  mutate(site = ifelse(species == "Rubus idaeus" & as.character(GPS_Time) == "22:22:16","EXTSAANA",site)) %>% 
  mutate(site = ifelse(species == "Rubus idaeus" & as.character(GPS_Time) == "23:30:02","EXTRETKU",site)) %>% 
  mutate(site = ifelse(species == "Micranthes foliolosa" & Comment == "SAXFOLEXT2240722","EXT2",site)) %>% 
  mutate(site = ifelse(species == "Micranthes foliolosa" & Comment == "SAXFOLEXT240722","EXT1",site)) %>% 
  mutate(site = ifelse(species == "Melampyrum pratense" & GPS_Date == "10.7.2022","EXT1",site))

pp <- bind_rows(pp %>% mutate(site = ifelse(species == "Rhododendron lapponicum" & GPS_Date == "10.7.2022","EXT1",site)),
                pp %>% filter(species == "Rhododendron lapponicum" & GPS_Date == "10.7.2022") %>% mutate(site = "EXT2"))
pp <- bind_rows(pp %>% mutate(site = ifelse(species == "Euphrasia salisburgensis" & GPS_Date == "10.7.2022","EXT1",site)),
                pp %>% filter(species == "Euphrasia salisburgensis" & GPS_Date == "10.7.2022") %>% mutate(site = "EXT2"))
pp <- bind_rows(pp %>% mutate(site = ifelse(species == "Draba fladnizensis" & GPS_Date == "22.7.2022","EXT1",site)),
                pp %>% filter(species == "Draba fladnizensis" & GPS_Date == "22.7.2022") %>% mutate(site = "EXT2"))

pp <- bind_rows(pp1, pp %>% select(site, species))


unique(d7$site[which(!d7$site %in% pp$site)])

d7 <- left_join(d7 %>% filter(year(date) < 2023),
                bind_cols(pp %>% st_drop_geometry(),
                          pp %>% st_coordinates() %>% as.data.frame())) %>% 
  bind_rows(.,
            left_join(d7 %>% filter(year(date) == 2023),
                      bind_cols(pp2 %>% st_drop_geometry(),
                                pp2 %>% st_coordinates() %>% as.data.frame()))) %>% 
  bind_rows(., 
            left_join(d7 %>% filter(year(date) == 2024),
                      bind_cols(pp3 %>% st_drop_geometry(),
                                pp3 %>% st_coordinates() %>% as.data.frame()))) %>% 
  rename(Lat = Y, Lon = X) %>% 
  relocate(Lat, Lon, .after = species) %>% 
  mutate(setting = "EXT")

d7 %>% filter(is.na(Lon)) %>% as.data.frame()

#############################################################
# Combine all

d <- bind_rows(d1, d2, d3, d4, d5, d6, d7, d8) %>% 
  mutate(setting = ifelse(startsWith(site, "EXT"), "EXT", setting))

d %>% filter(is.na(Lat))

table(d$setting)

d <- d %>% 
  mutate(setting = ifelse(setting == "geo", "KIL_X", setting),
         setting = ifelse(setting == "itvgrids", "KIL_ITV", setting),
         setting = ifelse(setting == "mikkuna", "KIL_MI", setting),
         setting = ifelse(setting == "rarearctic", "KIL_RA", setting),
         setting = ifelse(setting == "RAREARCTIC", "KIL_RA", setting),
         setting = ifelse(setting == "roslin", "KIL_ROS", setting),
         setting = ifelse(setting == "traitexp", "KIL_EXP", setting),
         setting = ifelse(startsWith(site, "EXT"), "EXT", setting),
         setting = ifelse(startsWith(site, "MAL"), "KIL_MAL", setting),
         setting = ifelse(startsWith(site, "AIL"), "KIL_AIL", setting),
         setting = ifelse(startsWith(site, "SE"), "KIL_EXP", setting),
         setting = ifelse(startsWith(site, "AE"), "KIL_EXP", setting),
         setting = ifelse(startsWith(site, "RA0"), "KIL_RA", setting),
         setting = ifelse(startsWith(site, "RA1"), "KIL_RA", setting),
         setting = ifelse(startsWith(site, "RA2"), "KIL_RA", setting),
         setting = ifelse(startsWith(site, "RA3"), "KIL_RA", setting),
         setting = ifelse(startsWith(site, "SAL"), "KIL_RA", setting),
         setting = ifelse(startsWith(site, "MI"), "KIL_MI", setting),
         setting = ifelse(startsWith(site, "L"), "KIL_L", setting),
         setting = ifelse(startsWith(site, "BA"), "KIL_BUG", setting),
         setting = ifelse(startsWith(site, "BB"), "KIL_BUG", setting),
         setting = ifelse(startsWith(site, "KUA"), "KIL_RA", setting),
         setting = ifelse(startsWith(site, "X"), "KIL_X", setting),
         setting = ifelse(startsWith(site, "BB"), "KIL_BUG", setting),
         setting = ifelse(grepl("^A[[:digit:]]", site), "KIL_ITV", setting),
         setting = ifelse(grepl("^B[[:digit:]]", site), "KIL_ITV", setting),
         setting = ifelse(grepl("^C[[:digit:]]", site), "KIL_ITV", setting),
         setting = ifelse(grepl("^D[[:digit:]]", site), "KIL_ITV", setting),
         setting = ifelse(grepl("^E[[:digit:]]", site), "KIL_ITV", setting),
         setting = ifelse(grepl("^F[[:digit:]]", site), "KIL_ITV", setting)) %>% 
  mutate(plot_type = ifelse(startsWith(site, "BA"), "b", plot_type),
         plot_type = ifelse(startsWith(site, "BB"), "b", plot_type),
         plot_type = ifelse(startsWith(site, "EXT"), "d", plot_type),
         plot_type = ifelse(grepl("^A[[:digit:]]", site), "b", plot_type),
         plot_type = ifelse(grepl("^B[[:digit:]]", site), "b", plot_type),
         plot_type = ifelse(grepl("^C[[:digit:]]", site), "b", plot_type),
         plot_type = ifelse(grepl("^D[[:digit:]]", site), "b", plot_type),
         plot_type = ifelse(grepl("^E[[:digit:]]", site), "b", plot_type),
         plot_type = ifelse(grepl("^F[[:digit:]]", site), "b", plot_type),
         plot_type = ifelse(startsWith(site, "MI") & year(date) %in% c(2021:2022,2024), "b", plot_type))

d <- d %>% filter(!(site == "B2" & year(date) == 2022))

table(d$setting)
d %>% 
  filter(is.na(setting))
d %>% 
  filter(is.na(plot_type))

# Fix names

nam <- read_csv2("env_data/all_species_abbr_edited_2.csv")
syns <- readxl::read_xlsx("env_data/synonyms.xlsx")

unique(d$species)[!unique(d$species) %in% nam$name]

d <- d %>% 
  mutate(species = str_squish(species)) %>% 
  left_join(., syns) %>% 
  mutate(species = ifelse(!is.na(truename), truename, species)) %>% 
  select(-truename)

unique(d$species)[!unique(d$species) %in% nam$name]

d %>% 
  group_by(species) %>% 
  count %>% 
  arrange(species) %>% as.data.frame()

d <- d %>% 
  mutate(SLA = SLA/10,
         w_weight = w_weight/n_leaf,
         d_weight = d_weight/n_leaf)

d <- d %>% 
  mutate(LDMC = ifelse(LDMC > 0.9, NA, LDMC)) %>% 
  select(-n_leaf_image, -area_sum, -area_sd)

# Write out semi-wide format data
write_csv(d, "trait_data/All_vascular_2016-2024_wide.csv")

# create site x date table

d <- read_csv("trait_data/All_vascular_2016-2024_wide.csv") %>% 
  filter(year(date) >= 2020) %>% 
  filter(!startsWith(site, "EXT"),
         !startsWith(site, "KUA")) %>% 
  filter(!setting %in% c("KIL_BUG","KIL_ITV"))

d <- d %>% 
  select(site, setting, date) %>% 
  unique() %>% 
  mutate(site2 = site) %>% 
  mutate(site2 = ifelse(setting == "KIL_EXP", substr(site2, 1, 4), site2)) %>% 
  relocate(site, site2)

write_csv(d, "output/sitedates.csv")
#######################################################
# Data to long format

d <- read_csv("trait_data/All_vascular_2016-2024_wide.csv")

heights <- d %>% 
  select(site:max_height) %>% 
  pivot_longer(cols = c(cvr:max_height), names_to = "trait") %>% 
  mutate(unit = ifelse(trait == "cvr", "%", "cm")) %>% 
  filter(!is.na(value))

d %>% filter(plot_type %in% c("c")) %>% pull(reproduction) %>% table

repro <- d %>% 
  select(site:Lon, reproduction) %>% 
  pivot_longer(cols = reproduction, names_to = "trait") %>% 
  filter(!is.na(value)) %>% 
  mutate(plot_type = factor(ifelse(is.na(plot_type), "d", plot_type), levels = c("c","b","a","d"))) %>%
  arrange(desc(site), date, species, plot_type) %>% 
  group_by(site, setting, date, species, Lat, Lon, trait) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  mutate(unit = "unitless") %>% 
  mutate(plot_type = as.character(plot_type))


leafs <- d %>% 
  select(site:Lon, n_inds:d_weight, leaf_area, SLA, LDMC) %>% 
  pivot_longer(cols = w_weight:LDMC, names_to = "trait") %>% 
  filter(!is.na(value)) %>% 
  mutate(plot_type = factor(ifelse(is.na(plot_type), "d", plot_type), levels = c("b","c","a","d"))) %>%
  arrange(desc(site), date, species, plot_type) %>% 
  group_by(site, setting, date, species, Lat, Lon, n_inds, n_leaf, trait, value) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  mutate(unit = ifelse(trait == "LDMC", "g/g", NA),
         unit = ifelse(trait == "SLA", "mm2/mg", unit),
         unit = ifelse(trait == "d_weight", "g", unit),
         unit = ifelse(trait == "w_weight", "g", unit),
         unit = ifelse(trait == "leaf_area", "cm2", unit)) %>% 
  mutate(plot_type = as.character(plot_type))

dl <- bind_rows(heights, repro, leafs) %>% 
  arrange(setting, site, species, trait)

d %>% filter(species == "Harrimanella hypnoides", LDMC > 0.9)
d %>% filter(species == "Alchemilla borealis")

d %>% 
  group_by(species) %>% 
  summarise(across(c(median_height,max_height,leaf_area,SLA,LDMC), min, na.rm = T)) %>% 
  as.data.frame() %>% view

dl %>% group_by(trait) %>% count
dl %>% group_by(setting) %>% count
dl %>% mutate(setting = ifelse(startsWith(setting, "KIL"), "KIL", setting)) %>% group_by(setting) %>% count
d %>% filter(!is.na(LDMC)) %>% filter(is.na(d_weight)) %>% pull(site) %>% unique

