library(tidyverse)

cal_funNA <- function(x) {((-1.34e-8) * (x^2) + (2.50e-4) * x + (-1.58e-1))*100 }

d <- read_csv("output/sitedates.csv")

############################################
# Kilpisjärvi

dt <- d %>% 
  filter(startsWith(setting, "KIL")) %>% 
  select(-site) %>% 
  rename(site = site2) %>% 
  unique()

cl <- read_csv("/scratch/project_2007415/temp/tomst_data_imputed.csv") %>% 
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2")) %>% 
  filter(!is.na(datetime)) %>% 
  filter(!is.na(site))

dt <- dt %>% 
  filter(site %in% unique(cl$site))

# Calibrate the moisture values
cl <- cl %>% 
  mutate(moist = round(cal_funNA(moist),1))

cl <- cl %>% 
  mutate(year = year(datetime))
gc()

KIL <- lapply(seq_len(nrow(dt)), function(x){
  # x <- 1
  dtt <- dt %>% slice(x)
  print(dtt$site)
  
  clt <- cl %>% 
    filter(site == dtt$site,
           year == year(dtt$date)) %>% 
    mutate(date = as_date(datetime)) %>% 
    filter(date <= dtt$date)
  
  clt <- clt %>% 
    mutate(date = as_date(datetime)) %>% 
    group_by(date) %>% 
    summarise(T1 = mean(T1, na.rm = TRUE),
              T3 = mean(T3, na.rm = TRUE),
              moist = mean(moist, na.rm = TRUE),
              T1_imp = mean(T1_imp, na.rm = TRUE),
              T3_imp = mean(T3_imp, na.rm = TRUE),
              moist_imp = mean(moist_imp, na.rm = TRUE))
  
  res <- bind_rows(tibble(var = "TDD", sensor = "T1", period = "moment",
                          value = sum(ifelse(clt$T1 < 0, 0, clt$T1)),
                          imputed_percentage = mean(ifelse(clt$T1 < 0, NA, clt$T1_imp), na.rm = TRUE)),
                   tibble(var = "TDD_days", sensor = "T1", period = "moment",
                          value = sum(ifelse(clt$T1 < 0, 0, 1)),
                          imputed_percentage = mean(ifelse(clt$T1 < 0, NA, clt$T1_imp), na.rm = TRUE)),
                   tibble(var = "TDD", sensor = "T3", period = "moment",
                          value = sum(ifelse(clt$T3 < 0, 0, clt$T3)),
                          imputed_percentage = mean(ifelse(clt$T3 < 0, NA, clt$T3_imp), na.rm = TRUE)),
                   tibble(var = "TDD_days", sensor = "T3", period = "moment",
                          value = sum(ifelse(clt$T3 < 0, 0, 1)),
                          imputed_percentage = mean(ifelse(clt$T3 < 0, NA, clt$T3_imp), na.rm = TRUE)),
                   
                   tibble(var = "GDD3", sensor = "T1", period = "moment",
                          value = sum(ifelse(clt$T1 < 3, 0, clt$T1)),
                          imputed_percentage = mean(ifelse(clt$T1 < 3, NA, clt$T1_imp), na.rm = TRUE)),
                   tibble(var = "GDD3_days", sensor = "T1", period = "moment",
                          value = sum(ifelse(clt$T1 < 3, 0, 1)),
                          imputed_percentage = mean(ifelse(clt$T1 < 3, NA, clt$T1_imp), na.rm = TRUE)),
                   tibble(var = "GDD3", sensor = "T3", period = "moment",
                          value = sum(ifelse(clt$T3 < 3, 0, clt$T3)),
                          imputed_percentage = mean(ifelse(clt$T3 < 3, NA, clt$T3_imp), na.rm = TRUE)),
                   tibble(var = "GDD3_days", sensor = "T3", period = "moment",
                          value = sum(ifelse(clt$T3 < 3, 0, 1)),
                          imputed_percentage = mean(ifelse(clt$T3 < 3, NA, clt$T3_imp), na.rm = TRUE)),
                   
                   tibble(var = "GMD", sensor = "moist", period = "moment",
                          value = sum(clt$moist, na.rm = TRUE),
                          imputed_percentage = mean(clt$moist_imp, na.rm = TRUE)),
                   
                   tibble(var = "date_mean", sensor = "T1", period = "moment",
                          value = clt %>% filter(date == dtt$date) %>% pull(T1),
                          imputed_percentage = clt %>% filter(date == dtt$date) %>% pull(T1_imp)),
                   tibble(var = "date_mean", sensor = "T3", period = "moment",
                          value = clt %>% filter(date == dtt$date) %>% pull(T3),
                          imputed_percentage = clt %>% filter(date == dtt$date) %>% pull(T3_imp)),
                   tibble(var = "date_mean", sensor = "moist", period = "moment",
                          value = clt %>% filter(date == dtt$date) %>% pull(moist),
                          imputed_percentage = clt %>% filter(date == dtt$date) %>% pull(moist_imp)),
                   
                   tibble(var = "week_mean", sensor = "T1", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(7) & date <= dtt$date) %>% pull(T1), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T1_imp, 7), na.rm = TRUE)),
                   tibble(var = "week_mean", sensor = "T3", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(7) & date <= dtt$date) %>% pull(T3), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T3_imp, 7), na.rm = TRUE)),
                   tibble(var = "week_mean", sensor = "moist", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(7) & date <= dtt$date) %>% pull(moist), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$moist_imp, 7), na.rm = TRUE)),
                   
                   tibble(var = "fortnight_mean", sensor = "T1", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(14) & date <= dtt$date) %>% pull(T1), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T1_imp, 14), na.rm = TRUE)),
                   tibble(var = "fortnight_mean", sensor = "T3", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(14) & date <= dtt$date) %>% pull(T3), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T3_imp, 14), na.rm = TRUE)),
                   tibble(var = "fortnight_mean", sensor = "moist", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(14) & date <= dtt$date) %>% pull(moist), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$moist_imp, 14), na.rm = TRUE)),
                   
                   tibble(var = "month_mean", sensor = "T1", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(28) & date <= dtt$date) %>% pull(T1), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T1_imp, 28), na.rm = TRUE)),
                   tibble(var = "month_mean", sensor = "T3", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(28) & date <= dtt$date) %>% pull(T3), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T3_imp, 28), na.rm = TRUE)),
                   tibble(var = "month_mean", sensor = "moist", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(28) & date <= dtt$date) %>% pull(moist), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$moist_imp, 28), na.rm = TRUE))
                   
  ) %>% 
    mutate(site = dtt$site,
           date = dtt$date) %>% 
    relocate(site, date)
  
  if((res %>% filter(var == "date_mean" & sensor == "T1") %>% nrow) == 0){
    res <- bind_rows(res,
                     tibble(var = "date_mean", sensor = "T1", period = "moment",
                            value = clt %>% filter(date == dtt$date - days(1)) %>% pull(T1),
                            imputed_percentage = clt %>% filter(date == dtt$date - days(1)) %>% pull(T1_imp))) %>% 
      mutate(site = dtt$site,
             date = dtt$date) %>% 
      relocate(site, date)
  }
  if((res %>% filter(var == "date_mean" & sensor == "T3") %>% nrow) == 0){
    res <- bind_rows(res,
                     tibble(var = "date_mean", sensor = "T3", period = "moment",
                            value = clt %>% filter(date == dtt$date - days(1)) %>% pull(T3),
                            imputed_percentage = clt %>% filter(date == dtt$date - days(1)) %>% pull(T3_imp))) %>% 
      mutate(site = dtt$site,
             date = dtt$date) %>% 
      relocate(site, date)
  }
  if((res %>% filter(var == "date_mean" & sensor == "moist") %>% nrow) == 0){
    res <- bind_rows(res,
                     tibble(var = "date_mean", sensor = "moist", period = "moment",
                            value = clt %>% filter(date == dtt$date - days(1)) %>% pull(moist),
                            imputed_percentage = clt %>% filter(date == dtt$date - days(1)) %>% pull(moist_imp))) %>% 
      mutate(site = dtt$site,
             date = dtt$date) %>% 
      relocate(site, date)
  }
  
  return(res)
  
}) %>% bind_rows()
rm(cl)
gc()
############################################################
# Värriö

dt <- d %>% 
  filter(startsWith(setting, "VAR")) %>% 
  select(-site) %>% 
  rename(site = site2) %>% 
  unique()

cl <- read_csv("/projappl/project_2007415/repos/MICROCLIMATES/output/VAR/tomst_data_imputed.csv") %>% 
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2")) %>% 
  filter(!is.na(datetime)) %>% 
  filter(!is.na(site))

dt <- dt %>% 
  filter(site %in% unique(cl$site))

# Calibrate the moisture values
cl <- cl %>% 
  mutate(moist = round(cal_funNA(moist),1))

cl <- cl %>% 
  mutate(year = year(datetime))
gc()

VAR <- lapply(seq_len(nrow(dt)), function(x){
  # x <- 1
  dtt <- dt %>% slice(x)
  print(dtt$site)
  
  clt <- cl %>% 
    filter(site == dtt$site,
           year == year(dtt$date)) %>% 
    mutate(date = as_date(datetime)) %>% 
    filter(date <= dtt$date)
  
  clt <- clt %>% 
    mutate(date = as_date(datetime)) %>% 
    group_by(date) %>% 
    summarise(T1 = mean(T1, na.rm = TRUE),
              T3 = mean(T3, na.rm = TRUE),
              moist = mean(moist, na.rm = TRUE),
              T1_imp = mean(T1_imp, na.rm = TRUE),
              T3_imp = mean(T3_imp, na.rm = TRUE),
              moist_imp = mean(moist_imp, na.rm = TRUE))
  
  res <- bind_rows(tibble(var = "TDD", sensor = "T1", period = "moment",
                          value = sum(ifelse(clt$T1 < 0, 0, clt$T1)),
                          imputed_percentage = mean(ifelse(clt$T1 < 0, NA, clt$T1_imp), na.rm = TRUE)),
                   tibble(var = "TDD_days", sensor = "T1", period = "moment",
                          value = sum(ifelse(clt$T1 < 0, 0, 1)),
                          imputed_percentage = mean(ifelse(clt$T1 < 0, NA, clt$T1_imp), na.rm = TRUE)),
                   tibble(var = "TDD", sensor = "T3", period = "moment",
                          value = sum(ifelse(clt$T3 < 0, 0, clt$T3)),
                          imputed_percentage = mean(ifelse(clt$T3 < 0, NA, clt$T3_imp), na.rm = TRUE)),
                   tibble(var = "TDD_days", sensor = "T3", period = "moment",
                          value = sum(ifelse(clt$T3 < 0, 0, 1)),
                          imputed_percentage = mean(ifelse(clt$T3 < 0, NA, clt$T3_imp), na.rm = TRUE)),
                   
                   tibble(var = "GDD3", sensor = "T1", period = "moment",
                          value = sum(ifelse(clt$T1 < 3, 0, clt$T1)),
                          imputed_percentage = mean(ifelse(clt$T1 < 3, NA, clt$T1_imp), na.rm = TRUE)),
                   tibble(var = "GDD3_days", sensor = "T1", period = "moment",
                          value = sum(ifelse(clt$T1 < 3, 0, 1)),
                          imputed_percentage = mean(ifelse(clt$T1 < 3, NA, clt$T1_imp), na.rm = TRUE)),
                   tibble(var = "GDD3", sensor = "T3", period = "moment",
                          value = sum(ifelse(clt$T3 < 3, 0, clt$T3)),
                          imputed_percentage = mean(ifelse(clt$T3 < 3, NA, clt$T3_imp), na.rm = TRUE)),
                   tibble(var = "GDD3_days", sensor = "T3", period = "moment",
                          value = sum(ifelse(clt$T3 < 3, 0, 1)),
                          imputed_percentage = mean(ifelse(clt$T3 < 3, NA, clt$T3_imp), na.rm = TRUE)),
                   
                   tibble(var = "GMD", sensor = "moist", period = "moment",
                          value = sum(clt$moist, na.rm = TRUE),
                          imputed_percentage = mean(clt$moist_imp, na.rm = TRUE)),
                   
                   tibble(var = "date_mean", sensor = "T1", period = "moment",
                          value = clt %>% filter(date == dtt$date) %>% pull(T1),
                          imputed_percentage = clt %>% filter(date == dtt$date) %>% pull(T1_imp)),
                   tibble(var = "date_mean", sensor = "T3", period = "moment",
                          value = clt %>% filter(date == dtt$date) %>% pull(T3),
                          imputed_percentage = clt %>% filter(date == dtt$date) %>% pull(T3_imp)),
                   tibble(var = "date_mean", sensor = "moist", period = "moment",
                          value = clt %>% filter(date == dtt$date) %>% pull(moist),
                          imputed_percentage = clt %>% filter(date == dtt$date) %>% pull(moist_imp)),
                   
                   tibble(var = "week_mean", sensor = "T1", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(7) & date <= dtt$date) %>% pull(T1), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T1_imp, 7), na.rm = TRUE)),
                   tibble(var = "week_mean", sensor = "T3", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(7) & date <= dtt$date) %>% pull(T3), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T3_imp, 7), na.rm = TRUE)),
                   tibble(var = "week_mean", sensor = "moist", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(7) & date <= dtt$date) %>% pull(moist), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$moist_imp, 7), na.rm = TRUE)),
                   
                   tibble(var = "fortnight_mean", sensor = "T1", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(14) & date <= dtt$date) %>% pull(T1), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T1_imp, 14), na.rm = TRUE)),
                   tibble(var = "fortnight_mean", sensor = "T3", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(14) & date <= dtt$date) %>% pull(T3), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T3_imp, 14), na.rm = TRUE)),
                   tibble(var = "fortnight_mean", sensor = "moist", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(14) & date <= dtt$date) %>% pull(moist), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$moist_imp, 14), na.rm = TRUE)),
                   
                   tibble(var = "month_mean", sensor = "T1", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(28) & date <= dtt$date) %>% pull(T1), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T1_imp, 28), na.rm = TRUE)),
                   tibble(var = "month_mean", sensor = "T3", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(28) & date <= dtt$date) %>% pull(T3), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T3_imp, 28), na.rm = TRUE)),
                   tibble(var = "month_mean", sensor = "moist", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(28) & date <= dtt$date) %>% pull(moist), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$moist_imp, 28), na.rm = TRUE))
                   
  ) %>% 
    mutate(site = dtt$site,
           date = dtt$date) %>% 
    relocate(site, date)
  
  if((res %>% filter(var == "date_mean" & sensor == "T1") %>% nrow) == 0){
    res <- bind_rows(res,
                     tibble(var = "date_mean", sensor = "T1", period = "moment",
                            value = clt %>% filter(date == dtt$date - days(1)) %>% pull(T1),
                            imputed_percentage = clt %>% filter(date == dtt$date - days(1)) %>% pull(T1_imp))) %>% 
      mutate(site = dtt$site,
             date = dtt$date) %>% 
      relocate(site, date)
  }
  if((res %>% filter(var == "date_mean" & sensor == "T3") %>% nrow) == 0){
    res <- bind_rows(res,
                     tibble(var = "date_mean", sensor = "T3", period = "moment",
                            value = clt %>% filter(date == dtt$date - days(1)) %>% pull(T3),
                            imputed_percentage = clt %>% filter(date == dtt$date - days(1)) %>% pull(T3_imp))) %>% 
      mutate(site = dtt$site,
             date = dtt$date) %>% 
      relocate(site, date)
  }
  if((res %>% filter(var == "date_mean" & sensor == "moist") %>% nrow) == 0){
    res <- bind_rows(res,
                     tibble(var = "date_mean", sensor = "moist", period = "moment",
                            value = clt %>% filter(date == dtt$date - days(1)) %>% pull(moist),
                            imputed_percentage = clt %>% filter(date == dtt$date - days(1)) %>% pull(moist_imp))) %>% 
      mutate(site = dtt$site,
             date = dtt$date) %>% 
      relocate(site, date)
  }
  
  return(res)
  
}) %>% bind_rows()

rm(cl)
gc()
############################################################
# Oulanka

dt <- d %>% 
  filter(startsWith(setting, "OUL")) %>% 
  select(-site) %>% 
  rename(site = site2) %>% 
  unique()

cl <- read_csv("/projappl/project_2007415/repos/MICROCLIMATES/output/OUL/tomst_data_imputed.csv") %>% 
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2")) %>% 
  filter(!is.na(datetime)) %>% 
  filter(!is.na(site))

dt <- dt %>% 
  filter(site %in% unique(cl$site))

# Calibrate the moisture values
cl <- cl %>% 
  mutate(moist = round(cal_funNA(moist),1))

cl <- cl %>% 
  mutate(year = year(datetime))
gc()

OUL <- lapply(seq_len(nrow(dt)), function(x){
  # x <- 1
  dtt <- dt %>% slice(x)
  print(dtt$site)
  
  clt <- cl %>% 
    filter(site == dtt$site,
           year == year(dtt$date)) %>% 
    mutate(date = as_date(datetime)) %>% 
    filter(date <= dtt$date)
  
  clt <- clt %>% 
    mutate(date = as_date(datetime)) %>% 
    group_by(date) %>% 
    summarise(T1 = mean(T1, na.rm = TRUE),
              T3 = mean(T3, na.rm = TRUE),
              moist = mean(moist, na.rm = TRUE),
              T1_imp = mean(T1_imp, na.rm = TRUE),
              T3_imp = mean(T3_imp, na.rm = TRUE),
              moist_imp = mean(moist_imp, na.rm = TRUE))
  
  res <- bind_rows(tibble(var = "TDD", sensor = "T1", period = "moment",
                          value = sum(ifelse(clt$T1 < 0, 0, clt$T1)),
                          imputed_percentage = mean(ifelse(clt$T1 < 0, NA, clt$T1_imp), na.rm = TRUE)),
                   tibble(var = "TDD_days", sensor = "T1", period = "moment",
                          value = sum(ifelse(clt$T1 < 0, 0, 1)),
                          imputed_percentage = mean(ifelse(clt$T1 < 0, NA, clt$T1_imp), na.rm = TRUE)),
                   tibble(var = "TDD", sensor = "T3", period = "moment",
                          value = sum(ifelse(clt$T3 < 0, 0, clt$T3)),
                          imputed_percentage = mean(ifelse(clt$T3 < 0, NA, clt$T3_imp), na.rm = TRUE)),
                   tibble(var = "TDD_days", sensor = "T3", period = "moment",
                          value = sum(ifelse(clt$T3 < 0, 0, 1)),
                          imputed_percentage = mean(ifelse(clt$T3 < 0, NA, clt$T3_imp), na.rm = TRUE)),
                   
                   tibble(var = "GDD3", sensor = "T1", period = "moment",
                          value = sum(ifelse(clt$T1 < 3, 0, clt$T1)),
                          imputed_percentage = mean(ifelse(clt$T1 < 3, NA, clt$T1_imp), na.rm = TRUE)),
                   tibble(var = "GDD3_days", sensor = "T1", period = "moment",
                          value = sum(ifelse(clt$T1 < 3, 0, 1)),
                          imputed_percentage = mean(ifelse(clt$T1 < 3, NA, clt$T1_imp), na.rm = TRUE)),
                   tibble(var = "GDD3", sensor = "T3", period = "moment",
                          value = sum(ifelse(clt$T3 < 3, 0, clt$T3)),
                          imputed_percentage = mean(ifelse(clt$T3 < 3, NA, clt$T3_imp), na.rm = TRUE)),
                   tibble(var = "GDD3_days", sensor = "T3", period = "moment",
                          value = sum(ifelse(clt$T3 < 3, 0, 1)),
                          imputed_percentage = mean(ifelse(clt$T3 < 3, NA, clt$T3_imp), na.rm = TRUE)),
                   
                   tibble(var = "GMD", sensor = "moist", period = "moment",
                          value = sum(clt$moist, na.rm = TRUE),
                          imputed_percentage = mean(clt$moist_imp, na.rm = TRUE)),
                   
                   tibble(var = "date_mean", sensor = "T1", period = "moment",
                          value = clt %>% filter(date == dtt$date) %>% pull(T1),
                          imputed_percentage = clt %>% filter(date == dtt$date) %>% pull(T1_imp)),
                   tibble(var = "date_mean", sensor = "T3", period = "moment",
                          value = clt %>% filter(date == dtt$date) %>% pull(T3),
                          imputed_percentage = clt %>% filter(date == dtt$date) %>% pull(T3_imp)),
                   tibble(var = "date_mean", sensor = "moist", period = "moment",
                          value = clt %>% filter(date == dtt$date) %>% pull(moist),
                          imputed_percentage = clt %>% filter(date == dtt$date) %>% pull(moist_imp)),
                   
                   tibble(var = "week_mean", sensor = "T1", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(7) & date <= dtt$date) %>% pull(T1), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T1_imp, 7), na.rm = TRUE)),
                   tibble(var = "week_mean", sensor = "T3", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(7) & date <= dtt$date) %>% pull(T3), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T3_imp, 7), na.rm = TRUE)),
                   tibble(var = "week_mean", sensor = "moist", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(7) & date <= dtt$date) %>% pull(moist), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$moist_imp, 7), na.rm = TRUE)),
                   
                   tibble(var = "fortnight_mean", sensor = "T1", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(14) & date <= dtt$date) %>% pull(T1), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T1_imp, 14), na.rm = TRUE)),
                   tibble(var = "fortnight_mean", sensor = "T3", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(14) & date <= dtt$date) %>% pull(T3), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T3_imp, 14), na.rm = TRUE)),
                   tibble(var = "fortnight_mean", sensor = "moist", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(14) & date <= dtt$date) %>% pull(moist), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$moist_imp, 14), na.rm = TRUE)),
                   
                   tibble(var = "month_mean", sensor = "T1", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(28) & date <= dtt$date) %>% pull(T1), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T1_imp, 28), na.rm = TRUE)),
                   tibble(var = "month_mean", sensor = "T3", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(28) & date <= dtt$date) %>% pull(T3), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T3_imp, 28), na.rm = TRUE)),
                   tibble(var = "month_mean", sensor = "moist", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(28) & date <= dtt$date) %>% pull(moist), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$moist_imp, 28), na.rm = TRUE))
                   
  ) %>% 
    mutate(site = dtt$site,
           date = dtt$date) %>% 
    relocate(site, date)
  
  if((res %>% filter(var == "date_mean" & sensor == "T1") %>% nrow) == 0){
    res <- bind_rows(res,
                     tibble(var = "date_mean", sensor = "T1", period = "moment",
                            value = clt %>% filter(date == dtt$date - days(1)) %>% pull(T1),
                            imputed_percentage = clt %>% filter(date == dtt$date - days(1)) %>% pull(T1_imp))) %>% 
      mutate(site = dtt$site,
             date = dtt$date) %>% 
      relocate(site, date)
  }
  if((res %>% filter(var == "date_mean" & sensor == "T3") %>% nrow) == 0){
    res <- bind_rows(res,
                     tibble(var = "date_mean", sensor = "T3", period = "moment",
                            value = clt %>% filter(date == dtt$date - days(1)) %>% pull(T3),
                            imputed_percentage = clt %>% filter(date == dtt$date - days(1)) %>% pull(T3_imp))) %>% 
      mutate(site = dtt$site,
             date = dtt$date) %>% 
      relocate(site, date)
  }
  if((res %>% filter(var == "date_mean" & sensor == "moist") %>% nrow) == 0){
    res <- bind_rows(res,
                     tibble(var = "date_mean", sensor = "moist", period = "moment",
                            value = clt %>% filter(date == dtt$date - days(1)) %>% pull(moist),
                            imputed_percentage = clt %>% filter(date == dtt$date - days(1)) %>% pull(moist_imp))) %>% 
      mutate(site = dtt$site,
             date = dtt$date) %>% 
      relocate(site, date)
  }
  
  return(res)
  
}) %>% bind_rows()

rm(cl)
gc()
############################################################
# Mattavarri

dt <- d %>% 
  filter(startsWith(setting, "MAT")) %>% 
  select(-site) %>% 
  rename(site = site2) %>% 
  unique()

cl <- read_csv("/projappl/project_2007415/repos/MICROCLIMATES/output/MAT/tomst_data_imputed.csv") %>% 
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2")) %>% 
  filter(!is.na(datetime)) %>% 
  filter(!is.na(site))

dt <- dt %>% 
  filter(site %in% unique(cl$site))

# Calibrate the moisture values
cl <- cl %>% 
  mutate(moist = round(cal_funNA(moist),1))

cl <- cl %>% 
  mutate(year = year(datetime))
gc()

MAT <- lapply(seq_len(nrow(dt)), function(x){
  # x <- 1
  dtt <- dt %>% slice(x)
  print(dtt$site)
  
  clt <- cl %>% 
    filter(site == dtt$site,
           year == year(dtt$date)) %>% 
    mutate(date = as_date(datetime)) %>% 
    filter(date <= dtt$date)
  
  clt <- clt %>% 
    mutate(date = as_date(datetime)) %>% 
    group_by(date) %>% 
    summarise(T1 = mean(T1, na.rm = TRUE),
              T3 = mean(T3, na.rm = TRUE),
              moist = mean(moist, na.rm = TRUE),
              T1_imp = mean(T1_imp, na.rm = TRUE),
              T3_imp = mean(T3_imp, na.rm = TRUE),
              moist_imp = mean(moist_imp, na.rm = TRUE))
  
  res <- bind_rows(tibble(var = "TDD", sensor = "T1", period = "moment",
                          value = sum(ifelse(clt$T1 < 0, 0, clt$T1)),
                          imputed_percentage = mean(ifelse(clt$T1 < 0, NA, clt$T1_imp), na.rm = TRUE)),
                   tibble(var = "TDD_days", sensor = "T1", period = "moment",
                          value = sum(ifelse(clt$T1 < 0, 0, 1)),
                          imputed_percentage = mean(ifelse(clt$T1 < 0, NA, clt$T1_imp), na.rm = TRUE)),
                   tibble(var = "TDD", sensor = "T3", period = "moment",
                          value = sum(ifelse(clt$T3 < 0, 0, clt$T3)),
                          imputed_percentage = mean(ifelse(clt$T3 < 0, NA, clt$T3_imp), na.rm = TRUE)),
                   tibble(var = "TDD_days", sensor = "T3", period = "moment",
                          value = sum(ifelse(clt$T3 < 0, 0, 1)),
                          imputed_percentage = mean(ifelse(clt$T3 < 0, NA, clt$T3_imp), na.rm = TRUE)),
                   
                   tibble(var = "GDD3", sensor = "T1", period = "moment",
                          value = sum(ifelse(clt$T1 < 3, 0, clt$T1)),
                          imputed_percentage = mean(ifelse(clt$T1 < 3, NA, clt$T1_imp), na.rm = TRUE)),
                   tibble(var = "GDD3_days", sensor = "T1", period = "moment",
                          value = sum(ifelse(clt$T1 < 3, 0, 1)),
                          imputed_percentage = mean(ifelse(clt$T1 < 3, NA, clt$T1_imp), na.rm = TRUE)),
                   tibble(var = "GDD3", sensor = "T3", period = "moment",
                          value = sum(ifelse(clt$T3 < 3, 0, clt$T3)),
                          imputed_percentage = mean(ifelse(clt$T3 < 3, NA, clt$T3_imp), na.rm = TRUE)),
                   tibble(var = "GDD3_days", sensor = "T3", period = "moment",
                          value = sum(ifelse(clt$T3 < 3, 0, 1)),
                          imputed_percentage = mean(ifelse(clt$T3 < 3, NA, clt$T3_imp), na.rm = TRUE)),
                   
                   tibble(var = "GMD", sensor = "moist", period = "moment",
                          value = sum(clt$moist, na.rm = TRUE),
                          imputed_percentage = mean(clt$moist_imp, na.rm = TRUE)),
                   
                   tibble(var = "date_mean", sensor = "T1", period = "moment",
                          value = clt %>% filter(date == dtt$date) %>% pull(T1),
                          imputed_percentage = clt %>% filter(date == dtt$date) %>% pull(T1_imp)),
                   tibble(var = "date_mean", sensor = "T3", period = "moment",
                          value = clt %>% filter(date == dtt$date) %>% pull(T3),
                          imputed_percentage = clt %>% filter(date == dtt$date) %>% pull(T3_imp)),
                   tibble(var = "date_mean", sensor = "moist", period = "moment",
                          value = clt %>% filter(date == dtt$date) %>% pull(moist),
                          imputed_percentage = clt %>% filter(date == dtt$date) %>% pull(moist_imp)),
                   
                   tibble(var = "week_mean", sensor = "T1", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(7) & date <= dtt$date) %>% pull(T1), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T1_imp, 7), na.rm = TRUE)),
                   tibble(var = "week_mean", sensor = "T3", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(7) & date <= dtt$date) %>% pull(T3), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T3_imp, 7), na.rm = TRUE)),
                   tibble(var = "week_mean", sensor = "moist", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(7) & date <= dtt$date) %>% pull(moist), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$moist_imp, 7), na.rm = TRUE)),
                   
                   tibble(var = "fortnight_mean", sensor = "T1", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(14) & date <= dtt$date) %>% pull(T1), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T1_imp, 14), na.rm = TRUE)),
                   tibble(var = "fortnight_mean", sensor = "T3", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(14) & date <= dtt$date) %>% pull(T3), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T3_imp, 14), na.rm = TRUE)),
                   tibble(var = "fortnight_mean", sensor = "moist", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(14) & date <= dtt$date) %>% pull(moist), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$moist_imp, 14), na.rm = TRUE)),
                   
                   tibble(var = "month_mean", sensor = "T1", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(28) & date <= dtt$date) %>% pull(T1), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T1_imp, 28), na.rm = TRUE)),
                   tibble(var = "month_mean", sensor = "T3", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(28) & date <= dtt$date) %>% pull(T3), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T3_imp, 28), na.rm = TRUE)),
                   tibble(var = "month_mean", sensor = "moist", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(28) & date <= dtt$date) %>% pull(moist), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$moist_imp, 28), na.rm = TRUE))
                   
  ) %>% 
    mutate(site = dtt$site,
           date = dtt$date) %>% 
    relocate(site, date)
  
  if((res %>% filter(var == "date_mean" & sensor == "T1") %>% nrow) == 0){
    res <- bind_rows(res,
                     tibble(var = "date_mean", sensor = "T1", period = "moment",
                            value = clt %>% filter(date == dtt$date - days(1)) %>% pull(T1),
                            imputed_percentage = clt %>% filter(date == dtt$date - days(1)) %>% pull(T1_imp))) %>% 
      mutate(site = dtt$site,
             date = dtt$date) %>% 
      relocate(site, date)
  }
  if((res %>% filter(var == "date_mean" & sensor == "T3") %>% nrow) == 0){
    res <- bind_rows(res,
                     tibble(var = "date_mean", sensor = "T3", period = "moment",
                            value = clt %>% filter(date == dtt$date - days(1)) %>% pull(T3),
                            imputed_percentage = clt %>% filter(date == dtt$date - days(1)) %>% pull(T3_imp))) %>% 
      mutate(site = dtt$site,
             date = dtt$date) %>% 
      relocate(site, date)
  }
  if((res %>% filter(var == "date_mean" & sensor == "moist") %>% nrow) == 0){
    res <- bind_rows(res,
                     tibble(var = "date_mean", sensor = "moist", period = "moment",
                            value = clt %>% filter(date == dtt$date - days(1)) %>% pull(moist),
                            imputed_percentage = clt %>% filter(date == dtt$date - days(1)) %>% pull(moist_imp))) %>% 
      mutate(site = dtt$site,
             date = dtt$date) %>% 
      relocate(site, date)
  }
  
  return(res)
  
}) %>% bind_rows()
rm(cl)
gc()
############################################################
# Rastigaisa

dt <- d %>% 
  filter(startsWith(setting, "RAS")) %>% 
  select(-site) %>% 
  rename(site = site2) %>% 
  unique()

cl <- read_csv("/projappl/project_2007415/repos/MICROCLIMATES/output/RAS/tomst_data_imputed.csv") %>% 
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2")) %>% 
  filter(!is.na(datetime)) %>% 
  filter(!is.na(site))

dt <- dt %>% 
  filter(site %in% unique(cl$site))

# Calibrate the moisture values
cl <- cl %>% 
  mutate(moist = round(cal_funNA(moist),1))

cl <- cl %>% 
  mutate(year = year(datetime))
gc()

RAS <- lapply(seq_len(nrow(dt)), function(x){
  # x <- 1
  dtt <- dt %>% slice(x)
  print(dtt$site)
  
  clt <- cl %>% 
    filter(site == dtt$site,
           year == year(dtt$date)) %>% 
    mutate(date = as_date(datetime)) %>% 
    filter(date <= dtt$date)
  
  clt <- clt %>% 
    mutate(date = as_date(datetime)) %>% 
    group_by(date) %>% 
    summarise(T1 = mean(T1, na.rm = TRUE),
              T3 = mean(T3, na.rm = TRUE),
              moist = mean(moist, na.rm = TRUE),
              T1_imp = mean(T1_imp, na.rm = TRUE),
              T3_imp = mean(T3_imp, na.rm = TRUE),
              moist_imp = mean(moist_imp, na.rm = TRUE))
  
  res <- bind_rows(tibble(var = "TDD", sensor = "T1", period = "moment",
                          value = sum(ifelse(clt$T1 < 0, 0, clt$T1)),
                          imputed_percentage = mean(ifelse(clt$T1 < 0, NA, clt$T1_imp), na.rm = TRUE)),
                   tibble(var = "TDD_days", sensor = "T1", period = "moment",
                          value = sum(ifelse(clt$T1 < 0, 0, 1)),
                          imputed_percentage = mean(ifelse(clt$T1 < 0, NA, clt$T1_imp), na.rm = TRUE)),
                   tibble(var = "TDD", sensor = "T3", period = "moment",
                          value = sum(ifelse(clt$T3 < 0, 0, clt$T3)),
                          imputed_percentage = mean(ifelse(clt$T3 < 0, NA, clt$T3_imp), na.rm = TRUE)),
                   tibble(var = "TDD_days", sensor = "T3", period = "moment",
                          value = sum(ifelse(clt$T3 < 0, 0, 1)),
                          imputed_percentage = mean(ifelse(clt$T3 < 0, NA, clt$T3_imp), na.rm = TRUE)),
                   
                   tibble(var = "GDD3", sensor = "T1", period = "moment",
                          value = sum(ifelse(clt$T1 < 3, 0, clt$T1)),
                          imputed_percentage = mean(ifelse(clt$T1 < 3, NA, clt$T1_imp), na.rm = TRUE)),
                   tibble(var = "GDD3_days", sensor = "T1", period = "moment",
                          value = sum(ifelse(clt$T1 < 3, 0, 1)),
                          imputed_percentage = mean(ifelse(clt$T1 < 3, NA, clt$T1_imp), na.rm = TRUE)),
                   tibble(var = "GDD3", sensor = "T3", period = "moment",
                          value = sum(ifelse(clt$T3 < 3, 0, clt$T3)),
                          imputed_percentage = mean(ifelse(clt$T3 < 3, NA, clt$T3_imp), na.rm = TRUE)),
                   tibble(var = "GDD3_days", sensor = "T3", period = "moment",
                          value = sum(ifelse(clt$T3 < 3, 0, 1)),
                          imputed_percentage = mean(ifelse(clt$T3 < 3, NA, clt$T3_imp), na.rm = TRUE)),
                   
                   tibble(var = "GMD", sensor = "moist", period = "moment",
                          value = sum(clt$moist, na.rm = TRUE),
                          imputed_percentage = mean(clt$moist_imp, na.rm = TRUE)),
                   
                   tibble(var = "date_mean", sensor = "T1", period = "moment",
                          value = clt %>% filter(date == dtt$date) %>% pull(T1),
                          imputed_percentage = clt %>% filter(date == dtt$date) %>% pull(T1_imp)),
                   tibble(var = "date_mean", sensor = "T3", period = "moment",
                          value = clt %>% filter(date == dtt$date) %>% pull(T3),
                          imputed_percentage = clt %>% filter(date == dtt$date) %>% pull(T3_imp)),
                   tibble(var = "date_mean", sensor = "moist", period = "moment",
                          value = clt %>% filter(date == dtt$date) %>% pull(moist),
                          imputed_percentage = clt %>% filter(date == dtt$date) %>% pull(moist_imp)),
                   
                   tibble(var = "week_mean", sensor = "T1", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(7) & date <= dtt$date) %>% pull(T1), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T1_imp, 7), na.rm = TRUE)),
                   tibble(var = "week_mean", sensor = "T3", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(7) & date <= dtt$date) %>% pull(T3), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T3_imp, 7), na.rm = TRUE)),
                   tibble(var = "week_mean", sensor = "moist", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(7) & date <= dtt$date) %>% pull(moist), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$moist_imp, 7), na.rm = TRUE)),
                   
                   tibble(var = "fortnight_mean", sensor = "T1", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(14) & date <= dtt$date) %>% pull(T1), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T1_imp, 14), na.rm = TRUE)),
                   tibble(var = "fortnight_mean", sensor = "T3", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(14) & date <= dtt$date) %>% pull(T3), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T3_imp, 14), na.rm = TRUE)),
                   tibble(var = "fortnight_mean", sensor = "moist", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(14) & date <= dtt$date) %>% pull(moist), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$moist_imp, 14), na.rm = TRUE)),
                   
                   tibble(var = "month_mean", sensor = "T1", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(28) & date <= dtt$date) %>% pull(T1), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T1_imp, 28), na.rm = TRUE)),
                   tibble(var = "month_mean", sensor = "T3", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(28) & date <= dtt$date) %>% pull(T3), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T3_imp, 28), na.rm = TRUE)),
                   tibble(var = "month_mean", sensor = "moist", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(28) & date <= dtt$date) %>% pull(moist), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$moist_imp, 28), na.rm = TRUE))
                   
  ) %>% 
    mutate(site = dtt$site,
           date = dtt$date) %>% 
    relocate(site, date)
  
  if((res %>% filter(var == "date_mean" & sensor == "T1") %>% nrow) == 0){
    res <- bind_rows(res,
                     tibble(var = "date_mean", sensor = "T1", period = "moment",
                            value = clt %>% filter(date == dtt$date - days(1)) %>% pull(T1),
                            imputed_percentage = clt %>% filter(date == dtt$date - days(1)) %>% pull(T1_imp))) %>% 
      mutate(site = dtt$site,
             date = dtt$date) %>% 
      relocate(site, date)
  }
  if((res %>% filter(var == "date_mean" & sensor == "T3") %>% nrow) == 0){
    res <- bind_rows(res,
                     tibble(var = "date_mean", sensor = "T3", period = "moment",
                            value = clt %>% filter(date == dtt$date - days(1)) %>% pull(T3),
                            imputed_percentage = clt %>% filter(date == dtt$date - days(1)) %>% pull(T3_imp))) %>% 
      mutate(site = dtt$site,
             date = dtt$date) %>% 
      relocate(site, date)
  }
  if((res %>% filter(var == "date_mean" & sensor == "moist") %>% nrow) == 0){
    res <- bind_rows(res,
                     tibble(var = "date_mean", sensor = "moist", period = "moment",
                            value = clt %>% filter(date == dtt$date - days(1)) %>% pull(moist),
                            imputed_percentage = clt %>% filter(date == dtt$date - days(1)) %>% pull(moist_imp))) %>% 
      mutate(site = dtt$site,
             date = dtt$date) %>% 
      relocate(site, date)
  }
  
  return(res)
  
}) %>% bind_rows()
rm(cl)
gc()
############################################################
# Vindelfjällen

dt <- d %>% 
  filter(startsWith(setting, "VIN")) %>% 
  select(-site) %>% 
  rename(site = site2) %>% 
  unique()

cl <- read_csv("/projappl/project_2007415/repos/MICROCLIMATES/output/VIN/tomst_data_imputed.csv") %>% 
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2")) %>% 
  filter(!is.na(datetime)) %>% 
  filter(!is.na(site))

dt <- dt %>% 
  filter(site %in% unique(cl$site))

# Calibrate the moisture values
cl <- cl %>% 
  mutate(moist = round(cal_funNA(moist),1))

cl <- cl %>% 
  mutate(year = year(datetime))
gc()

VIN <- lapply(seq_len(nrow(dt)), function(x){
  # x <- 1
  dtt <- dt %>% slice(x)
  print(dtt$site)
  
  clt <- cl %>% 
    filter(site == dtt$site,
           year == year(dtt$date)) %>% 
    mutate(date = as_date(datetime)) %>% 
    filter(date <= dtt$date)
  
  clt <- clt %>% 
    mutate(date = as_date(datetime)) %>% 
    group_by(date) %>% 
    summarise(T1 = mean(T1, na.rm = TRUE),
              T3 = mean(T3, na.rm = TRUE),
              moist = mean(moist, na.rm = TRUE),
              T1_imp = mean(T1_imp, na.rm = TRUE),
              T3_imp = mean(T3_imp, na.rm = TRUE),
              moist_imp = mean(moist_imp, na.rm = TRUE))
  
  res <- bind_rows(tibble(var = "TDD", sensor = "T1", period = "moment",
                          value = sum(ifelse(clt$T1 < 0, 0, clt$T1)),
                          imputed_percentage = mean(ifelse(clt$T1 < 0, NA, clt$T1_imp), na.rm = TRUE)),
                   tibble(var = "TDD_days", sensor = "T1", period = "moment",
                          value = sum(ifelse(clt$T1 < 0, 0, 1)),
                          imputed_percentage = mean(ifelse(clt$T1 < 0, NA, clt$T1_imp), na.rm = TRUE)),
                   tibble(var = "TDD", sensor = "T3", period = "moment",
                          value = sum(ifelse(clt$T3 < 0, 0, clt$T3)),
                          imputed_percentage = mean(ifelse(clt$T3 < 0, NA, clt$T3_imp), na.rm = TRUE)),
                   tibble(var = "TDD_days", sensor = "T3", period = "moment",
                          value = sum(ifelse(clt$T3 < 0, 0, 1)),
                          imputed_percentage = mean(ifelse(clt$T3 < 0, NA, clt$T3_imp), na.rm = TRUE)),
                   
                   tibble(var = "GDD3", sensor = "T1", period = "moment",
                          value = sum(ifelse(clt$T1 < 3, 0, clt$T1)),
                          imputed_percentage = mean(ifelse(clt$T1 < 3, NA, clt$T1_imp), na.rm = TRUE)),
                   tibble(var = "GDD3_days", sensor = "T1", period = "moment",
                          value = sum(ifelse(clt$T1 < 3, 0, 1)),
                          imputed_percentage = mean(ifelse(clt$T1 < 3, NA, clt$T1_imp), na.rm = TRUE)),
                   tibble(var = "GDD3", sensor = "T3", period = "moment",
                          value = sum(ifelse(clt$T3 < 3, 0, clt$T3)),
                          imputed_percentage = mean(ifelse(clt$T3 < 3, NA, clt$T3_imp), na.rm = TRUE)),
                   tibble(var = "GDD3_days", sensor = "T3", period = "moment",
                          value = sum(ifelse(clt$T3 < 3, 0, 1)),
                          imputed_percentage = mean(ifelse(clt$T3 < 3, NA, clt$T3_imp), na.rm = TRUE)),
                   
                   tibble(var = "GMD", sensor = "moist", period = "moment",
                          value = sum(clt$moist, na.rm = TRUE),
                          imputed_percentage = mean(clt$moist_imp, na.rm = TRUE)),
                   
                   tibble(var = "date_mean", sensor = "T1", period = "moment",
                          value = clt %>% filter(date == dtt$date) %>% pull(T1),
                          imputed_percentage = clt %>% filter(date == dtt$date) %>% pull(T1_imp)),
                   tibble(var = "date_mean", sensor = "T3", period = "moment",
                          value = clt %>% filter(date == dtt$date) %>% pull(T3),
                          imputed_percentage = clt %>% filter(date == dtt$date) %>% pull(T3_imp)),
                   tibble(var = "date_mean", sensor = "moist", period = "moment",
                          value = clt %>% filter(date == dtt$date) %>% pull(moist),
                          imputed_percentage = clt %>% filter(date == dtt$date) %>% pull(moist_imp)),
                   
                   tibble(var = "week_mean", sensor = "T1", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(7) & date <= dtt$date) %>% pull(T1), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T1_imp, 7), na.rm = TRUE)),
                   tibble(var = "week_mean", sensor = "T3", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(7) & date <= dtt$date) %>% pull(T3), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T3_imp, 7), na.rm = TRUE)),
                   tibble(var = "week_mean", sensor = "moist", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(7) & date <= dtt$date) %>% pull(moist), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$moist_imp, 7), na.rm = TRUE)),
                   
                   tibble(var = "fortnight_mean", sensor = "T1", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(14) & date <= dtt$date) %>% pull(T1), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T1_imp, 14), na.rm = TRUE)),
                   tibble(var = "fortnight_mean", sensor = "T3", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(14) & date <= dtt$date) %>% pull(T3), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T3_imp, 14), na.rm = TRUE)),
                   tibble(var = "fortnight_mean", sensor = "moist", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(14) & date <= dtt$date) %>% pull(moist), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$moist_imp, 14), na.rm = TRUE)),
                   
                   tibble(var = "month_mean", sensor = "T1", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(28) & date <= dtt$date) %>% pull(T1), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T1_imp, 28), na.rm = TRUE)),
                   tibble(var = "month_mean", sensor = "T3", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(28) & date <= dtt$date) %>% pull(T3), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$T3_imp, 28), na.rm = TRUE)),
                   tibble(var = "month_mean", sensor = "moist", period = "moment",
                          value = mean(clt %>% filter(date > dtt$date - days(28) & date <= dtt$date) %>% pull(moist), na.rm = TRUE),
                          imputed_percentage = mean(tail(clt$moist_imp, 28), na.rm = TRUE))
                   
  ) %>% 
    mutate(site = dtt$site,
           date = dtt$date) %>% 
    relocate(site, date)
  
  if((res %>% filter(var == "date_mean" & sensor == "T1") %>% nrow) == 0){
    res <- bind_rows(res,
                     tibble(var = "date_mean", sensor = "T1", period = "moment",
                            value = clt %>% filter(date == dtt$date - days(1)) %>% pull(T1),
                            imputed_percentage = clt %>% filter(date == dtt$date - days(1)) %>% pull(T1_imp))) %>% 
      mutate(site = dtt$site,
             date = dtt$date) %>% 
      relocate(site, date)
  }
  if((res %>% filter(var == "date_mean" & sensor == "T3") %>% nrow) == 0){
    res <- bind_rows(res,
                     tibble(var = "date_mean", sensor = "T3", period = "moment",
                            value = clt %>% filter(date == dtt$date - days(1)) %>% pull(T3),
                            imputed_percentage = clt %>% filter(date == dtt$date - days(1)) %>% pull(T3_imp))) %>% 
      mutate(site = dtt$site,
             date = dtt$date) %>% 
      relocate(site, date)
  }
  if((res %>% filter(var == "date_mean" & sensor == "moist") %>% nrow) == 0){
    res <- bind_rows(res,
                     tibble(var = "date_mean", sensor = "moist", period = "moment",
                            value = clt %>% filter(date == dtt$date - days(1)) %>% pull(moist),
                            imputed_percentage = clt %>% filter(date == dtt$date - days(1)) %>% pull(moist_imp))) %>% 
      mutate(site = dtt$site,
             date = dtt$date) %>% 
      relocate(site, date)
  }
  
  return(res)
  
}) %>% bind_rows()
rm(cl)
gc()
#####################################
# Combine

all <- bind_rows(KIL,
                 VAR,
                 OUL,
                 RAS,
                 MAT,
                 VIN)

all %>% write_csv("env_data/microclimate_vars_moments.csv")


#################################################################
# Long term variables

d <- read_csv("output/sitedates.csv")%>% 
  select(-site) %>% 
  rename(site = site2) %>% 
  unique()

cl <- bind_rows(read_csv("/projappl/project_2007415/repos/MICROCLIMATES/output/KIL/tomst_temperature_variables.csv"),
                read_csv("/projappl/project_2007415/repos/MICROCLIMATES/output/OUL/tomst_temperature_variables.csv"),
                read_csv("/projappl/project_2007415/repos/MICROCLIMATES/output/VAR/tomst_temperature_variables.csv"),
                read_csv("/projappl/project_2007415/repos/MICROCLIMATES/output/MAT/tomst_temperature_variables.csv"),
                read_csv("/projappl/project_2007415/repos/MICROCLIMATES/output/VIN/tomst_temperature_variables.csv"),
                read_csv("/projappl/project_2007415/repos/MICROCLIMATES/output/RAS/tomst_temperature_variables.csv"),
                read_csv("/projappl/project_2007415/repos/MICROCLIMATES/output/PAL/tomst_temperature_variables.csv")) %>% 
  filter(year >= 2020) %>% 
  filter(sensor != "T2")

m <- bind_rows(read_csv("/projappl/project_2007415/repos/MICROCLIMATES/output/KIL/tomst_moisture_variables.csv"),
               read_csv("/projappl/project_2007415/repos/MICROCLIMATES/output/OUL/tomst_moisture_variables.csv"),
               read_csv("/projappl/project_2007415/repos/MICROCLIMATES/output/VAR/tomst_moisture_variables.csv"),
               read_csv("/projappl/project_2007415/repos/MICROCLIMATES/output/MAT/tomst_moisture_variables.csv"),
               read_csv("/projappl/project_2007415/repos/MICROCLIMATES/output/VIN/tomst_moisture_variables.csv"),
               read_csv("/projappl/project_2007415/repos/MICROCLIMATES/output/RAS/tomst_moisture_variables.csv"),
               read_csv("/projappl/project_2007415/repos/MICROCLIMATES/output/PAL/tomst_moisture_variables.csv")) %>% 
  filter(year >= 2020) %>% 
  filter(moist_type == "moist") %>% 
  rename(sensor = moist_type)

cl <- bind_rows(cl, m) %>% 
  filter(site %in% unique(d$site))

cl <- cl %>% 
  mutate(variable = paste(sensor, var, period, sep = "_"))

unique(cl$variable)

# Select which you want
selected <- c("T3_max_annual","T1_min_annual","T1_mean_annual",
              "T3_mean_Jun","T3_mean_Jul","T1_mean_Jun","T1_mean_Jul","T1_min_Jun","T3_min_Jun",
              "T1_start_of_season_growing_season","T3_start_of_season_growing_season",
              "T1_freeze_thaw_freq_annual","T3_freeze_thaw_freq_annual",
              "T3_GDD3_annual","T3_TDD_annual","T3_FDD_annual","T1_GDD3_annual","T1_TDD_annual",
              "moist_median_Jun","moist_median_Jul","moist_mean_Jun","moist_mean_Jul","moist_iqr90_growing_season")

cl <- cl %>% 
  filter(variable %in% selected) %>% 
  pivot_wider(id_cols = c(site,year), names_from = variable, values_from = value)

ds <- cl %>% 
  group_by(site) %>% 
  summarise(across(-year, ~mean(.x, na.rm = T)))

summary(ds)
ds %>% filter(is.na(moist_mean_Jun))
cor(ds %>% select(-site), use = "pairwise.complete.obs") %>% round(2)

ds %>% write_csv("env_data/microclimate_vars.csv")

