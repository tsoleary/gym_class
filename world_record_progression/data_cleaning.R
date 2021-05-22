# ------------------------------------------------------------------------------
# Data Cleaning -- Progression World Athletics Records for Men and Women
# April 29, 2021
# TS O'Leary
# ------------------------------------------------------------------------------

# Load libraries -----
require(tidyverse)

# Load data -----
setwd(here::here("world_record_progression/data"))
files <- list.files()

# Create null df
df <- NULL

# Load all files in data/ and bind rows
for (i in 1:length(files)) {
  temp_df <- read_csv(files[i], 
                      col_types = cols(.default = "c")) %>%
    mutate(file = files[i])
  df <- bind_rows(df, temp_df)
}


# Get event and sex from file name -----
df <- df %>%
  separate(file, 
           into = c("event", "sex", "note"), 
           sep = "_")

# Clean Time column ---- Yikes ----
y <- df %>%
  mutate(Time = str_remove_all(Time, "\\(.+\\)")) %>%
  mutate(Time = str_remove_all(Time, "\\s*\\+*")) %>%
  mutate(Time = str_remove_all(Time, "\\[.+\\]")) %>%
  mutate(Time = str_remove_all(Time, "[:alpha:]")) %>%
  mutate(colon = str_count(Time, "\\:"),
         decimal = str_count(Time, "\\.")) %>%
  mutate(Time = ifelse(decimal == 0, paste0(Time, ".00"), Time)) %>%
  mutate(Time = ifelse(colon == 0, paste0("00:00:", Time), Time)) %>%
  mutate(Time = ifelse(colon == 1, paste0("00:", Time), Time)) %>%
  separate(Time, into = c("h", "m", "s"), sep = ":") %>%
  separate(s, into = c("s", "ms"), sep = "\\.") %>%
  mutate(mili = str_length(ms),
         mins = str_length(m),
         secs = str_length(s),
         hrs = str_length(h)) %>%
  mutate(h = ifelse(hrs == 1, paste0("0", h), h)) %>%
  mutate(m = ifelse(mins == 1, paste0("0", m), m)) %>%
  mutate(s = ifelse(secs == 1, paste0("0", s), s)) %>%
  mutate(ms = ifelse(mili == 1, paste0(ms, "0"), ms)) %>%
  mutate(Time = paste(h, m, s, sep = ":")) %>%
  mutate(Time = paste(Time, ms, sep = ".")) %>%
  mutate(Athlete = ifelse(is.na(Athlete), Name, Athlete)) %>%
  mutate(Date = str_remove_all(Date, "\\s*\\[.+\\]"))

# Save df in .csv
write_csv(y %>%
  select(Date, h, m, s, ms, Time, Wind, Auto, event, sex, Athlete, Nationality) %>%
    arrange(Date),
  here::here("world_record_progression/temp_dat.csv"))

# What a forking nightmare ... bit of date cleaning behind the scenes ----
dat <- read_csv(here::here("world_record_progression/temp_dat.csv"))

#  More date cleaning with code ----
x <- dat %>%
  filter(str_detect(Date, "-")) %>%
  mutate(Date = as.Date(Date))

v <- dat %>%
  filter(str_detect(Date, ",")) %>%
  mutate(Date = as.Date(Date, format = "%B %d, %Y"))

x <- bind_rows(x,v)

# Time conversion to seconds and normalization -----
df_time <- x %>%
  mutate(secs = as.numeric(paste(s, ms, sep = ".")),
         hrs = as.numeric(h),
         mins = as.numeric(m)) %>%
  mutate(time_s = hrs*60*60 + mins*60 + secs) %>%
  mutate(time_m = time_s/60) %>%
  mutate(time_h = time_s/(60*60)) 

df_norm <- df_time %>%
  group_by(event, sex) %>%
  mutate(time_norm = time_s/min(time_s))

# Save df
write_csv(df_norm %>%
            mutate(Time = as.character(paste(paste(h, m, s, sep = ":"), 
                                             ms, sep = "."))) %>%
  select(Date, event, sex, Time, Wind, 
         Auto, Athlete, Nationality, contains("time_")) %>%
  arrange(event, sex, Date),
  here::here("world_record_progression/world_record_progression_data.csv"))

# Load final data
dat <- read_csv(here::here("world_record_progression/world_record_progression_data.csv"), 
                col_types = cols(Time = "c"))

# Quickly clean nationality -----
d <- dat %>%
  filter(str_detect(Athlete, "\\([:alpha:]{3}\\)")) %>%
  separate(Athlete, into = c("Athlete", "Country"), sep = "\\(") %>%
  mutate(Country = str_remove_all(Country, "\\)"))

df_cc <- read_csv(here::here("world_record_progression/country_codes.csv"))

d <- left_join(d, df_cc %>% select(`alpha-3`, name), 
               by = c("Country" = "alpha-3"))

d <- d %>%
  mutate(Nationality = name)

d2 <- dat %>%
  filter(!is.na(Nationality))

x <- bind_rows(d2, d) %>%
  select(-c("Country", "name"))

write_csv(x, here::here("world_record_progression/world_record_progression_data.csv"))


# Descending only times

df <- read_csv(
  here::here("world_record_progression/world_record_progression_data.csv"), 
  col_types = cols(Time = "c")
) 

# Filter data to have only the fastest records regarless of source (IAAF etc)
dat <- df %>%
  group_by(event, sex) %>%
  group_split()


keep <- vector(mode = "logical", length = nrow(df))
new_list <- vector(mode = "list", length = length(dat))
row <-  0
for (i in 1:length(dat)) {
  
  dat_df <- dat[[i]]
  dat_df$keep <- TRUE
  row <- row + 1
  
  
  for (j in 2:nrow(dat_df)) {
    
    dat_df$keep[j] <- as.logical(dat_df[j,"time_s"] <= min(dat_df[2:j,"time_s"]))
  }
  new_list[[i]] <- dat_df
}

dat <- bind_rows(new_list)

dat <- dat %>%
  filter(keep == TRUE)

write_csv(dat, here::here("world_record_progression/world_record_progression_data_desc.csv"))