# ------------------------------------------------------------------------------
# Gym Class - NYT Upshot map
# February 23, 2021
# TS O'Leary
# ------------------------------------------------------------------------------

# Load packages
require(tidyverse)
require(plotly)

# Load data ---> "No Data" replaced with NA
df <- read_delim(here::here("us_counties/data/nyt_upshot_county_data.tsv"), 
                 delim = "\t")

# Rank data
dat <- df %>% 
  mutate(rank_income = min_rank(desc(income)),
         rank_education = min_rank(desc(education)),
         rank_life = min_rank(desc(life)),
         rank_unemployment = min_rank(unemployment),
         rank_disability = min_rank(disability),
         rank_unemployment = min_rank(unemployment)) %>%
  pivot_longer(contains("rank_"), 
               names_to = "type", 
               values_to = "rank_n")  %>%
  mutate(weight = case_when(type == "rank_disability" ~ 1,
                            type == "rank_income" ~ 1,
                            type == "rank_obesity" ~ 1,
                            type == "rank_unemployment" ~ 1,
                            type == "rank_education" ~ 1,
                            type == "rank_life" ~ 1)) %>%
  group_by(id) %>%
  summarise(rank_avg = weighted.mean(rank_n, weight)) %>%
  mutate(rank_me = min_rank(rank_avg))

dat <- full_join(df, dat)









counties <- rjson::fromJSON(file='https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json')


g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = col2rgb('white')
)



fig <- plot_ly() %>% 
  add_trace(
    type = "choropleth",
    geojson = counties,
    locations = str_pad(dat$id, 5, pad = "0"),
    z = dat$rank,
    colorscale = "Viridis",
    zmin = 1,
    zmax = max(dat$rank, na.rm = TRUE),
    text = dat$County,
    marker = list(line = list(width = 0))) %>% 
  colorbar(title = "Rank") %>% 
  layout(title = "Best county in US") %>% 
  layout(geo = g)

fig


# Other data import and merge --------------------------------------------------

pop <- read_delim(here::here("us_counties/data/clean/pop_est.csv"), 
                 delim = ",") %>%
  mutate(id = str_pad(id, 5, pad = "0"))

pov <- read_delim(here::here("us_counties/data/clean/poverty_data.csv"), 
                  delim = ",") %>%
  mutate(id = str_pad(id, 5, pad = "0"))

land <- read_delim(here::here("us_counties/data/clean/land_area.csv"), 
                  delim = ",") %>%
  mutate(id = str_pad(id, 5, pad = "0"))

sun <- read_delim(here::here("us_counties/data/clean/sun.txt"), 
                  delim = "\t") %>%
  mutate(id = str_pad(id, 5, pad = "0"))

precip <- read_delim(here::here("us_counties/data/clean/precip.txt"), 
                  delim = "\t") %>%
  mutate(id = str_pad(id, 5, pad = "0"))

temp <- read_delim(here::here("us_counties/data/clean/temp.txt"), 
                     delim = "\t") %>%
  mutate(id = str_pad(id, 5, pad = "0")) %>%
  filter(Month %in% c("Jan", "Jul")) %>%
  select(id, Month, daily_max_air_temp_f) %>%
  pivot_wider(names_from = Month, values_from = daily_max_air_temp_f) %>%
  rename(temp_jan = Jan, temp_jul = Jul)


comb_df <- full_join(pop, pov) %>%
  full_join(land) %>%
  full_join(sun) %>%
  full_join(precip) %>%
  full_join(temp)

#UMORE UGLY non reproducible combining stuff ....

write_tsv(df_x, here::here("us_counties/data/clean/combined_data.tsv"))


# Load the combined df ----
df <- read_delim(here::here("us_counties/data/clean/combined_data.tsv"), 
           delim = "\t")
