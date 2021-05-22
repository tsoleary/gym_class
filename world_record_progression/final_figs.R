# ------------------------------------------------------------------------------
# Final Figure Examples
# May 10, 2021
# TS O'Leary
# ------------------------------------------------------------------------------


# Load Libraries
require(tidyverse)
require(plotly)

# Load data
df_r <- read_csv(here::here("world_record_progression/ratio.csv"))
dat <- read_csv(here::here("world_record_progression/world_record_progression_data.csv"), 
                col_types = cols(Time = "c"))


# Ratio figure -----
df_r <- df_r %>%
  mutate(w_m_ratio = w/m) %>%
  filter(decade >= 1960) 

# Reorder the events from shortest to longest
df_r$event <- factor(df_r$event, levels = c("100", 
                                            "400", 
                                            "1500",
                                            "5000",
                                            "10000",
                                            "marathon"))

# Plot
df_r %>%
  ggplot() + 
  geom_line(aes(x = decade, y = w_m_ratio, color = event)) +
  geom_point(aes(x = decade, y = w_m_ratio, fill = event), 
             shape = 21, 
             color = "grey50", 
             size = 3, 
             alpha = 0.8) +
  labs(x = "Decade") +
  scale_fill_brewer(palette = 2, 
                    name = "Event",
                    labels = c("100m", 
                               "400m", 
                               "1500m",
                               "5000m",
                               "10000m",
                               "Marathon")) +
  scale_color_brewer(palette = 2, 
                     name = "Event",
                     labels = c("100m", 
                                "400m", 
                                "1500m",
                                "5000m",
                                "10000m",
                                "Marathon")) +
  scale_y_continuous(name = "Record Ratio\nWomen to Men", 
                     breaks = c(1.0, 1.1, 1.2, 1.3, 1.4, 1.5), 
                     labels = c(1.0, 1.1, 1.2, 1.3, 1.4, 1.5), 
                     limits = c(0.95, 1.5)) + 
  scale_x_continuous(name = "Decade", 
                     breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey20") +
  theme_classic()


# Non-linear fits figures -----

# Normalize data to the fastest time (male) 
dat <- dat %>%
  group_by(event) %>%
  mutate(time_norm_male = time_s/min(time_s))

# Reorder the events from shortest to longest
dat$event <- factor(dat$event, levels = c("100", 
                                          "400", 
                                          "1500",
                                          "5000",
                                          "10000",
                                          "marathon"))


# Clean data to include only the fastest times to date 
dat %>%
  ggplot() +
  geom_point(aes(x = Date, 
                 y = time_norm, 
                 fill = event, 
                 shape = sex),
             color = "grey50", 
             alpha = 0.8,
             size = 2) +
  scale_fill_brewer(palette = 2, 
                    name = "Event",
                    labels = c("100m", 
                               "400m", 
                               "1500m",
                               "5000m",
                               "10000m",
                               "Marathon")) +
  scale_shape_manual(name = "Sex",
                     values = c(21, 22)) +
  theme_classic()
