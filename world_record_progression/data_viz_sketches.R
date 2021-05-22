# ------------------------------------------------------------------------------
# World Record Progression -- Data Viz
# April 30, 2021
# TS O'Leary
# ------------------------------------------------------------------------------

# Load Libraries
require(tidyverse)
require(plotly)
require(gganimate)
require(ggimage)

# Load data
dat <- read_csv(here::here("world_record_progression/world_record_progression_data.csv"), 
                col_types = cols(Time = "c"))
  

# Normalized data 
p <- dat %>%
  ggplot() +
  geom_point(aes(x = Date, 
                 y = time_norm, 
                 color = event, 
                 shape = sex)) +
  theme_classic()

p

ggplotly(p)

# Marathon data 
p <- dat %>%
  filter(event == "marathon") %>%
  ggplot() +
  geom_point(aes(x = Date, 
                 y = time_h, 
                 color = sex)) +
  labs(title = "Marathon",
       y = "Hours") +
  theme_classic() 

p

ggplotly(p)

# 10k
dat %>%
  filter(event == "10000") %>%
  ggplot() +
  geom_point(aes(x = Date, 
                 y = time_m, 
                 color = sex)) +
  labs(title = "10k",
       y = "Minutes") +
  theme_classic() 

p

ggplotly(p)

# 5k
dat %>%
  filter(event == "5000") %>%
  ggplot() +
  geom_point(aes(x = Date, 
                 y = time_m, 
                 color = sex)) +
  labs(title = "5k",
       y = "Minutes") +
  theme_classic() 

p

ggplotly(p)

# 1500
dat %>%
  filter(event == "1500") %>%
  ggplot() +
  geom_point(aes(x = Date, 
                 y = time_m, 
                 color = sex)) +
  labs(title = "1500 m",
       y = "Minutes") +
  theme_classic() 

p

ggplotly(p)

# 400
dat %>%
  filter(event == "400") %>%
  ggplot() +
  geom_point(aes(x = Date, 
                 y = time_s, 
                 color = sex)) +
  labs(title = "400 m",
       y = "Minutes") +
  theme_classic() +
  ggplotly

p

ggplotly(p)

# 100
dat %>%
  filter(event == "100") %>%
  ggplot() +
  geom_point(aes(x = Date, 
                 y = time_s, 
                 color = sex)) +
  labs(title = "100 m",
       y = "Seconds") +
  theme_classic() 

p

ggplotly(p)


# Animated plots ----

dat %>%
  filter(event == "marathon") %>%
  ggplot(aes(x = Date, 
             y = time_h,
             color = sex)) +
  geom_line() +
  geom_point() +
  labs(title = "Marathon world record progression",
       y = "Hours") +
  theme_classic(base_size = 20) +
  transition_reveal(Date)


# Racing visualization -----

x <- read_csv("~/Downloads/running_records_plus.csv") %>%
  select(-X1) %>%
  mutate(year = as.numeric(format(Date, format = "%Y"))) %>%
  mutate(decade = year - year %% 10) %>%
  group_by(event, sex, decade) %>%
  mutate(decade_best = time_s == min(time_s))

write_csv(x, here::here("world_record_progression/record_progression_data_decade.csv"))

x <- dat %>%
  mutate(dist = case_when(event == "marathon" ~ "42195", 
                          event != "marathon" ~ event)) %>%
  mutate(dist = as.numeric(dist)) %>%
  mutate(speed = dist/time_s) %>%
  filter(event == "marathon") %>%
  mutate(year = as.numeric(format(Date, format = "%Y"))) %>%
  mutate(decade = year - year %% 10) %>%
  filter(decade %in% c(1960, 1980, 2010)) %>%
  group_by(decade, sex) %>%
  top_n(1, wt = speed) %>%
  arrange(sex, speed)

x %>%
  ggplot(aes(x = speed, 
           y = 0.1)) +
  annotate("segment", 
           x = 0, xend = max(x$speed), y = 0, yend = 0, 
           size = 2, 
           color = "#555555") +
  annotate("segment", 
           x = 0, xend = 0, y = -0.1, yend = 0.1, 
           size = 2, 
           color = "#555555") +
  annotate("segment", 
           x = max(x$speed), xend = max(x$speed), y = -0.1, yend = 0.1, 
           size = 2, 
           color = "#555555") +
  geom_image(aes(image = here::here("world_record_progression/runner_image.png")), 
                 color = c("#ecf9f2", "#79d2a6", "#339966",
                           "#8b008b", "#ffb3ff", "#ff33ff"), 
             alpha = 0.3,
             size = 0.1) +
  scale_y_continuous(limits = c(-0.5, 0.5)) +
  theme(panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(), 
        legend.position = "none")

# Ratio decade ----
df <- read_csv(here::here("world_record_progression/record_progression_data_decade.csv"))

x <- df %>%
  group_by(event, sex, decade) %>%
  slice_min(time_s, n = 1) %>%
  summarise(time_s = mean(time_s)) %>%
  pivot_wider(names_from = sex, values_from = time_s)

write_csv(x, here::here("world_record_progression/ratio.csv"))




x <- read_csv(here::here("world_record_progression/ratio.csv"))

y <- x %>%
  mutate(w_m_ratio = w/m) %>%
  filter(decade >= 1960) 

y$event <- factor(y$event, levels = c("100", 
                                      "400", 
                                      "1500",
                                      "5000",
                                      "10000",
                                      "marathon"))

p <- y %>%
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
                      breaks = c(0.75, 1, 1.25, 1.5), 
                      labels = c(0.75, 1, 1.25, 1.5), 
                      limits = c(0.75, 1.5)) + 
   scale_x_continuous(name = "Decade", 
                      breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) + 
   geom_hline(yintercept = 1, linetype = "dashed", color = "grey20") +
   theme_classic()
p
ggplotly(p) 
