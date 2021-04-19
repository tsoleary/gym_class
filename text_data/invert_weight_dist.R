# ------------------------------------------------------------------------------
# Category edge list
# April 19, 2021
# TS O'Leary
# ------------------------------------------------------------------------------


# Load packages
require(tidyverse)

# Load data
df <- read_csv(here::here("text_data/edgelist.csv"))

# Create inverse of weight
df <- df %>%
  filter(Weight > 0) %>%
  mutate(Weight = 1/Weight)


# Write new .csv
write_tsv(df, here::here("text_data/edgelist_inverse.csv"))


# Import weighted degree and modularity data from gephi -----
dat <- read_csv(here::here("text_data/net_wt_degree.csv"))

dat %>%
  ggplot() +
  geom_histogram(aes(x = `Weighted Degree`, 
                     fill = as.factor(modularity_class)),
                     color = "grey50", 
                 bins = 10) +
  scale_fill_manual(name = "Module Class",
                    values = c("#848EDF",
                               "#729A35",
                               "#D373BE",
                               "#E1665C",
                               "#CE7E36")) +
  ylab("Count") +
  theme_classic()


x <- dat %>%
  select(Id, `Weighted Degree`, modularity_class) %>%
  arrange(desc(`Weighted Degree`))
