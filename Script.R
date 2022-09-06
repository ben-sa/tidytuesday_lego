# Load libraries
library(tidyverse)
library(scales)
library(ggthemes)

# Load data
inventories <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
inventory_sets <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_sets.csv.gz')
sets <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')

# Join data
data <- left_join(inventories, inventory_sets, by = "set_num") |>
  left_join(sets, by = "set_num")

# Remove rows with missing year, reduce columns
data <- data %>% 
  filter(!(is.na(year))) %>% 
  select(-inventory_id, -quantity, -img_url)

# Show number of sets and parts over time
parts_time <- data %>% 
  group_by(year) %>% 
  summarize(sets = n(), parts = sum(num_parts)/sets) %>%
  filter(year != 2022) %>% 
  pivot_longer(sets:parts, names_to = "measure", values_to = "number") %>% 
  ggplot(aes(x = year, y = number, group = measure, color = measure)) +
  geom_line(size = 1.4) +
  labs(title = "Lego since the 1950's",
       subtitle = "Number of sets published increasing rapidly, while average parts per set growig moderately",
       y = "Number of sets / parts", x = "",
       caption = "Source: rebrickable.com") +
  scale_y_continuous(label = label_number()) +
  theme_solarized() + 
  theme(plot.title = element_text(size=17, face = "bold"),
        axis.ticks = element_blank(), axis.line = element_blank(),
        legend.key = element_rect(fill = NA, color = NA),
        legend.text = element_text(size = 12),
        legend.title = element_blank()) +
  scale_x_continuous(expand = c(0,0), breaks = c(1950, 1960, 1970,
                                                 1980, 1990, 2000,
                                                 2010, 2020))

parts_time
