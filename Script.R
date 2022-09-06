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
       caption = "Source: data provided by rebrickable.com") +
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

# Analyze set names
library(tidytext)

data_tidy <- data %>%
  select(set_num, year, name) %>% 
  unnest_tokens(word, name) %>% 
  anti_join(get_stopwords())

# Show top words in graph
data_tidy %>% 
  filter(str_count(word) > 2,
         word != "set", word != "pack",
         word != "bricks", word != "lego",
         word != "brick", word != "collection",
         word != "basic", word != "box") %>%
  tabyl(word) %>% arrange(desc(n)) %>%
  head(25) %>% 
  adorn_pct_formatting() %>% 
  ggplot(aes(x = n, y = reorder(word, n), label = percent)) +
  geom_bar(stat = "identity", fill = "cadetblue4") +
  theme_solarized() +
  theme(plot.title = element_text(size=17, face = "bold"),
        plot.subtitle = element_text(size=10),
       axis.ticks = element_blank(), axis.line = element_blank(),
       legend.key = element_rect(fill = NA, color = NA),
       legend.text = element_text(size = 12),
       legend.title = element_blank()) +
  scale_x_continuous(expand = c(0,0), label = label_number(),
                     limits = c(0, 750)) +
  labs(title = "Most frequently used words in LEGO set titles",
       subtitle = "Most popular titles include Emergency Vehicles, Action figures and cities",
       x = "Number of occurences (% of all words used)",
       y = "",
       caption = "Source: data provided by rebrickable.com") +
  geom_text(hjust = 1.3, size = 3, color = "white")

# Create Word cloud
library(wordcloud)
data_tidy %>% 
  select(word) %>% 
  count(word) %>%
  mutate(stem = wordStem(word)) %>% 
  filter(str_count(word) > 2,
         word != "set", word != "pack",
         word != "bricks", word != "lego",
         word != "brick") %>%
  with(wordcloud(stem, n, max.words = 150))
