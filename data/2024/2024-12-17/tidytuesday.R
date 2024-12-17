library(tidyverse)
library(tidytuesdayR)

# For the figure I use fonts from the following GH repo that must be installed
# locally
# https://github.com/jonathonf/solbera-dnd-fonts
# The colors used are based on the Homebrewery
# https://homebrewery.naturalcrit.com/

tuesdata <- tidytuesdayR::tt_load(2024, week = 51)

# the classes we want to collect information about - paladin and ranger are put
# last because they are half casters
classes <- c("bard", "cleric", "druid", "sorcerer","warlock", "wizard",
             "paladin","ranger")

# use mapping to get spells specific to each class in a list
spells_class <- map(classes, function(x) {tuesdata$spells |> filter(!!sym(x))}) 
names(spells_class) <- classes

# map again to get proportion of spells that require concentration by class
concentrate_class <- map(spells_class, function(x) {
  x |>
    group_by(level) |>
    summarize(prop_concentrate = mean(concentration))
}) |>
  # bind this into a tibble. The .id argument gets the list name as a variable
  bind_rows(.id = "class") |>
  # factorize class with nice labeling for later plotting
  mutate(class = factor(class,
                        levels = classes,
                        labels = str_to_title(classes)))

concentrate_class |>
  ggplot(aes(x = level, y = prop_concentrate))+
  geom_col(fill = "#9c2b1b")+
  scale_x_continuous(breaks = 0:9)+
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~class)+
  labs(title = "Share of spells requiring concentration",
       subtitle = "by level and class",
       caption = "Source: D&D Free Rules (2024)",
       y = NULL)+
  theme_bw()+
  theme(text = element_text(family = "ScalySansCaps"),  
        title = element_text(colour = "#58180D", 
                             family = "ScalySansCaps-Bold"),
        strip.text = element_text(colour = "#58180D", 
                                  family = "ScalySansCaps-Bold"),
        strip.background = element_rect(fill = "#e0e5c1"),
        panel.background = element_rect(fill = "#EEE5CE"),
        plot.background = element_rect(fill = "#FDF1DC"),
        panel.grid = element_blank())
