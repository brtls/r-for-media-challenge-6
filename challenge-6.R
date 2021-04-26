library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(ggrepel)

# Stelle den mitgegebenen Plot nach. Alle Daten dafür habt Ihr ja vorliegen!

# load df with all palamentarians  in the German BT
# bundestag_2019 <- …

# load their Nebentätigkeiten
# nebeneinkuenfte <- …

nebeneinkuenfte <- read.csv("data-ROsix.csv", encoding = "UTF-8") %>% 
  janitor::clean_names()  %>%  
  mutate(
    name = str_split(name, ", ") %>% map_chr(~rev(.) %>% paste(collapse = " ")),
    nebentaetigkeiten = nebentatigkeiten == "ja"
  ) %>% 
  select(-nebentatigkeiten, -partei)

bundestag_2019 <- readRDS("bundestag_2019.rds")

bundestag_2019 <- bundestag_2019 %>% 
  left_join(nebeneinkuenfte)

partei_farben <- list(
  "CDU" = "black",
  "SPD" = "red", 
  "AfD" = "blue",
  "FDP" = "yellow",
  "Linke" = "violet",
  "Grüne" = "green", 
  "CSU" = "black",
  "fraktionslos" = "grey"
)

# disabling scientific notation
options(scipen = 999)

# plotting all the needed value with a dot plot
ggplot(data = bundestag_2019, aes(x = lebensdaten, y = land, size = mindest_einkunfte_in_euro, color = fraktion)) +
  geom_point(alpha = 0.5, position = "jitter") +
  scale_color_manual(values = partei_farben) +
  geom_label_repel(
    show.legend = FALSE,
    data = bundestag_2019 %>% 
      filter(mindest_einkunfte_in_euro > 450000),
    aes(label = name),
    color = "black",
    size = 3.5,
    fill = "white", 
  ) +
  theme_minimal() +
  labs(
    title = "Abgeordnete und deren Nebeneinkünfte",
    subtitle = "Wie viel Verdienen die Bundestagsabgeordneten neben ihrer Hauptberufung?",
    x = "Geburtsjahr",
    y = "Bundesland",
    size = "Mindestnebeneinkünfte",
    color = "Fraktion"
  )