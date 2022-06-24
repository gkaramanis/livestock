# Idea from
# https://erdavis.com/2022/06/21/where-is-there-more-livestock-than-people/
  
library(tidyverse)
library(camcorder)
library(colorspace)

gg_record(dir = "livestock-temp", device = "png", width = 10, height = 10, units = "in", dpi = 320)

# Read in population
pop_raw <- readxl::read_xlsx(here::here("data/be0101_tabhel2020.xlsx"), skip = 4)

pop <- pop_raw %>% 
  janitor::clean_names() %>% 
  filter(nchar(kommun) == 4) %>% 
  select(1:3)

# Read in livestock
livestock_raw <- readxl::read_xlsx(here::here("data/JO0103F05_20220623-084945.xlsx"), col_names = c("kommun", "djurslag", "x2020"), skip = 3)

livestock <- livestock_raw %>% 
  fill(kommun) %>% 
  separate(kommun, into = c("kommun", "namn")) %>% 
  mutate(
    x2020 = as.numeric(x2020),
    x2020 = replace_na(x2020, 0)
  ) %>% 
  filter(!is.na(djurslag))

livestock_pop <- livestock %>% 
  left_join(pop) %>% 
  mutate(
    more_livestock = x2020 > folkmangd,
    ratio = x2020/folkmangd
    )

# Read in shapefile
shp <- sf::read_sf(here::here("data/2018_kommuner/alla_kommuner.shp")) %>%
  sf::st_simplify(dTolerance = 1000) %>% 
  janitor::clean_names() %>% 
  rename("kommun" = "kom")

shp_livestock_pop <- shp %>% 
  left_join(livestock_pop)

# Plot all livestock types
shp_livestock_pop %>% 
  ggplot() +
  geom_sf(aes(fill = more_livestock), color = NA) +
  scale_fill_manual(values = c("white", "black")) +
  facet_wrap(vars(djurslag), ncol = 5) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey97", color = NA)
  )
  

# Make livestock group
animal_groups <- livestock %>% 
  pivot_wider(names_from = djurslag, values_from = x2020) %>% 
  mutate(
    cattle = `kor för mjölkproduktion` + `kor för uppfödning av kalvar` + `kalvar, under 1 år` + `kvigor, tjurar och stutar`,
    pigs = `smågrisar, under 20 kg` + `slaktgrisar, 20 kg och däröver` + `galtar för avel` + `suggor för avel`,
    chicken = `slaktkycklingar` + `värpkycklingar` + `höns`,
    sheep = `baggar och tackor` + `lamm`
  ) %>% 
  pivot_longer(3:last_col(), names_to = "djurslag", values_to = "x2020") %>% 
  left_join(pop) %>% 
  mutate(
    more_livestock = x2020 > folkmangd,
    ratio = x2020/folkmangd
  )

shp_animal_groups <- shp %>% 
  left_join(animal_groups)

# All big groups
shp_animal_groups %>% 
  filter(djurslag %in% c("cattle", "pigs", "chicken", "sheep")) %>%
  ggplot() +
  geom_sf(aes(fill = more_livestock), color = NA) +
  scale_fill_manual(values = c("white", "black")) +
  facet_wrap(vars(djurslag), ncol = 5) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey97", color = NA)
  )

gg_resize_film(height = 10, width = 6)

# Individual groups
# Chicken
shp_animal_groups %>% 
  filter(djurslag == "chicken" & more_livestock) %>% 
  ggplot() +
  geom_sf(data = shp, fill = "white", size = 0.2, color = "grey60") +
  geom_sf(aes(fill = ratio), color = "grey20", size = 0.1) +
  geom_sf_text(data = . %>% filter(ratio == max(.$ratio)), aes(label = namn_kom), hjust = 0, nudge_x = 30000, family = "Outfit") +
  scale_fill_binned_sequential("OrYel", breaks = seq(0, 200, 20), limits = c(1, 200), guide = guide_colorsteps(title = "chicken to people ratio", title.position = "top", show.limits = TRUE)) +
  labs(
    title = "Municipalities with more chicken than people",
    subtitle = "Broilers, laying hens and chicken - June 2020",
    caption = "Sources: Jordbruksverket, SCB, Valmyndigheten · Graphic: Georgios Karamanis "
  ) +
  theme_void(base_family = "Outfit") +
  theme(
    legend.position = "top",
    legend.key.width = unit(3, "line"),
    legend.key.height = unit(1, "line"),
    plot.background = element_rect(fill = "grey95", color = NA),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(20, 0, 7, 0)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(0, 0, 15, 0)),
    plot.caption = element_text(hjust = 0.5, margin = margin(0, 0, 10, 0))
  )

# Cows/Cattle
shp_animal_groups %>% 
  filter(djurslag == "cattle" & more_livestock) %>% 
  ggplot() +
  geom_sf(data = shp, fill = "white", size = 0.2, color = "grey60") +
  geom_sf(aes(fill = ratio), color = "grey20", size = 0.1) +
  geom_sf_text(data = . %>% filter(ratio == max(.$ratio)), aes(label = namn_kom), hjust = 0, nudge_x = 30000, family = "Outfit") +
  scale_fill_binned_sequential("Purples 3", breaks = 1:4, limits = c(1, 4), guide = guide_colorsteps(title = "cattle to people ratio", title.position = "top", show.limits = TRUE)) +
  labs(
    title = "Municipalities with more cattle than people",
    subtitle = "Cows for milk production, cows for raising calves,\ncalves under 1 year, heifers, bulls and steers - June 2020",
    caption = "Sources: Jordbruksverket, SCB, Valmyndigheten · Graphic: Georgios Karamanis "
  ) +
  theme_void(base_family = "Outfit") +
  theme(
    legend.position = "top",
    legend.key.width = unit(3, "line"),
    legend.key.height = unit(1, "line"),
    plot.background = element_rect(fill = "grey95", color = NA),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(20, 0, 7, 0)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(0, 0, 15, 0)),
    plot.caption = element_text(hjust = 0.5, margin = margin(0, 0, 10, 0))
  )


# Pigs
shp_animal_groups %>% 
  filter(djurslag == "pigs" & more_livestock) %>% 
  ggplot() +
  geom_sf(data = shp, fill = "white", size = 0.2, color = "grey60") +
  geom_sf(aes(fill = ratio), color = "grey20", size = 0.1) +
  geom_sf_text(data = . %>% filter(ratio == max(.$ratio)), aes(label = namn_kom), hjust = 0, nudge_x = 30000, family = "Outfit") +
  scale_fill_binned_sequential("Reds 2", breaks = 1:4, limits = c(1, 4), guide = guide_colorsteps(title = "pigs to people ratio", title.position = "top", show.limits = TRUE)) +
  labs(
    title = "Municipalities with more pigs than people",
    subtitle = "Piglets under 20 kg, pigs for slaughter (20 kg and over),\n boars and sows for breeding - June 2020",
    caption = "Sources: Jordbruksverket, SCB, Valmyndigheten · Graphic: Georgios Karamanis "
  ) +
  theme_void(base_family = "Outfit") +
  theme(
    legend.position = "top",
    legend.key.width = unit(3, "line"),
    legend.key.height = unit(1, "line"),
    plot.background = element_rect(fill = "grey95", color = NA),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(20, 0, 7, 0)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(0, 0, 15, 0)),
    plot.caption = element_text(hjust = 0.5, margin = margin(0, 0, 10, 0))
  )


# Most common livestock by municipality
shp_animal_groups %>% 
  filter(djurslag %in% c("cattle", "pigs", "chicken", "sheep")) %>%
  group_by(namn_kom) %>% 
  slice_max(order_by = x2020, n = 1) %>% 
  ungroup() %>% 
  ggplot() +
  geom_sf(aes(fill = djurslag), size = 0.1, color = "grey90") +
  scale_fill_manual(values = c("#574D9C", "#E7B36A", "#914546", "darkolivegreen4")) +
  labs(
    title = "Most common livestock by municipality",
    subtitle = "June 2020",
    caption = "Sources: Jordbruksverket, SCB, Valmyndigheten · Graphic: Georgios Karamanis "
  ) +
  theme_void(base_family = "Outfit") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.key.width = unit(0.8, "line"),
    legend.key.height = unit(0.8, "line"),
    plot.background = element_rect(fill = "grey95", color = NA),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(20, 0, 7, 0)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(0, 0, 15, 0)),
    plot.caption = element_text(hjust = 0.5, margin = margin(0, 0, 10, 0))
  )


# Counts 
shp_animal_groups %>% 
  filter(djurslag %in% c("cattle", "pigs", "chicken", "sheep")) %>%
  add_count(kommun, more_livestock, sort = TRUE) %>% 
  filter(more_livestock) %>% View()

# Counts
livestock_pop %>% 
  add_count(kommun, more_livestock, sort = TRUE) %>% 
  filter(more_livestock) %>% View()
