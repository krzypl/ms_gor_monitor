library(tidyverse)

#data preparation -----

fito_counts <- read_rds("data/fito_counts.rds")

fito_biomass <- read_rds("data/fito_biomass.rds")

fito_classification <- read_rds("data/fito_classification.rds")

fito_spe <- fito_counts %>% 
  select(!date & !layer)

fito_spe_rs <- rowSums(fito_spe)

fito_perc <- as_tibble((fito_spe/fito_spe_rs)*100) %>% 
  mutate(date = fito_counts$date,
         layer = fito_counts$layer)

fito_counts_long <- fito_counts %>% 
  pivot_longer(cols = -c("date", "layer"), names_to = "species", values_to = "counts") %>% 
  mutate(species = factor(species)) %>% 
  left_join(fito_classification)

fito_perc_long <- fito_perc %>% 
  pivot_longer(cols = -c("date", "layer"), names_to = "species", values_to = "rel_abund") %>% 
  mutate(species = factor(species)) %>% 
  left_join(fito_classification)

fito_biomass_long <- fito_biomass %>% 
  pivot_longer(cols = -c("date", "layer"), names_to = "species", values_to = "biomass") %>% 
  mutate(species = factor(species)) %>% 
  left_join(fito_classification)

fito_long <- fito_counts_long %>% 
  left_join(fito_perc_long) %>% 
  left_join(fito_biomass_long)

#plots ------

fito_groups_rel_abund_plot <- fito_long %>% 
  group_by(date, layer, group) %>% 
  summarise(rel_abund = sum(rel_abund)) %>% 
  ggplot() +
  geom_col(aes(x = date, y = rel_abund, fill = layer), position = "dodge") +
  scale_x_date(date_labels = "%m.%Y",  
               date_breaks = "4 months",
               date_minor_breaks = "1 month",
               name = "Date") +
  facet_wrap(.~ group, scales = "fixed", ncol = 1) +
  theme_minimal()

fito_groups_biomass_plot <- fito_long %>% 
  group_by(date, layer, group) %>% 
  summarise(biomass = sum(biomass)) %>% 
  ggplot() +
  geom_col(aes(x = date, y = biomass, fill = layer), position = "dodge") +
  scale_x_date(date_labels = "%m.%Y",  
               date_breaks = "4 months",
               date_minor_breaks = "1 month",
               name = "Date") +
  facet_wrap(.~ group, scales = "fixed", ncol = 1) +
  theme_minimal()

dominant_fito <- fito_long %>% 
  group_by(date, layer, species) %>% 
  summarise(max_rel_abund = max(rel_abund)) %>% 
  ungroup() %>% 
  filter(max_rel_abund > 35) %>% 
  distinct(species) %>% 
  pull()

label_species <- fito_long %>%
  filter(species %in% dominant_fito) %>% 
  group_by(species) %>%
  summarise(
    date = min(date),          
    value = max(rel_abund)   
  )

fito_dominants_plot <- fito_long %>% 
  filter(species %in% dominant_fito) %>%
  ggplot() +
  geom_col(aes(x = date, y = rel_abund, fill = layer), position = "dodge") +
  scale_x_date(date_labels = "%m.%Y",  
               date_breaks = "4 months",
               date_minor_breaks = "1 month",
               name = "Date") +
  geom_text(data = label_species, 
            aes(x = date, y = value, label = species), hjust = 0, 
            size = 2) +
  facet_wrap(.~ species, scales = "fixed", ncol = 1) +
  theme_minimal() +
  theme(
    strip.text = element_blank(),
    axis.text.y = element_text(),
    axis.title.y = element_blank()) +
  labs(
    x = "Date",
    fill = "layer"
  )

fito_totals_plot <- fito_long %>% 
  group_by(date, layer) %>% 
  summarise(total_abundance = sum(counts),
            total_biomass = sum(biomass)) %>%
  pivot_longer(cols = c("total_abundance", "total_biomass"),
               names_to = "parameter",
               values_to = "value") %>% 
  ggplot(aes(x = date, y = value, color = layer)) +
  geom_line() +
  geom_point() +
  scale_x_date(date_labels = "%m.%Y",  
               date_breaks = "4 months",
               date_minor_breaks = "1 month",
               name = "Date") +
  facet_wrap(.~parameter, scales = "free_y", ncol = 1) +
  theme_minimal()
