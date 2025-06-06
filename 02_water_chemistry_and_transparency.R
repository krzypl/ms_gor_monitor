library(tidyverse)

wch_data <- read_rds("data/water_chemistry_and_transparency.rds")

wch_data_for_tsi <- wch_data #tsi from trophic state index

wch_data_for_tsi$layer[is.na(wch_data_for_tsi$layer)] <- "epilimnion"

wch_data_for_tsi <- wch_data_for_tsi %>% 
  mutate(layer = if_else(layer == "1 m", "epilimnion", layer),
         layer = factor(layer))

wch_data_tsi <- wch_data_for_tsi %>% 
  pivot_wider(id_cols = c(layer, date), names_from = variable, values_from = value) %>% 
  filter(layer == "epilimnion") %>% 
  mutate(tsi_tp = 10*(6 - log(48/(TP*1000))/0.693),
         tsi_chla = 10*(6-((2.04-0.68*log(chla))/0.693)),
         tsi_s = 10*(6-((log(secchi))/0.693)),
         tsi = (tsi_tp + tsi_chla + tsi_s)/3
  )


tsi <- wch_data_tsi %>% 
  select(layer, date, tsi) %>% 
  rename(value = tsi) %>% 
  mutate(unit = NA,
         variable = factor("TSI"))

wch_data <- wch_data %>% 
  add_row(tsi)


wch_data_plot <- ggplot(wch_data, aes(x = date, y = value, color = layer), alpha = 0.3) +
  geom_line() +
  geom_point() +
  scale_x_date(date_labels = "%m.%Y",  
               date_breaks = "8 months",
               date_minor_breaks = "1 month",
               name = "Date") +
  facet_wrap(.~variable, scales = "free_y", ncol = 2) +
  theme_minimal()

wch_data_plot_red <- wch_data %>% 
  filter(!(variable %in% c("K", "Na") & value > 30)) %>% #perhaps erronous values
  ggplot(aes(x = date, y = value, color = layer), alpha = 0.3) +
#  scale_color_brewer(palette = "Set1") +
  scale_color_manual(
    name = "Depth",
    values = c(
      "epilimnion" = "blue",
      "metalimnion" = "darkred",
      "hypolimnion" = "darkgreen",
      "1 m" = "magenta",
      "4 m" = "black",
      "7 m" = "orange",
      "10 m" = "lightblue",
      "NA" = "grey"
    )
  ) +
  geom_line() +
  geom_point() +
  scale_x_date(date_labels = "%m.%Y",  
               date_breaks = "8 months",
               date_minor_breaks = "1 month",
               name = "Date") +
  facet_wrap(.~variable, scales = "free_y", ncol = 2) +
  theme_minimal()