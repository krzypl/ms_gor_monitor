library(tidyverse)

multiprobe_data <- read_rds("data/multiprobe_data_raw.rds")



mp_data_unique <- multiprobe_data %>% 
  mutate(depth_m = round(depth_m, digits = 0)) %>% 
  distinct(date, depth_m, .keep_all = TRUE)

diff(mp_data_unique$depth_m) #observation for 0 m is missing for 2024-06-28

mp_data_unique_for_plot <- mp_data_unique %>% 
  filter(depth_m %in% c(1, 5, 10, 15, 20, 30))

mp_data_long <- mp_data_unique %>% 
  pivot_longer(cols = c(odo_perc:tds_mgl, temp), names_to = "variable", values_to = "value") %>% 
  mutate(
    unit = case_when(
      variable == "odo_perc" ~ "%",
      variable == "odo_mgl" ~ "mg/l",
      variable == "orp_mv" ~ "mv",
      variable == "sp_cond_us_cm" ~ "µS/cm",
      variable == "tds_mgl" ~ "mg/L",
      variable == "depth_m" ~ "m",
      variable == "temp" ~ "°C",
      TRUE               ~ NA_character_
    )
  ) %>% 
  mutate(depth_m = as.factor(depth_m))

write_rds(mp_data_long, "data/multiprobe_data_filtered_long.rds")


mp_data_plot <- mp_data_long %>% 
  filter(depth_m %in% c(1, 5, 10, 15, 20, 30)) %>% 
  ggplot(aes(x = date, y = value, color = depth_m)) + 
  geom_line() +
  geom_point() +
  scale_x_date(date_labels = "%m.%Y",  
               date_breaks = "4 months",
               date_minor_breaks = "1 month",
               name = "Date") +
  facet_wrap(.~ variable, scales = "free_y", ncol = 1) +
  theme_minimal()
