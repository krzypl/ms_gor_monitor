library(tidyverse)

st_data <- read_rds("data/sediment_trap_data.rds")

st_data <- st_data %>% 
  mutate(start_date = as.Date(start_date),
         end_date = as.Date(end_date))


active_area_single_liner <- pi*(0.086/2)^2 #in squared meters

st_data <- st_data %>% 
  mutate(active_area = number_of_liners_collecting_sediment*active_area_single_liner,
         days_elapsed = as.numeric(end_date - start_date),
         tot_mar = dry_weight/active_area/days_elapsed,
         tn_mar = tn/100 * tot_mar,
         ts_mar = ts/100 * tot_mar,
         tic_mar = tic/100 * tot_mar,
         toc_mar = toc/100 * tot_mar,
         caco3_mar = (tic*8.33)/100 * tot_mar,
         mo_mar = (toc*1.96)/100 * tot_mar,
         sio2_mar = tot_mar - caco3_mar - mo_mar)

st_data_plot <- st_data %>% 
  pivot_longer(cols = c(tot_mar:sio2_mar), names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable, 
                            levels = c("tot_mar", "caco3_mar", "mo_mar", "sio2_mar", "tic_mar", "toc_mar", "tn_mar", "ts_mar"))) %>% 
  filter(variable %in% c("tot_mar", "caco3_mar", "mo_mar", "sio2_mar")) %>% 
  ggplot() +
  geom_rect(aes(xmin = start_date, xmax = end_date, ymin = 0, ymax = value)) +
  scale_x_date(date_labels = "%m.%Y",  
               date_breaks = "4 months",
               date_minor_breaks = "1 month",
               name = "Date") +
  facet_wrap(.~variable, scales = "free_y", ncol =1) +
  theme_minimal()
