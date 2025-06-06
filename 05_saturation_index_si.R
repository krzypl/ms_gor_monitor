library(tidyverse)

wch_data_wide <- read_rds("data/water_chemistry_and_transparency.rds") %>% 
  filter(layer %in% c("epilimnion", "hypolimnion")) %>% 
  pivot_wider(id_cols = c(date, layer), names_from = variable, values_from = value) %>% 
  mutate(HCO3 = CaCO3*0.61)

#to obtain HCO3- from CaCO3, we multiply the values by 0.61
#SI is calculated to epilimnion and hypolimnion (probki dla metalimnionu byly pobierane z roznej glebokosci, dla uproszczenia usuwamy je)

mp_data_wide <- read_rds("data/multiprobe_data_filtered_long.rds") %>% 
  filter(depth_m %in% c("1", "30") & variable %in% c("temp", "ph", "sp_cond_us_cm")) %>% 
  mutate(layer = factor(ifelse(depth_m == "1", "epilimnion", "hypolimnion"))) %>% 
  pivot_wider(id_cols = c(date, layer), names_from = variable, values_from = value)



df_4_si <- wch_data_wide %>% 
  left_join(mp_data_wide) %>% 
  select(date, layer, Ca, HCO3, ph, sp_cond_us_cm, temp) %>% 
  rename(ec = sp_cond_us_cm)

#all EC values are below 592.6, so slope and intercept values are 0.000015231 and -0.000079191, respectively

si <- df_4_si %>% 
  mutate(slope = 0.000015231,
         intercept = -0.000079191,
         I_calc = slope*ec+intercept,
         A = 0.4883+8.074*0.0001*temp,
         B = 0.3241+1.6*0.0001*temp,
         rHCO3 = 4,
         log_gamma = (-A*sqrt(I_calc))/(1+rHCO3*B*sqrt(I_calc)),
         gamma_HCO3 = 10^log_gamma,
         HCO3_mol = HCO3/61000,
         HCO3_mol_ion = gamma_HCO3*HCO3_mol,
         log_K1_t = 14.8435-0.032786*(temp+273.15)-(3404.71/(temp+273.15)),
         K1_t = 10^log_K1_t,
         H_mol = 10^(-ph),
         H2CO3_mol = (H_mol*HCO3_mol_ion)/K1_t,
         H2CO3_mol_u = H2CO3_mol*1000000,
         log_K2_t = 6.498-0.02379*(temp+273.15)-(2902.39/(temp+273.15)),
         K2_t = 10^log_K2_t,
         CO3_mol_ion = K2_t*HCO3_mol_ion/H_mol,
         r_Ca = 6,
         log_gamma_Ca = (-A*4*sqrt(I_calc))/(1+r_Ca*B*sqrt(I_calc)),
         gamma_Ca = 10^log_gamma_Ca,
         Ca_mol = Ca/40000,
         Ca_mol_ion = Ca_mol*gamma_Ca,
         log_Kc_t = -171.9065-0.077993*(temp+273.15)+2839.319/(temp+273.15)+71.595*log10(temp+273.15),
         Kc_t = 10^log_Kc_t,
         omega = Ca_mol_ion*CO3_mol_ion/Kc_t,
         si = log10(omega)
         )

