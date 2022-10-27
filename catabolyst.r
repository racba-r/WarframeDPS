source("dps_functions.R")

base_corrosive <- 53
base_cc <- 0.11
base_cd <- round(2.9*4095/32)*32/4095
base_ms <- 1
base_fire_rate <- 12

build <- function(ppg = 1.87, ptc = 1.1, hornet = 2.2, barrel = 1.2, lt = 0.6, anemic = 0, expel = 0.55, phc = 1.65, cascadia = 0, avenger = 0, adarza = 0, velocity = 1.2, vig_swap = 1.65, ability_str = 0, volt_shield = 0, bless = 0, eclipse_light = 1, cc = 0, cd = 0, ms = 0, dmg = 0, fr = 0, dtc = 0, tox = 0, cold = 0){
  
  modded_cc <- base_cc*(1 + ppg + cascadia + cc) + avenger*0.45 + adarza*0.6
  modded_cd <- base_cd*(1 + ptc + cd)*ifelse(volt_shield == 0,1,2)
  modded_ms <- base_ms*(1 + barrel + lt + ms)
  modded_fr <- base_fire_rate*(1 + velocity + lt + anemic + fr)
  
  modded_corro <- base_corrosive*(1 + hornet - ifelse(anemic != 0, 0.15, 0) + dmg + vig_swap)
  modded_viral <- modded_corro*(tox + cold)
  modded_heat <- modded_corro*phc
  modded_elec <- modded_corro*volt_shield*0.5
  
  quantum <- modded_corro/16
  
  inflicted_corro <- round(modded_corro/quantum)*quantum*(1 + expel + dtc + bless*0.25)*(1 + ability_str*1.5*eclipse_light)
  inflicted_viral <- round(modded_viral/quantum)*quantum*(1 + expel + dtc + bless*0.25)*(1 + ability_str*1.5*eclipse_light)
  inflicted_heat <- round(modded_heat/quantum)*quantum*(1 + expel + dtc + bless*0.25)*(1 + ability_str*1.5*eclipse_light)
  inflicted_elec <- round(modded_elec/quantum)*quantum*(1 + expel + dtc + bless*0.25)*(1 + ability_str*1.5*eclipse_light)
  
  return(data.frame(cc = modded_cc, cd = modded_cd, ms = modded_ms, fr = modded_fr, corro = inflicted_corro, viral = inflicted_viral, heat = inflicted_heat, elec = inflicted_elec))
}

#bless, avenger and adarza are 0 or 1
#percentages are converted to decimals,eg. 133.2% is 1.332
#volt_shield is from 0 to 6 (if shooting through volt's shield)

#example
(stats <- build(volt_shield = 1,ability_str = 3.09, cold = 0.9, tox = 0.9, ms = 1.2, avenger = 1))
ttk(stats[[1]], stats[[2]], stats[[3]], stats[[6]], stats[[4]], 1) #viral ttk (pick dmg type you want from 5 to 8 and replace the '6' in the function call)
