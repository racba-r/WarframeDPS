source("dps_functions.R")

base_impact <- 32.25
base_puncture <- 5.375
base_slash <- 5.375

base_cc <- 0.21
base_cd <- round(2.3*4095/32)*32/4095
base_ms <- 1
base_fire_rate <- 2.56

dmg_modifiers <- c(1.5, 0.8, 1, 1.75, 1, 1, 1, 1) #i-p-s-mag-elec-corro-viral-heat

build <- function(ppg = 1.87, ptc = 1.1, hornet = 2.2, barrel = 1.2, lt = 0.6, anemic = 0, expel = 0.55, phc = 1.65, cascadia = 0, lich_bonus = 0.6, corro = 0, viral = 0, avenger = 0, adarza = 0, velocity = 1.2, vig_swap = 1.65, ability_str = 0, volt_shield = 0, bless = 0, eclipse_light = 1, cc = 0, cd = 0, ms = 0, dmg = 0, fr = 0, dtc = 0, tox = 0, elec = 0, cold = 0, neg_impact = 0, neg_punc = 0, neg_sl = 0){
  
  modded_cc <- base_cc*(1 + ppg + cascadia + cc) + avenger*0.45 + adarza*0.6
  modded_cd <- base_cd*(1 + ptc + cd)*ifelse(volt_shield == 0,1,2)
  modded_ms <- base_ms*(1 + barrel + lt + ms)
  modded_fr <- base_fire_rate*(1 + velocity + lt + anemic + fr)
  base_mag <- lich_bonus*(base_impact + base_puncture + base_slash)
  
  modded_imp <- base_impact*(1 + hornet - ifelse(anemic != 0, 0.15, 0) + dmg + vig_swap)
  modded_punc <- base_puncture*(1 + hornet - ifelse(anemic != 0, 0.15, 0) + dmg + vig_swap)
  modded_sl <- base_slash*(1 + hornet - ifelse(anemic != 0, 0.15, 0) + dmg + vig_swap)
  modded_mag <- base_mag*(1 + hornet - ifelse(anemic != 0, 0.15, 0) + dmg + vig_swap)
  modded_corro <- corro*(tox + elec)*(modded_imp + modded_punc + modded_sl + modded_mag)
  modded_viral <- viral*(tox + cold)*(modded_imp + modded_punc + modded_sl + modded_mag)
  modded_elec <- volt_shield*0.5*(modded_imp + modded_punc + modded_sl + modded_mag)
  modded_heat <- phc*(modded_imp + modded_punc + modded_sl + modded_mag)
  
  quantum <- (modded_imp + modded_punc + modded_sl + modded_mag)/16
  
  inflicted_imp <- round(base_impact*(1 + hornet - ifelse(anemic != 0, 0.15, 0) + dmg + vig_swap - neg_impact)/quantum,digits = 0)*quantum*(1 + ability_str*1.5*eclipse_light)*(1 + dtc + expel + bless*0.25)*dmg_modifiers[1]
  inflicted_punc <- round(base_puncture*(1 + hornet - ifelse(anemic != 0, 0.15, 0) + dmg + vig_swap - neg_punc)/quantum,digits = 0)*quantum*(1 + ability_str*1.5*eclipse_light)*(1 + dtc + expel + bless*0.25)*dmg_modifiers[2]
  inflicted_sl <- round(base_slash*(1 + hornet - ifelse(anemic != 0, 0.15, 0) + dmg + vig_swap - neg_sl)/quantum,digits = 0)*quantum*(1 + ability_str*1.5*eclipse_light)*(1 + dtc + expel + bless*0.25)*dmg_modifiers[3]
  inflicted_mag <- round(modded_mag/quantum,digits = 0)*quantum*(1 + ability_str*1.5*eclipse_light)*(1 + dtc + expel + bless*0.25)*dmg_modifiers[4]
  inflicted_elec <- round(modded_elec/quantum,digits = 0)*quantum*(1 + ability_str*1.5*eclipse_light)*(1 + dtc + expel + bless*0.25)*dmg_modifiers[5]
  inflicted_corro <- round(modded_corro/quantum,digits = 0)*quantum*(1 + ability_str*1.5*eclipse_light)*(1 + dtc + expel + bless*0.25)*dmg_modifiers[6]
  inflicted_viral <- round(modded_viral/quantum,digits = 0)*quantum*(1 + ability_str*1.5*eclipse_light)*(1 + dtc + expel + bless*0.25)*dmg_modifiers[7]
  inflicted_heat <- round(modded_heat/quantum,digits = 0)*quantum*(1 + ability_str*1.5*eclipse_light)*(1 + dtc + expel + bless*0.25)*dmg_modifiers[8]

  return_frame <- data.frame(cc = modded_cc, cd = modded_cd, ms = modded_ms, fr = modded_fr, impact = inflicted_imp, puncture = inflicted_punc, slash = inflicted_sl, mag = inflicted_mag)
  if(viral == 1) return_frame <- data.frame(return_frame, viral = inflicted_viral, heat = inflicted_heat)
  if(corro == 1) return_frame <- data.frame(return_frame, corro = inflicted_corro, heat = inflicted_heat)
  if(volt_shield != 0) return_frame <- data.frame(return_frame, elec = inflicted_elec)
  
  return(return_frame)
}

#bless, avenger and adarza are 0 or 1
#percentages are converted to decimals,eg. 133.2% is 1.332
#volt_shield is from 0 to 6 (if shooting through volt's shield)

#example

(stats <- build(ability_str = 3, viral = 1, tox = 0.93, cold = 0.86, ms = 0.83,avenger = 1,volt_shield = 1))

#stats[[1]] = cc, stats[[2]] = cd, stats[[3]] = ms, stats[[4]] = fr, stats[[5...]] = impact/puncture.... (pick what dmg type you want)
#ttk(stats[[1]],stats[[2]],stats[[3]],stats[[8]],stats[[4]],1) #ttk for mag not representative as ttk is calculated as (number of shots needed to kill)/fire_rate and kkraken has low fr

(damage_per_second <- dps(stats[[1]], stats[[2]], stats[[3]], stats[[8]], stats[[4]], 3)) #mag dps (pick dmg type you want from 5 onwards and replace the '8' in the function call)
sapply(damage_per_second, sum, simplify = F) #dmg per bullet fired
