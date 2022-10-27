source("dps_functions.R") #place dps_functions.r in the same folder as this script

leg <- 111751.5
armor <- 1092

base_impact <- 24
base_puncture <- 46
base_heat <- 50

base_cc <- 0.3
base_cd <- round(2.2*4095/32)*32/4095
base_ms <- 1
base_fire_rate <- 8.33

dmg_modifiers <- c(300/(300 + armor), 1.25*1.15*300/(300 + armor*0.85), 1.25*1.75*300/(300 + armor*0.25), 1.5*0.5*300/(300 + armor*1.5))

build <- function(rubedo = 1.87, heat_mod = 1.2, elec_mod = 1.2, parallax = 1, autotrigger = 0, cfocus = 0.6, hollowedbullets = 0.8, dual_rounds = 0, sabot = 0, cc = 0, ms = 0, cd = 0, heat = 0, elec = 0, dmg = 0, fr = 0, dtc = 0, neg_impact = 0, neg_punc = 0, bless = 0, eclipse_str = 0, adarza = 0, avenger = 0, eclipse_light = 1, volt_shield = 0, mirage = 1){
  
  modded_cc <- base_cc*(1 + parallax + cfocus + cc) + avenger*0.45 + adarza*0.6
  modded_cd <- base_cd*(1 + hollowedbullets + cfocus + cd)*ifelse(volt_shield == 0,1,2)
  modded_ms <- base_ms*(1 + dual_rounds + ms)
  modded_fr <- base_fire_rate*(1 + autotrigger + fr)
  
  modded_imp <- base_impact*(1 + rubedo + sabot + dmg)
  modded_punc <- base_puncture*(1 + rubedo + sabot + dmg)
  modded_heat <- base_heat*(1 + rubedo + sabot + dmg)
  modded_elec <- (modded_imp + modded_punc + modded_heat)*volt_shield*0.5
  modded_rad <- (modded_imp + modded_punc + modded_heat)*(1 + heat_mod + elec_mod + heat + elec) - modded_imp - modded_punc
  
  quantum <- (modded_imp + modded_punc + modded_heat)/16
  
  inflicted_imp <- round(base_impact*(1 + rubedo + sabot + dmg - neg_impact)/quantum,digits = 0)*quantum*(1 + eclipse_str*ifelse(mirage == 1, 2, 1.5)*eclipse_light)*(1 + dtc + bless*0.25)*dmg_modifiers[[1]]
  inflicted_punc <- round(base_puncture*(1 + rubedo + sabot + dmg - neg_punc)/quantum,digits = 0)*quantum*(1 + eclipse_str*ifelse(mirage == 1, 2, 1.5)*eclipse_light)*(1 + dtc + bless*0.25)*dmg_modifiers[[2]]
  inflicted_rad <- round(modded_rad/quantum,digits = 0)*quantum*(1 + eclipse_str*ifelse(mirage == 1, 2, 1.5)*eclipse_light)*(1 + dtc + bless*0.25)*dmg_modifiers[[3]]
  inflicted_elec <- round(modded_elec/quantum,digits = 0)*quantum*(1 + eclipse_str*ifelse(mirage == 1, 2, 1.5)*eclipse_light)*(1 + dtc + bless*0.25)*dmg_modifiers[[4]]
  inflicted_dmg <- c(inflicted_imp,inflicted_punc,inflicted_rad,inflicted_elec)
  
  return(data.frame(crit_chance = modded_cc, crit_dmg = modded_cd, multishot = modded_ms, bullet_dmg = sum(inflicted_dmg), fire_rate = modded_fr))
}

#by default mirage is used (mirage = 1),set to 0 if using subsumed eclipse
#bless, avenger and adarza are 0 or 1
#percentages are converted to decimals,eg. 133.2% is 1.332
#shield is from 0 to 6 (if shooting through volt's shield)

#example
(stats <- build(eclipse_str = 3, dual_rounds = 0.6,parallax = 0, autotrigger = 0.6, avenger = 1, cd = 0.49, ms = 0.4, volt_shield = 0))
sapply(dps(stats[[1]], stats[[2]], stats[[3]], stats[[4]], stats[[5]], 1), sum, simplify = F) #dmg per bullet fired
