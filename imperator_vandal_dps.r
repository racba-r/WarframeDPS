source("dps_functions.R") #place dps_functions.r in the same folder as this script

leg <- 111751.5
armor <- 1092

base_i <- 20
base_p <- 17.5
base_s <- 12.5

base_cc <- 0.28
base_cd <- round(2.4*4095/32)*32/4095
base_ms <- 1
base_fire_rate <- 25

dmg_modifiers <- c(300/(300 + armor), 1.25*1.15*300/(300 + armor*0.85), 0.75*0.5*300/(300 + armor*1.5), 1.25*1.75*300/(300 + armor*0.25), 1.5*0.5*300/(300 + armor*1.5))

build <- function(rubedo = 1.87, heat_mod = 1.2, elec_mod = 1.2, parallax = 1, autotrigger = 0, critfocus = 0.6, hollowed_bullets = 0.8, dual_rounds = 0, sabot = 0, magma = 0, cc = 0, ms = 0, cd = 0, heat = 0, elec = 0, dmg = 0, fr = 0, dtc = 0, neg_i = 0, neg_p = 0, neg_s = 0, bless = 0, eclipse_str = 0, adarza = 0, avenger = 0, eclipse_light = 1, shield = 0, mirage = 1){
  
  modded_cc <- base_cc*(1 + parallax + critfocus + cc) + avenger*0.45 + adarza*0.6
  modded_cd <- base_cd*(1 + hollowed_bullets + critfocus + cd)*ifelse(shield == 0,1,2)
  modded_ms <- base_ms*(1 + dual_rounds + ms)
  modded_fr <- base_fire_rate*(1 + autotrigger + fr)
  
  modded_i <- base_i*(1 + rubedo + sabot + dmg) 
  modded_p <- base_p*(1 + rubedo + sabot + dmg)
  modded_s <- base_s*(1 + rubedo + sabot + dmg)
  
  quantum <- (modded_i + modded_p + modded_s)/16
  
  inflicted_i <- round(base_i*(1 + rubedo + sabot + dmg - neg_i)/quantum,digits = 0)*quantum*(1 + eclipse_str*ifelse(mirage == 1, 2, 1.5)*eclipse_light)*(1 + dtc + bless*0.25)*dmg_modifiers[1]
  inflicted_p <- round(base_p*(1 + rubedo + sabot + dmg - neg_p)/quantum,digits = 0)*quantum*(1 + eclipse_str*ifelse(mirage == 1, 2, 1.5)*eclipse_light)*(1 + dtc + bless*0.25)*dmg_modifiers[2]
  inflicted_s <- round(base_s*(1 + rubedo + sabot + dmg - neg_s)/quantum,digits = 0)*quantum*(1 + eclipse_str*ifelse(mirage == 1, 2, 1.5)*eclipse_light)*(1 + dtc + bless*0.25)*dmg_modifiers[3]
  inflicted_rad <- round((modded_i + modded_p + modded_s)*(heat_mod + elec_mod + magma + heat + elec)/quantum,digits = 0)*quantum*(1 + eclipse_str*ifelse(mirage == 1, 2, 1.5)*eclipse_light)*(1 + dtc + bless*0.25)*dmg_modifiers[4]
  inflicted_elec <- round((modded_i + modded_p + modded_s)*shield*0.5/quantum,digits = 0)*quantum*(1 + eclipse_str*ifelse(mirage == 1, 2, 1.5)*eclipse_light)*(1 + dtc + bless*0.25)*dmg_modifiers[5]
  
  inflicted_dmg <- sum(c(inflicted_i, inflicted_p, inflicted_s, inflicted_rad, inflicted_elec))
  
  return(ttk(modded_cc, modded_cd, modded_ms, inflicted_dmg, modded_fr, 1, leg))
}

#by default mirage is used (mirage = 1),set to 0 if using subsumed eclipse
#bless, avenger and adarza are 0 or 1
#percentages are converted to decimals,eg. 133.2% is 1.332
#shield is from 0 to 6 (if shooting through volt's shield)

build(eclipse_str = 3.5, cd = 0.7, ms = 0.53, fr = 0.54, dual_rounds = 0.6, avenger = 1, shield = 1) #example
