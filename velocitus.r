leg <- 111751.5
armor <- 1092

base_cc <- 0.3
base_cd <- round(2*4095/32)*32/4095
base_ms <- 1

base_i <- 150
base_p <- 150
base_s <- 150
base_mag <- 150

dmg_modifiers <- c(300/(300 + armor),1.25*1.15*300/(300 + armor*0.85),0.75*0.5*300/(300 + armor*1.5),0.5*300/(300 + armor*1.5),1.25*1.75*300/(300 + armor*0.25),1.5*0.5*300/(300 + armor*1.5))

build <- function(parallax = 1, rubedo = 1.87, critfocus = 0.6, hollowed_bullets = 0.8, dual_rounds = 0, autotrigger = 0.6, heat_mod = 1.2, elec_mod = 1.2, magma = 0, sabot = 0, intrinsics = 0.2, shield = 1, bless = 0, neg_i = 0, neg_p = 0, neg_s = 0,cc = 0, cd = 0, ms = 0, dmg = 0, dtc = 0, heat = 0,elec = 0, fr = 0){
    
    modded_cc <- base_cc*(1 + parallax + critfocus + cc)
    modded_cd <- ifelse(shield >= 1, 2, 1)*base_cd*(1 + hollowed_bullets + critfocus + cd)
    modded_ms <- base_ms*(1 + dual_rounds + ms)
    
    modded_i <- base_i*(1 + rubedo + sabot + intrinsics + dmg)
    modded_p <- base_p*(1 + rubedo + sabot + intrinsics + dmg)
    modded_s <- base_s*(1 + rubedo + sabot + intrinsics + dmg)
    modded_mag <- base_mag*(1 + rubedo + sabot + intrinsics + dmg)
    
    quantum <- (modded_i + modded_p + modded_s + modded_mag)/16
    
    inflicted_i <- round(base_i*(1 + rubedo + sabot + intrinsics + dmg - neg_i)/quantum)*quantum*(1 + dtc + bless*0.25)*dmg_modifiers[1]
    inflicted_p <- round(base_p*(1 + rubedo + sabot + intrinsics + dmg - neg_p)/quantum)*quantum*(1 + dtc + bless*0.25)*dmg_modifiers[2]
    inflicted_s <- round(base_s*(1 + rubedo + sabot + intrinsics + dmg - neg_s)/quantum)*quantum*(1 + dtc + bless*0.25)*dmg_modifiers[3]
    inflicted_mag <- round(modded_mag/quantum)*quantum*(1 + dtc + bless*0.25)*dmg_modifiers[4]
    inflicted_rad <- round((modded_i + modded_p + modded_s + modded_mag)*(heat_mod + elec_mod + magma + heat + elec)/quantum)*quantum*(1 + dtc + bless*0.25)*dmg_modifiers[5]
    inflicted_elec <- round(shield*0.5*(modded_i + modded_p + modded_s + modded_mag)/quantum)*quantum*(1 + dtc + bless*0.25)*dmg_modifiers[6]
    
    return(round(floor(modded_ms)*(1 + floor(modded_cc)*(modded_cd - 1))*(sum(c(inflicted_i,inflicted_p,inflicted_s,inflicted_mag,inflicted_rad,inflicted_elec)))))
}
#bless is 0 or 1
#percentages are converted to decimals,eg. 133.2% is 1.332 

leg - build(cc = 1.11, dtc = 0.476, heat = 1.265) #example