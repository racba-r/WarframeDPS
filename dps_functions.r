shield <- 156720

possible_shots <- function(cc, cd, dmg){
  shots <- round(c((1 + floor(cc)*(cd - 1))*dmg, (1 + ceiling(cc)*(cd - 1))*dmg))
  shots_probs <- c(ceiling(cc) - cc, cc - floor(cc))
  return(data.frame(shots, shots_probs))
}

multishot <- function(ms){
  number_of_shots <- c(floor(ms), ceiling(ms))
  ms_probs <- c(ceiling(ms) - ms, ms - floor(ms))
  return(data.frame(number_of_shots,ms_probs))
}

bullet <- function(cc, cd, ms, dmg, burst){
  shots <- possible_shots(cc, cd, dmg)
  multi_shot <- multishot(ms)
  if(ms %% 1 == 0){
    sample_size = ms*burst
  }else{
    sample_size <- sum(sample(multi_shot[[1]], size = burst, prob = multi_shot[[2]], replace = T))  
  }
  return(sample(shots[[1]], size = sample_size, prob = shots[[2]],replace = T))
}

dps <- function(cc, cd, ms, dmg, fr, burst){
  total_number_of_shots <- floor(fr)
  return(replicate(total_number_of_shots, bullet(cc, cd, ms, dmg, burst), simplify = F))
}

ttk <- function(cc, cd, ms, dmg, fr, burst, shield_value = shield){
  return(round(which(cumsum(sapply(dps(cc, cd, ms, dmg, fr, burst), sum)) >= shield_value)[1]/floor(fr), digits = 4))
} #if the weapon cant destroy the shield in one second it will return NA
