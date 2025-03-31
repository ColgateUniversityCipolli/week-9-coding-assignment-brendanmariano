library(tidyverse)
###################
# Data Cleaning
###################
#Continuous data
dat = read_csv("agacis.csv") |>
  select(-Annual) |>
  pivot_longer(cols = c("Jan", "Feb", "Mar",
               "Apr", "May", "Jun",
               "Jul", "Aug", "Sep",
               "Oct", "Nov", "Dec"),
               names_to = "Month",
               values_to = "Precipitation") |>
  mutate(Precipitation = case_when(Precipitation == "M" ~ NA_character_,
            TRUE ~ Precipitation))  |>
  mutate(Precipitation = as.numeric(Precipitation))
  
view(dat)
mle.gamma = function(data, par, neg = F) {
  alpha = par[1]
  beta =par[2]
  
  dist = dgamma(x = data, shape = alpha, rate = beta)
  log.lik = sum(log(dist))
  
  return(ifelse(neg, -log.lik, log.lik))
}

vals = optim(fn = mle.gamma, 
      data = dat$Precipitation, 
      par = c(1,1),
      neg = T)
vals